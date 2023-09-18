use std::sync::atomic::Ordering::Relaxed;

use rug::{Float, Complete};

use crate::{
    runner::{CastError, FLOAT_PRECISION},
    span::SpannedValue,
};

#[derive(Debug, Clone)]
pub enum ValueMagnitude {
    Int(rug::Integer),
    Float(rug::Float),
    Constant(rug::float::Constant),
}

impl TryFrom<SpannedValue<ValueMagnitude>> for rug::Integer {
    type Error = CastError;

    fn try_from(value: SpannedValue<ValueMagnitude>) -> Result<Self, Self::Error> {
        match value.value {
            ValueMagnitude::Int(i) => Ok(i),
            v => Err(CastError {
                from: v.ty(),
                to: "int",
                location: (value.start..value.end).into(),
                src: value.source,
            }),
        }
    }
}

impl TryFrom<SpannedValue<ValueMagnitude>> for usize {
    type Error = CastError;

    fn try_from(value: SpannedValue<ValueMagnitude>) -> Result<Self, Self::Error> {
        match value.value {
            ValueMagnitude::Int(i) if i.to_usize().is_some() => Ok(i.to_usize().unwrap()),
            v => Err(CastError {
                from: v.ty(),
                to: "usize",
                location: (value.start..value.end).into(),
                src: value.source,
            }),
        }
    }
}

impl ValueMagnitude {
    pub fn as_string(&self, rounding: Option<usize>) -> String {
        match self {
            ValueMagnitude::Int(n) => n.to_string(),
            ValueMagnitude::Float(f) => match rounding {
                None => f.to_string(),
                Some(p) => format!("{f:.*}", p),
            },
            ValueMagnitude::Constant(c) => {
                let f = Float::with_val(FLOAT_PRECISION.load(Relaxed), c);
                match rounding {
                    None => f.to_string(),
                    Some(p) => format!("{f:.*}", p),
                }
            }
        }
    }

    pub fn ty(&self) -> &'static str {
        match self {
            ValueMagnitude::Int(_) => "int",
            ValueMagnitude::Float(_) => "float",
            ValueMagnitude::Constant(_) => "float",
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            ValueMagnitude::Int(i) => i.is_zero(),
            ValueMagnitude::Float(f) => f.is_zero(),
            ValueMagnitude::Constant(_) => false,
        }
    }

    pub fn into_float(self) -> rug::Float {
        let precision = FLOAT_PRECISION.load(Relaxed);
        match self {
            ValueMagnitude::Int(i) => Float::with_val(precision, i),
            ValueMagnitude::Float(f) => f,
            ValueMagnitude::Constant(c) => Float::with_val(precision, c),
        }
    }

    pub fn into_int(self) -> rug::Integer {
        match self {
            ValueMagnitude::Float(f) => f.to_integer().unwrap_or(rug::Integer::ZERO),
            ValueMagnitude::Int(i) => i,
            ValueMagnitude::Constant(c) => Float::with_val(53, c)
                .to_integer()
                .expect("constants should not be NaN"),
        }
    }
}

macro_rules! impl_op {
    ($op:ident, $fn:ident, $symb:tt) => {
        impl std::ops::$op for ValueMagnitude {
            type Output = Self;

            fn $fn(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) => ValueMagnitude::Int(a $symb b),
                    (ValueMagnitude::Int(a), ValueMagnitude::Float(b)) => ValueMagnitude::Float(a $symb b),
                    (ValueMagnitude::Float(a), ValueMagnitude::Int(b)) => ValueMagnitude::Float(a $symb b),
                    (ValueMagnitude::Float(a), ValueMagnitude::Float(b)) => ValueMagnitude::Float(a $symb b),
                    (ValueMagnitude::Int(a), ValueMagnitude::Constant(c)) => {
                        ValueMagnitude::Float(a $symb Float::with_val(FLOAT_PRECISION.load(Relaxed), c))
                    }
                    (ValueMagnitude::Float(f), ValueMagnitude::Constant(c)) => {
                        ValueMagnitude::Float(f $symb Float::with_val(FLOAT_PRECISION.load(Relaxed), c))
                    }
                    (ValueMagnitude::Constant(c), ValueMagnitude::Int(i)) => {
                        ValueMagnitude::Float(Float::with_val(FLOAT_PRECISION.load(Relaxed), c) $symb i)
                    }
                    (ValueMagnitude::Constant(c), ValueMagnitude::Float(f)) => {
                        ValueMagnitude::Float(Float::with_val(FLOAT_PRECISION.load(Relaxed), c) $symb f)
                    }
                    (ValueMagnitude::Constant(c1), ValueMagnitude::Constant(c2)) => ValueMagnitude::Float(
                        Float::with_val(FLOAT_PRECISION.load(Relaxed), c1)
                            $symb Float::with_val(FLOAT_PRECISION.load(Relaxed), c2),
                    ),
                }
            }
        }
    };
}

impl_op!(Mul, mul, *);
impl_op!(Add, add, +);
impl_op!(Sub, sub, -);

impl std::ops::Div for ValueMagnitude {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) if (&a % &b).complete().is_zero() => {
                ValueMagnitude::Int(a / b)
            }
            (lhs, rhs) => ValueMagnitude::Float(lhs.into_float() / rhs.into_float()),
        }
    }
}

impl std::ops::Neg for ValueMagnitude {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            ValueMagnitude::Int(i) => ValueMagnitude::Int(-i),
            ValueMagnitude::Float(f) => ValueMagnitude::Float(f),
            ValueMagnitude::Constant(c) => {
                ValueMagnitude::Float(-Float::with_val(FLOAT_PRECISION.load(Relaxed), c))
            }
        }
    }
}

impl std::ops::Shl for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn shl(self, rhs: Self) -> Self::Output {
        let lhs: rug::Integer = self.try_into()?;
        let rhs: usize = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs << rhs))
    }
}

impl std::ops::Shr for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn shr(self, rhs: Self) -> Self::Output {
        let lhs: rug::Integer = self.try_into()?;
        let rhs: usize = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs >> rhs))
    }
}

impl std::ops::BitOr for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn bitor(self, rhs: Self) -> Self::Output {
        let lhs: rug::Integer = self.try_into()?;
        let rhs: rug::Integer = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs | rhs))
    }
}

impl std::ops::BitAnd for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        let lhs: rug::Integer = self.try_into()?;
        let rhs: rug::Integer = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs & rhs))
    }
}

impl std::ops::BitXor for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let lhs: rug::Integer = self.try_into()?;
        let rhs: rug::Integer = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs ^ rhs))
    }
}
