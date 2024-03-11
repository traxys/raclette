use crate::{runner::CastError, span::SpannedValue};

#[derive(Debug, Clone, Copy)]
pub enum ValueMagnitude {
    Int(i64),
    Float(f64),
}

impl TryFrom<SpannedValue<ValueMagnitude>> for i64 {
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
            ValueMagnitude::Int(i) if TryInto::<usize>::try_into(i).is_ok() => Ok(i as usize),
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
            ValueMagnitude::Float(f) => {
                let abs = f.clone().abs();
                match rounding {
                    Some(r)
                        if abs >= 0.1f64.powi(r as i32) * 0.98
                            && abs <= (10.0f64.powi(r as i32) + f64::EPSILON) * 1.01 =>
                    {
                        format!("{:.*}", r, f)
                    }
                    _ => f.to_string(),
                }
            }
        }
    }

    pub fn ty(&self) -> &'static str {
        match self {
            ValueMagnitude::Int(_) => "int",
            ValueMagnitude::Float(_) => "float",
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            &ValueMagnitude::Int(i) => i == 0,
            &ValueMagnitude::Float(f) => f == 0.,
        }
    }

    pub fn into_float(self) -> f64 {
        match self {
            ValueMagnitude::Int(i) => i as f64,
            ValueMagnitude::Float(f) => f,
        }
    }

    pub fn into_int(self) -> i64 {
        match self {
            ValueMagnitude::Float(f) => f as i64,
            ValueMagnitude::Int(i) => i,
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
                    (ValueMagnitude::Int(a), ValueMagnitude::Float(b)) => ValueMagnitude::Float(a as f64 $symb b),
                    (ValueMagnitude::Float(a), ValueMagnitude::Int(b)) => ValueMagnitude::Float(a $symb b as f64),
                    (ValueMagnitude::Float(a), ValueMagnitude::Float(b)) => ValueMagnitude::Float(a $symb b),
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
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) if (a % b) == 0 => {
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
            ValueMagnitude::Float(f) => ValueMagnitude::Float(-f),
        }
    }
}

impl std::ops::Shl for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn shl(self, rhs: Self) -> Self::Output {
        let lhs: i64 = self.try_into()?;
        let rhs: usize = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs << rhs))
    }
}

impl std::ops::Shr for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn shr(self, rhs: Self) -> Self::Output {
        let lhs: i64 = self.try_into()?;
        let rhs: usize = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs >> rhs))
    }
}

impl std::ops::BitOr for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn bitor(self, rhs: Self) -> Self::Output {
        let lhs: i64 = self.try_into()?;
        let rhs: i64 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs | rhs))
    }
}

impl std::ops::BitAnd for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        let lhs: i64 = self.try_into()?;
        let rhs: i64 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs & rhs))
    }
}

impl std::ops::BitXor for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let lhs: i64 = self.try_into()?;
        let rhs: i64 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs ^ rhs))
    }
}
