use crate::{
    runner::{CastError, RunnerError},
    span::SpannedValue,
};

#[derive(Debug, Clone, Copy)]
pub enum ValueMagnitude {
    Int(i128),
    Float(f64),
}

impl SpannedValue<ValueMagnitude> {
    pub fn nan_guard(&self) -> Result<(), RunnerError> {
        match self.value {
            ValueMagnitude::Float(f) if f.is_nan() => Err(RunnerError::NaN {
                location: (self.start..self.end).into(),
                src: self.source.clone(),
            }),
            _ => Ok(()),
        }
    }

    pub fn eq(self, other: Self) -> Result<bool, RunnerError> {
        self.nan_guard()?;
        other.nan_guard()?;

        match (self.value, other.value) {
            (ValueMagnitude::Int(l), ValueMagnitude::Int(r)) => Ok(l == r),
            (ValueMagnitude::Float(l), ValueMagnitude::Float(r)) => Ok(l == r),
            (ValueMagnitude::Int(l), ValueMagnitude::Float(r)) => Ok(l as f64 == r),
            (ValueMagnitude::Float(l), ValueMagnitude::Int(r)) => Ok(l == r as f64),
        }
    }

    pub fn cmp(self, other: Self) -> Result<std::cmp::Ordering, RunnerError> {
        self.nan_guard()?;
        other.nan_guard()?;

        match (self.value, other.value) {
            (ValueMagnitude::Int(l), ValueMagnitude::Int(r)) => Ok(l.cmp(&r)),
            (ValueMagnitude::Float(l), ValueMagnitude::Float(r)) => Ok(l.partial_cmp(&r).unwrap()),
            (ValueMagnitude::Int(l), ValueMagnitude::Float(r)) => {
                Ok((l as f64).partial_cmp(&r).unwrap())
            }
            (ValueMagnitude::Float(l), ValueMagnitude::Int(r)) => {
                Ok(l.partial_cmp(&(r as f64)).unwrap())
            }
        }
    }
}

impl TryFrom<SpannedValue<ValueMagnitude>> for i128 {
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

    pub fn into_int(self) -> i128 {
        match self {
            ValueMagnitude::Float(f) => f as i128,
            ValueMagnitude::Int(i) => i,
        }
    }
}

macro_rules! impl_op {
    ($op:ident, $fn:ident, $forward:ident) => {
        impl std::ops::$op for SpannedValue<ValueMagnitude> {
            type Output = Result<ValueMagnitude, RunnerError>;

            fn $fn(self, rhs: Self) -> Self::Output {
                Ok(match (self.value, rhs.value) {
                    (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) => a
                        .$forward(b)
                        .map(ValueMagnitude::Int)
                        .unwrap_or_else(|| ValueMagnitude::Float((a as f64).$fn(b as f64))),
                    (ValueMagnitude::Int(a), ValueMagnitude::Float(b)) => {
                        ValueMagnitude::Float((a as f64).$fn(b))
                    }
                    (ValueMagnitude::Float(a), ValueMagnitude::Int(b)) => {
                        ValueMagnitude::Float(a.$fn(b as f64))
                    }
                    (ValueMagnitude::Float(a), ValueMagnitude::Float(b)) => {
                        ValueMagnitude::Float(a.$fn(b))
                    }
                })
            }
        }
    };
}

impl_op!(Mul, mul, checked_mul);
impl_op!(Add, add, checked_add);
impl_op!(Sub, sub, checked_sub);

impl std::ops::Div for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, RunnerError>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self.value, rhs.value) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) if (a % b) == 0 => {
                ValueMagnitude::Int(a / b)
            }
            (lhs, rhs) => ValueMagnitude::Float(lhs.into_float() / rhs.into_float()),
        })
    }
}

impl std::ops::Neg for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, RunnerError>;

    fn neg(self) -> Self::Output {
        Ok(match self.value {
            ValueMagnitude::Int(i) => ValueMagnitude::Int(-i),
            ValueMagnitude::Float(f) => ValueMagnitude::Float(-f),
        })
    }
}

impl std::ops::Shl for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn shl(self, rhs: Self) -> Self::Output {
        let lhs: i128 = self.try_into()?;
        let rhs: usize = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs << rhs))
    }
}

impl std::ops::Shr for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn shr(self, rhs: Self) -> Self::Output {
        let lhs: i128 = self.try_into()?;
        let rhs: usize = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs >> rhs))
    }
}

impl std::ops::Rem for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn rem(self, rhs: Self) -> Self::Output {
        let lhs: i128 = self.try_into()?;
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs % rhs))
    }
}

impl std::ops::BitOr for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn bitor(self, rhs: Self) -> Self::Output {
        let lhs: i128 = self.try_into()?;
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs | rhs))
    }
}

impl std::ops::BitAnd for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        let lhs: i128 = self.try_into()?;
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs & rhs))
    }
}

impl std::ops::BitXor for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let lhs: i128 = self.try_into()?;
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs ^ rhs))
    }
}
