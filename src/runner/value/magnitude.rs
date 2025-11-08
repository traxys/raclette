use crate::{
    runner::{CastError, RunnerError},
    span::{Span, SpannedValue},
};

#[derive(Debug, Clone, Copy)]
pub enum ValueMagnitude {
    Int(i128),
    Float(f64),
}

trait FloatGuard: Sized {
    fn guard_infinity(self, op_span: Span) -> Result<Self, RunnerError>;
}

impl FloatGuard for f64 {
    fn guard_infinity(self, op_span: Span) -> Result<Self, RunnerError> {
        if self.is_infinite() {
            Err(RunnerError::Overflow {
                location: (op_span.start..op_span.end).into(),
                src: op_span.source,
            })
        } else {
            Ok(self)
        }
    }
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
                let abs = f.abs();
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
        match *self {
            ValueMagnitude::Int(i) => i == 0,
            ValueMagnitude::Float(f) => f == 0.,
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

    pub fn pow(self, op_span: Span, exponent: Self) -> Result<ValueMagnitude, RunnerError> {
        match (self, exponent) {
            (ValueMagnitude::Int(b), ValueMagnitude::Int(e)) if e >= 0 && e <= u32::MAX as i128 => {
                Ok(b.checked_pow(e as u32)
                    .map(ValueMagnitude::Int)
                    .map(Ok)
                    .unwrap_or_else(|| {
                        (b as f64)
                            .powf(e as f64)
                            .guard_infinity(op_span)
                            .map(ValueMagnitude::Float)
                    })?)
            }
            (b, e) => Ok(ValueMagnitude::Float(
                b.into_float()
                    .powf(e.into_float())
                    .guard_infinity(op_span)?,
            )),
        }
    }

    pub fn div(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        Ok(match (lhs.value, rhs.value) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) if (a % b) == 0 => {
                ValueMagnitude::Int(a / b)
            }
            (lhs, rhs) => ValueMagnitude::Float(lhs.into_float() / rhs.into_float()),
        })
    }

    pub fn neg(_span: Span, val: SpannedValue<Self>) -> Result<Self, RunnerError> {
        Ok(match val.value {
            ValueMagnitude::Int(i) => ValueMagnitude::Int(-i),
            ValueMagnitude::Float(f) => ValueMagnitude::Float(-f),
        })
    }

    pub fn shl(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs: i128 = lhs.try_into()?;
        let rhs: usize = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs << rhs))
    }

    pub fn shr(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs: i128 = lhs.try_into()?;
        let rhs: usize = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs >> rhs))
    }

    pub fn rem(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs: i128 = lhs.try_into()?;
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs % rhs))
    }

    pub fn bit_or(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs: i128 = lhs.try_into()?;
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs | rhs))
    }

    pub fn bit_and(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs: i128 = lhs.try_into()?;
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs & rhs))
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

impl std::ops::BitXor for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, CastError>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let lhs: i128 = self.try_into()?;
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs ^ rhs))
    }
}
