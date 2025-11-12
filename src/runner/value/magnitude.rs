use crate::{
    runner::{CastError, RunnerError},
    span::{Span, SpannedValue},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FiniteF64(f64);

impl Eq for FiniteF64 {}

impl PartialOrd for FiniteF64 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FiniteF64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.partial_cmp(&other.0).unwrap()
    }
}

fn float_guard(f: f64, op_span: Span) -> Result<FiniteF64, RunnerError> {
    if f.is_infinite() {
        Err(RunnerError::Overflow {
            location: (op_span.start..op_span.end).into(),
            src: op_span.source,
        })
    } else if f.is_nan() {
        Err(RunnerError::NaN {
            location: (op_span.start..op_span.end).into(),
            src: op_span.source,
        })
    } else {
        Ok(FiniteF64(f))
    }
}

impl FiniteF64 {
    pub const PI: Self = Self(std::f64::consts::PI);

    pub fn new(value: f64, source_span: Span) -> Result<Self, RunnerError> {
        if value.is_infinite() {
            Err(RunnerError::Overflow {
                location: (source_span.start..source_span.end).into(),
                src: source_span.source,
            })
        } else {
            Ok(Self(value))
        }
    }

    pub fn from_finite(value: f64) -> Self {
        assert!(value.is_finite());
        Self(value)
    }

    pub fn powf(op_span: Span, base: FiniteF64, exponent: FiniteF64) -> Result<Self, RunnerError> {
        float_guard(base.0.powf(exponent.0), op_span)
    }

    pub fn div(op_span: Span, lhs: FiniteF64, rhs: FiniteF64) -> Result<Self, RunnerError> {
        float_guard(lhs.0 / rhs.0, op_span)
    }

    pub fn mul(op_span: Span, lhs: FiniteF64, rhs: FiniteF64) -> Result<Self, RunnerError> {
        float_guard(lhs.0 * rhs.0, op_span)
    }

    pub fn add(op_span: Span, lhs: FiniteF64, rhs: FiniteF64) -> Result<Self, RunnerError> {
        float_guard(lhs.0 + rhs.0, op_span)
    }

    pub fn sub(op_span: Span, lhs: FiniteF64, rhs: FiniteF64) -> Result<Self, RunnerError> {
        float_guard(lhs.0 - rhs.0, op_span)
    }

    pub fn abs(self) -> Self {
        Self(self.0.abs())
    }
}

impl From<FiniteF64> for f64 {
    fn from(value: FiniteF64) -> Self {
        value.0
    }
}

impl std::ops::Neg for FiniteF64 {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ValueMagnitude {
    Int(i128),
    Float(FiniteF64),
}

impl SpannedValue<ValueMagnitude> {
    pub fn eq(self, other: Self) -> Result<bool, RunnerError> {
        match (self.value, other.value) {
            (ValueMagnitude::Int(l), ValueMagnitude::Int(r)) => Ok(l == r),
            (ValueMagnitude::Float(l), ValueMagnitude::Float(r)) => Ok(l == r),
            (ValueMagnitude::Int(l), ValueMagnitude::Float(r)) => Ok(FiniteF64(l as f64) == r),
            (ValueMagnitude::Float(l), ValueMagnitude::Int(r)) => Ok(l == FiniteF64(r as f64)),
        }
    }

    pub fn cmp(self, other: Self) -> Result<std::cmp::Ordering, RunnerError> {
        match (self.value, other.value) {
            (ValueMagnitude::Int(l), ValueMagnitude::Int(r)) => Ok(l.cmp(&r)),
            (ValueMagnitude::Float(l), ValueMagnitude::Float(r)) => Ok(l.cmp(&r)),
            (ValueMagnitude::Int(l), ValueMagnitude::Float(r)) => Ok((FiniteF64(l as f64)).cmp(&r)),
            (ValueMagnitude::Float(l), ValueMagnitude::Int(r)) => Ok(l.cmp(&FiniteF64(r as f64))),
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
                let abs = f.0.abs();
                match rounding {
                    Some(r)
                        if abs >= 0.1f64.powi(r as i32) * 0.98
                            && abs <= (10.0f64.powi(r as i32) + f64::EPSILON) * 1.01 =>
                    {
                        format!("{:.*}", r, f.0)
                    }
                    _ => f.0.to_string(),
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
            ValueMagnitude::Float(f) => f.0 == 0.,
        }
    }

    pub fn into_float(self) -> FiniteF64 {
        match self {
            ValueMagnitude::Int(i) => FiniteF64(i as f64),
            ValueMagnitude::Float(f) => f,
        }
    }

    pub fn into_int(self) -> i128 {
        match self {
            ValueMagnitude::Float(f) => f.0 as i128,
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
                        FiniteF64::powf(
                            op_span.clone(),
                            FiniteF64::new(b as f64, op_span.clone()).unwrap(),
                            FiniteF64::new(e as f64, op_span).unwrap(),
                        )
                        .map(ValueMagnitude::Float)
                    })?)
            }
            (b, e) => Ok(ValueMagnitude::Float(FiniteF64::powf(
                op_span,
                b.into_float(),
                e.into_float(),
            )?)),
        }
    }

    pub fn div(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        Ok(match (lhs.value, rhs.value) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) if b != 0 && (a % b) == 0 => {
                ValueMagnitude::Int(a / b)
            }
            (lhs, rhs) => {
                ValueMagnitude::Float(FiniteF64::div(span, lhs.into_float(), rhs.into_float())?)
            }
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

    pub fn bit_xor(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs: i128 = lhs.try_into()?;
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude::Int(lhs ^ rhs))
    }

    pub fn mul(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        match (lhs.value, rhs.value) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) => a
                .checked_mul(b)
                .map(ValueMagnitude::Int)
                .map(Ok)
                .unwrap_or_else(|| {
                    FiniteF64::mul(
                        span.clone(),
                        FiniteF64::new(a as f64, span.clone()).unwrap(),
                        FiniteF64::new(b as f64, span).unwrap(),
                    )
                    .map(ValueMagnitude::Float)
                }),
            (a, b) => Ok(ValueMagnitude::Float(FiniteF64::mul(
                span,
                a.into_float(),
                b.into_float(),
            )?)),
        }
    }

    pub fn add(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        match (lhs.value, rhs.value) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) => a
                .checked_add(b)
                .map(ValueMagnitude::Int)
                .map(Ok)
                .unwrap_or_else(|| {
                    FiniteF64::add(
                        span.clone(),
                        FiniteF64::new(a as f64, span.clone()).unwrap(),
                        FiniteF64::new(b as f64, span).unwrap(),
                    )
                    .map(ValueMagnitude::Float)
                }),
            (a, b) => Ok(ValueMagnitude::Float(FiniteF64::add(
                span,
                a.into_float(),
                b.into_float(),
            )?)),
        }
    }

    pub fn sub(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        match (lhs.value, rhs.value) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) => a
                .checked_sub(b)
                .map(ValueMagnitude::Int)
                .map(Ok)
                .unwrap_or_else(|| {
                    FiniteF64::sub(
                        span.clone(),
                        FiniteF64::new(a as f64, span.clone()).unwrap(),
                        FiniteF64::new(b as f64, span).unwrap(),
                    )
                    .map(ValueMagnitude::Float)
                }),
            (a, b) => Ok(ValueMagnitude::Float(FiniteF64::sub(
                span,
                a.into_float(),
                b.into_float(),
            )?)),
        }
    }
}
