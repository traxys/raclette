mod magnitude;
mod numeric;
mod unit;

use std::sync::Arc;

use super::{CastError, RunnerError};
use crate::span::{Span, SpannedValue, SpanningExt};

pub use magnitude::ValueMagnitude;
pub use numeric::NumericValue;
pub use unit::{
    Dimension, ScaleRender, ScaleType, Unit, BYTE_UNIT, KNOWN_UNITS, MASS_UNIT, TIME_UNIT,
};

#[derive(Debug, Clone)]
pub enum Value {
    Numeric(NumericValue),
    Str(String),
    Bool(bool),
    Atom(Arc<str>),
}

impl Value {
    pub fn is_zero(&self) -> bool {
        match self {
            Value::Numeric(n) => n.magnitude.is_zero(),
            Value::Str(_) => false,
            Value::Atom(_) => false,
            Value::Bool(_) => false,
        }
    }

    pub fn ty(&self) -> &'static str {
        match self {
            Value::Numeric(_) => "number",
            Value::Str(_) => "str",
            Value::Bool(_) => "bool",
            Value::Atom(_) => "atom",
        }
    }

    pub fn mul(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs_span = lhs.span();
        let rhs_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(
                (a.spanned(&lhs_span) * b.spanned(&rhs_span))?,
            )),
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (lhs.start..lhs.end).into(),
                src: lhs.source,
            }),
        }
    }

    pub fn rem(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let l_span = lhs.span();
        let r_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => {
                Ok(Value::Numeric((a.spanned(&l_span) % b.spanned(&r_span))?))
            }
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (lhs.start..lhs.end).into(),
                src: lhs.source,
            }),
        }
    }

    pub fn div(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs_span = lhs.span();
        let rhs_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(
                (a.spanned(&lhs_span) / b.spanned(&rhs_span))?,
            )),
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (lhs.start..lhs.end).into(),
                src: lhs.source,
            }),
        }
    }

    pub fn add(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs_span = lhs.span();
        let rhs_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(
                (a.spanned(&lhs_span) + b.spanned(&rhs_span))?,
            )),
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (lhs.start..lhs.end).into(),
                src: lhs.source,
            }),
        }
    }
}

impl SpannedValue<Value> {
    pub fn eq(self, other: Self) -> Result<bool, RunnerError> {
        let l_span = self.span();
        let r_span = other.span();
        match (self.value, other.value) {
            (Value::Numeric(a), Value::Numeric(b)) => a.spanned(&l_span).eq(b.spanned(&r_span)),
            (Value::Str(a), Value::Str(b)) => Ok(a == b),
            (Value::Atom(a), Value::Atom(b)) => Ok(a == b),
            (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
            (l, r) => Err(RunnerError::IncompatibleTypes {
                lhs_ty: l.ty(),
                rhs_ty: r.ty(),
                lhs: (self.start..self.end).into(),
                rhs: (other.start..other.end).into(),
                src: self.source,
            }),
        }
    }

    pub fn cmp(self, other: Self) -> Result<std::cmp::Ordering, RunnerError> {
        let l_span = self.span();
        let r_span = other.span();
        match (self.value, other.value) {
            (Value::Numeric(a), Value::Numeric(b)) => a.spanned(&l_span).cmp(b.spanned(&r_span)),
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (other.start..other.end).into(),
                src: self.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
        }
    }
}

impl From<NumericValue> for Value {
    fn from(value: NumericValue) -> Self {
        Self::Numeric(value)
    }
}

impl std::ops::Neg for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn neg(self) -> Self::Output {
        let span = self.span();
        match self.value {
            Value::Numeric(n) => Ok(Value::Numeric((-n.spanned(&span))?)),
            v => Err(RunnerError::InvalidType {
                ty: v.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
        }
    }
}

impl std::ops::Shl for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn shl(self, rhs: Self) -> Self::Output {
        let l_span = self.span();
        let r_span = rhs.span();

        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => {
                Ok(Value::Numeric((a.spanned(&l_span) << b.spanned(&r_span))?))
            }
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
        }
    }
}

impl std::ops::Shr for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn shr(self, rhs: Self) -> Self::Output {
        let l_span = self.span();
        let r_span = rhs.span();

        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => {
                Ok(Value::Numeric((a.spanned(&l_span) >> b.spanned(&r_span))?))
            }
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
        }
    }
}

impl std::ops::BitOr for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn bitor(self, rhs: Self) -> Self::Output {
        let l_span = self.span();
        let r_span = rhs.span();

        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => {
                Ok(Value::Numeric((a.spanned(&l_span) | b.spanned(&r_span))?))
            }
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
        }
    }
}

impl std::ops::BitAnd for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        let l_span = self.span();
        let r_span = rhs.span();

        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => {
                Ok(Value::Numeric((a.spanned(&l_span) & b.spanned(&r_span))?))
            }
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
        }
    }
}

impl std::ops::BitXor for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let l_span = self.span();
        let r_span = rhs.span();

        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => {
                Ok(Value::Numeric((a.spanned(&l_span) ^ b.spanned(&r_span))?))
            }
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
        }
    }
}

impl std::ops::Sub for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn sub(self, rhs: Self) -> Self::Output {
        let lhs_span = self.span();
        let rhs_span = rhs.span();

        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(
                (a.spanned(&lhs_span) - b.spanned(&rhs_span))?,
            )),
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (self.start..self.end).into(),
                src: self.source,
            }),
        }
    }
}

impl TryFrom<SpannedValue<Value>> for u32 {
    type Error = CastError;

    fn try_from(value: SpannedValue<Value>) -> Result<Self, Self::Error> {
        let spn = value.span();
        let numeric: i128 = value.try_into()?;

        match numeric.try_into() {
            Err(_) => Err(CastError::from_val(
                Value::Numeric(NumericValue {
                    magnitude: ValueMagnitude::Int(numeric),
                    unit: Unit::dimensionless(),
                })
                .spanned(&spn),
                "u32",
            )),
            Ok(v) => Ok(v),
        }
    }
}

impl TryFrom<SpannedValue<Value>> for bool {
    type Error = CastError;

    fn try_from(value: SpannedValue<Value>) -> Result<Self, Self::Error> {
        match value.value {
            Value::Bool(b) => Ok(b),
            _ => Err(CastError::from_val(value, "bool")),
        }
    }
}

impl TryFrom<SpannedValue<Value>> for i128 {
    type Error = CastError;

    fn try_from(value: SpannedValue<Value>) -> Result<Self, Self::Error> {
        match value.value {
            Value::Numeric(NumericValue {
                magnitude: ValueMagnitude::Int(i),
                ..
            }) => Ok(i),
            _ => Err(CastError::from_val(value, "int")),
        }
    }
}

impl TryFrom<SpannedValue<Value>> for NumericValue {
    type Error = CastError;

    fn try_from(value: SpannedValue<Value>) -> Result<Self, Self::Error> {
        match value {
            SpannedValue {
                value: Value::Numeric(n),
                ..
            } => Ok(n),
            SpannedValue {
                value: Value::Str(_),
                ..
            } => Err(CastError::from_val(value, "numeric")),
            SpannedValue {
                value: Value::Atom(_),
                ..
            } => Err(CastError::from_val(value, "numeric")),
            SpannedValue {
                value: Value::Bool(_),
                ..
            } => Err(CastError::from_val(value, "numeric")),
        }
    }
}
