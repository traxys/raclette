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
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs_span = lhs.span();
        let rhs_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::mul(
                span,
                a.spanned(&lhs_span),
                b.spanned(&rhs_span),
            )?)),
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
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let l_span = lhs.span();
        let r_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::rem(
                span,
                a.spanned(&l_span),
                b.spanned(&r_span),
            )?)),
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
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs_span = lhs.span();
        let rhs_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::div(
                span,
                a.spanned(&lhs_span),
                b.spanned(&rhs_span),
            )?)),
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
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs_span = lhs.span();
        let rhs_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::add(
                span,
                a.spanned(&lhs_span),
                b.spanned(&rhs_span),
            )?)),
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

    pub fn sub(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs_span = lhs.span();
        let rhs_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::sub(
                span,
                a.spanned(&lhs_span),
                b.spanned(&rhs_span),
            )?)),
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (rhs.start..rhs.end).into(),
                src: lhs.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (lhs.start..lhs.end).into(),
                src: lhs.source,
            }),
        }
    }

    pub fn shl(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let l_span = lhs.span();
        let r_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::shl(
                span,
                a.spanned(&l_span),
                b.spanned(&r_span),
            )?)),
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

    pub fn shr(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let l_span = lhs.span();
        let r_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::shr(
                span,
                a.spanned(&l_span),
                b.spanned(&r_span),
            )?)),
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

    pub fn bit_or(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let l_span = lhs.span();
        let r_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::bit_or(
                span,
                a.spanned(&l_span),
                b.spanned(&r_span),
            )?)),
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

    pub fn bit_and(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let l_span = lhs.span();
        let r_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::bit_and(
                span,
                a.spanned(&l_span),
                b.spanned(&r_span),
            )?)),
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

    pub fn bit_xor(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let l_span = lhs.span();
        let r_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::bit_xor(
                span,
                a.spanned(&l_span),
                b.spanned(&r_span),
            )?)),
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

    pub fn neg(span: Span, val: SpannedValue<Self>) -> Result<Self, RunnerError> {
        let val_span = val.span();
        match val.value {
            Value::Numeric(n) => Ok(Value::Numeric(NumericValue::neg(
                span,
                n.spanned(&val_span),
            )?)),
            v => Err(RunnerError::InvalidType {
                ty: v.ty(),
                location: (val.start..val.end).into(),
                src: val.source,
            }),
        }
    }

    pub fn cmp(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<std::cmp::Ordering, RunnerError> {
        let l_span = lhs.span();
        let r_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => a.spanned(&l_span).cmp(b.spanned(&r_span)),
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

    pub fn eq(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<bool, RunnerError> {
        let l_span = lhs.span();
        let r_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => a.spanned(&l_span).eq(b.spanned(&r_span)),
            (Value::Str(a), Value::Str(b)) => Ok(a == b),
            (Value::Atom(a), Value::Atom(b)) => Ok(a == b),
            (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
            (l, r) => Err(RunnerError::IncompatibleTypes {
                lhs_ty: l.ty(),
                rhs_ty: r.ty(),
                lhs: (lhs.start..lhs.end).into(),
                rhs: (rhs.start..rhs.end).into(),
                src: lhs.source,
            }),
        }
    }

    pub fn pow(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs_span = lhs.span();
        let rhs_span = rhs.span();

        match (lhs.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(
                NumericValue::pow(span, a.spanned(&lhs_span), b.spanned(&rhs_span))?,
            )),
            (Value::Numeric(_), r) => Err(RunnerError::InvalidType {
                ty: r.ty(),
                location: (rhs_span.start..rhs_span.end).into(),
                src: rhs_span.source,
            }),
            (l, _) => Err(RunnerError::InvalidType {
                ty: l.ty(),
                location: (lhs_span.start..lhs_span.end).into(),
                src: lhs_span.source,
            }),
        }
    }
}

impl From<NumericValue> for Value {
    fn from(value: NumericValue) -> Self {
        Self::Numeric(value)
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
