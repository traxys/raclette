mod magnitude;
mod numeric;
mod unit;

use std::sync::Arc;

use super::{CastError, RunnerError};
use crate::span::{SpannedValue, SpanningExt};

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
}

impl SpannedValue<Value> {
    pub fn eq(self, other: Self, ) -> Result<bool, RunnerError> {
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

impl std::ops::Div for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a / b)),
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

impl std::ops::Mul for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a * b)),
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

impl std::ops::Add for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn add(self, rhs: Self) -> Self::Output {
        let l_span = self.span();
        let r_span = rhs.span();

        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => {
                let lhs_unit = a.unit.to_string();
                let rhs_unit = b.unit.to_string();
                Ok(Value::Numeric((a + b).map_err(|_| {
                    RunnerError::UnitMismatch {
                        lhs: (l_span.start..l_span.end).into(),
                        lhs_unit,
                        rhs: (r_span.start..r_span.end).into(),
                        rhs_unit,
                        src: self.source,
                    }
                })?))
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

impl std::ops::Neg for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn neg(self) -> Self::Output {
        match self.value {
            Value::Numeric(n) => Ok(Value::Numeric((-n).unwrap())),
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
        let l_span = self.span();
        let r_span = rhs.span();

        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => {
                let lhs_unit = a.unit.to_string();
                let rhs_unit = b.unit.to_string();
                Ok(Value::Numeric((a - b).map_err(|_| {
                    RunnerError::UnitMismatch {
                        lhs: (l_span.start..l_span.end).into(),
                        lhs_unit,
                        rhs: (r_span.start..r_span.end).into(),
                        rhs_unit,
                        src: self.source,
                    }
                })?))
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

impl TryFrom<SpannedValue<Value>> for u32 {
    type Error = CastError;

    fn try_from(value: SpannedValue<Value>) -> Result<Self, Self::Error> {
        let spn = value.span();
        let numeric: i64 = value.try_into()?;

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

impl TryFrom<SpannedValue<Value>> for i64 {
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
