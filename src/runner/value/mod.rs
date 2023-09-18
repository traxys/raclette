mod magnitude;
mod numeric;
mod unit;

use std::sync::Arc;

use super::{CastError, RunnerError};
use crate::span::{SpannedValue, SpanningExt};

pub use magnitude::ValueMagnitude;
pub use numeric::NumericValue;
pub use unit::{
    Dimension, ScaleRender, ScaleStep, ScaleType, Unit, BYTE_UNIT, KNOWN_UNITS, MASS_UNIT,
    TIME_UNIT,
};

#[derive(Debug, Clone)]
pub enum Value {
    Numeric(NumericValue),
    Str(String),
    Atom(Arc<str>),
}

impl Value {
    pub fn is_zero(&self) -> bool {
        match self {
            Value::Numeric(n) => n.magnitude.is_zero(),
            Value::Str(_) => false,
            Value::Atom(_) => false,
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
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
        }
    }
}

impl std::ops::Mul for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a * b)),
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
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
                let rhs_unit = a.unit.to_string();
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
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
        }
    }
}

impl std::ops::Neg for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn neg(self) -> Self::Output {
        match self.value {
            Value::Numeric(n) => Ok(Value::Numeric((-n).unwrap())),
            Value::Str(_) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            Value::Atom(_) => Err(RunnerError::InvalidType {
                ty: "atom",
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
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
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
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
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
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
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
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
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
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
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
                let rhs_unit = a.unit.to_string();
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
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
        }
    }
}

impl TryFrom<SpannedValue<Value>> for rug::Integer {
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
        }
    }
}
