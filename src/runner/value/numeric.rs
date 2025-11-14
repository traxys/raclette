use crate::{
    runner::RunnerError,
    span::{Span, SpannedValue, SpanningExt},
};

use super::{Unit, ValueMagnitude};

#[derive(Debug, Clone)]
pub struct NumericValue {
    pub magnitude: ValueMagnitude,
    pub unit: Unit,
}

impl SpannedValue<NumericValue> {
    pub fn eq(self, other: Self) -> Result<bool, RunnerError> {
        if self.unit != other.unit {
            return Err(RunnerError::UnitMismatch {
                lhs: (self.start..self.end).into(),
                lhs_unit: self.unit.to_string(),
                rhs: (other.start..other.end).into(),
                rhs_unit: other.unit.to_string(),
                src: self.source,
            });
        }

        Ok(self.magnitude().eq(&other.magnitude()))
    }

    pub fn cmp(self, other: Self) -> Result<std::cmp::Ordering, RunnerError> {
        if self.unit != other.unit {
            return Err(RunnerError::UnitMismatch {
                lhs: (self.start..self.end).into(),
                lhs_unit: self.unit.to_string(),
                rhs: (other.start..other.end).into(),
                rhs_unit: other.unit.to_string(),
                src: self.source,
            });
        }

        Ok(self.magnitude().cmp(&other.magnitude()))
    }

    pub fn magnitude(self) -> SpannedValue<ValueMagnitude> {
        let span = self.span();
        self.value.magnitude.spanned(&span)
    }
}

impl NumericValue {
    pub fn mul(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        Ok(NumericValue {
            unit: lhs.value.unit * rhs.value.unit,
            magnitude: ValueMagnitude::mul(span, lhs.magnitude(), rhs.magnitude())?,
        })
    }

    pub fn div(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        Ok(NumericValue {
            unit: lhs.unit / rhs.unit,
            magnitude: ValueMagnitude::div(span, lhs.magnitude(), rhs.magnitude())?,
        })
    }

    pub fn add(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        if lhs.unit != rhs.unit {
            return Err(RunnerError::UnitMismatch {
                lhs: (lhs.start..lhs.end).into(),
                lhs_unit: lhs.unit.to_string(),
                rhs: (rhs.start..rhs.end).into(),
                rhs_unit: rhs.unit.to_string(),
                src: lhs.source,
            });
        }

        Ok(NumericValue {
            unit: lhs.value.unit,
            magnitude: ValueMagnitude::add(span, lhs.magnitude(), rhs.magnitude())?,
        })
    }

    pub fn sub(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        if lhs.unit != rhs.unit {
            return Err(RunnerError::UnitMismatch {
                lhs: (lhs.start..lhs.end).into(),
                lhs_unit: lhs.unit.to_string(),
                rhs: (rhs.start..rhs.end).into(),
                rhs_unit: rhs.unit.to_string(),
                src: lhs.source,
            });
        }

        Ok(NumericValue {
            unit: lhs.value.unit,
            magnitude: ValueMagnitude::sub(span, lhs.magnitude(), rhs.magnitude())?,
        })
    }

    pub fn shl(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        Ok(NumericValue {
            unit: lhs.unit,
            magnitude: ValueMagnitude::shl(span, lhs.magnitude(), rhs.magnitude())?,
        })
    }

    pub fn shr(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        Ok(NumericValue {
            unit: lhs.unit,
            magnitude: ValueMagnitude::shr(span, lhs.magnitude(), rhs.magnitude())?,
        })
    }

    pub fn rem(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        Ok(NumericValue {
            unit: lhs.unit,
            magnitude: ValueMagnitude::rem(span, lhs.magnitude(), rhs.magnitude())?,
        })
    }

    pub fn bit_or(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        if !lhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (lhs.start..lhs.end).into(),
                src: lhs.source,
            });
        }

        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        Ok(NumericValue {
            unit: lhs.unit,
            magnitude: ValueMagnitude::bit_or(span, lhs.magnitude(), rhs.magnitude())?,
        })
    }

    pub fn bit_and(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        if !lhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (lhs.start..lhs.end).into(),
                src: lhs.source,
            });
        }

        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        Ok(NumericValue {
            unit: lhs.unit,
            magnitude: ValueMagnitude::bit_and(span, lhs.magnitude(), rhs.magnitude())?,
        })
    }

    pub fn bit_xor(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        if !lhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (lhs.start..lhs.end).into(),
                src: lhs.source,
            });
        }

        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        Ok(NumericValue {
            unit: lhs.unit,
            magnitude: ValueMagnitude::bit_xor(span, lhs.magnitude(), rhs.magnitude())?,
        })
    }

    pub fn neg(span: Span, val: SpannedValue<Self>) -> Result<Self, RunnerError> {
        Ok(NumericValue {
            unit: val.value.unit,
            magnitude: ValueMagnitude::neg(span, val.magnitude())?,
        })
    }

    pub fn pow(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        let rhs_span = rhs.span();

        if !rhs.unit.is_dimensionless() {
            Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs_span.start..rhs_span.end).into(),
                src: rhs_span.source.clone(),
            })?;
        }

        if lhs.unit.is_dimensionless() {
            Ok(NumericValue {
                unit: Unit::dimensionless(),
                magnitude: lhs.value.magnitude.pow(span, rhs.magnitude())?,
            })
        } else {
            let exponent: i128 = rhs.clone().magnitude().try_into()?;

            if exponent.unsigned_abs() > i64::MAX as u128 {
                return Err(RunnerError::UnitOverflow {
                    location: (span.start..span.end).into(),
                    src: span.source,
                });
            }

            let unit = lhs.value.unit.pow(span.clone(), exponent as i64)?;

            Ok(NumericValue {
                unit,
                magnitude: lhs.value.magnitude.pow(span, rhs.magnitude())?,
            })
        }
    }
}
