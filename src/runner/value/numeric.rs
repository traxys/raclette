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

        self.magnitude
            .spanned(&self)
            .eq(other.magnitude.spanned(&other))
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

        self.magnitude
            .spanned(&self)
            .cmp(other.magnitude.spanned(&other))
    }

    pub fn magnitude(&self) -> SpannedValue<ValueMagnitude> {
        self.magnitude.spanned(self)
    }
}

impl NumericValue {
    pub fn mul(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        Ok(NumericValue {
            magnitude: (lhs.magnitude.spanned(&lhs) * rhs.magnitude.spanned(&rhs))?,
            unit: lhs.value.unit * rhs.value.unit,
        })
    }

    pub fn div(
        span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        Ok(NumericValue {
            magnitude: ValueMagnitude::div(span, lhs.magnitude(), rhs.magnitude())?,
            unit: lhs.unit / rhs.unit,
        })
    }

    pub fn add(
        _span: Span,
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
            magnitude: (lhs.value.magnitude.spanned(&lhs) + rhs.value.magnitude.spanned(&rhs))?,
            unit: lhs.value.unit,
        })
    }

    pub fn sub(
        _span: Span,
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
            magnitude: (lhs.value.magnitude.spanned(&lhs) - rhs.value.magnitude.spanned(&rhs))?,
            unit: lhs.value.unit,
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
            magnitude: ValueMagnitude::shl(span, lhs.magnitude(), rhs.magnitude())?,
            unit: lhs.unit,
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
            magnitude: ValueMagnitude::shr(span, lhs.magnitude(), rhs.magnitude())?,
            unit: lhs.unit,
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
            magnitude: ValueMagnitude::rem(span, lhs.magnitude(), rhs.magnitude())?,
            unit: lhs.unit,
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
            magnitude: ValueMagnitude::bit_or(span, lhs.magnitude(), rhs.magnitude())?,
            unit: lhs.unit,
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
            magnitude: ValueMagnitude::bit_and(span, lhs.magnitude(), rhs.magnitude())?,
            unit: lhs.unit,
        })
    }

    pub fn bit_xor(
        _span: Span,
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

        let lhs_span = lhs.span();
        let rhs_span = rhs.span();
        let unit = lhs.unit;

        Ok(NumericValue {
            magnitude: (lhs.value.magnitude.spanned(&lhs_span)
                ^ rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }

    pub fn neg(span: Span, val: SpannedValue<Self>) -> Result<Self, RunnerError> {
        Ok(NumericValue {
            magnitude: ValueMagnitude::neg(span, val.magnitude())?,
            unit: val.value.unit,
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
                magnitude: lhs.magnitude.pow(span, rhs.magnitude)?,
                unit: Unit::dimensionless(),
            })
        } else {
            let exponent: i128 = rhs.value.magnitude.spanned(&rhs_span).try_into()?;

            if exponent.unsigned_abs() > i64::MAX as u128 {
                return Err(RunnerError::UnitOverflow {
                    location: (span.start..span.end).into(),
                    src: span.source,
                });
            }

            let unit = lhs.value.unit.pow(span.clone(), exponent as i64)?;

            Ok(NumericValue {
                magnitude: lhs.magnitude.pow(span, rhs.magnitude)?,
                unit,
            })
        }
    }
}
