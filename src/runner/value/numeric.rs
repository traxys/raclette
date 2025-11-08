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
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        Ok(NumericValue {
            magnitude: (lhs.magnitude.spanned(&lhs) / rhs.magnitude.spanned(&rhs))?,
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
        _span: Span,
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

        let lhs_span = lhs.span();
        let rhs_span = rhs.span();
        let unit = lhs.unit;

        Ok(NumericValue {
            magnitude: (lhs.value.magnitude.spanned(&lhs_span)
                << rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }

    pub fn shr(
        _span: Span,
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

        let lhs_span = lhs.span();
        let rhs_span = rhs.span();
        let unit = lhs.unit;

        Ok(NumericValue {
            magnitude: (lhs.value.magnitude.spanned(&lhs_span)
                >> rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }

    pub fn rem(
        _span: Span,
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

        let lhs_span = lhs.span();
        let rhs_span = rhs.span();
        let unit = lhs.unit;

        Ok(NumericValue {
            magnitude: (lhs.value.magnitude.spanned(&lhs_span)
                % rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }

    pub fn bit_or(
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
                | rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }

    pub fn bit_and(
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
                & rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
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

    pub fn neg(_span: Span, val: SpannedValue<Self>) -> Result<Self, RunnerError> {
        Ok(NumericValue {
            magnitude: (-val.value.magnitude.spanned(&val))?,
            unit: val.value.unit,
        })
    }

    pub fn pow(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<NumericValue, RunnerError> {
        let lhs_span = lhs.span();
        let rhs_span = rhs.span();

        if !rhs.unit.is_dimensionless() {
            Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs_span.start..rhs_span.end).into(),
                src: rhs_span.source.clone(),
            })?;
        }

        let value: i128 = lhs.value.magnitude.spanned(&lhs_span).try_into()?;
        let exponent: i128 = rhs.value.magnitude.spanned(&rhs_span).try_into()?;

        if exponent.unsigned_abs() > u32::MAX as u128 {
            panic!("Attempted to exponentiate with overflow");
        }

        let neg = exponent < 0;
        let exponent = exponent.unsigned_abs() as u32;

        let exp_unit = lhs.value.unit.pow(exponent);

        Ok(if neg {
            NumericValue {
                magnitude: ValueMagnitude::Float(1. / (value as f64).powf(exponent as f64)),
                unit: Unit {
                    dimensions: exp_unit.dimensions.map(|_, x| -x),
                },
            }
        } else {
            NumericValue {
                magnitude: ValueMagnitude::Int(value.pow(exponent)),
                unit: exp_unit,
            }
        })
    }
}
