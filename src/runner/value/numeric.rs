use crate::{
    runner::RunnerError,
    span::{SpannedValue, SpanningExt},
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

impl std::ops::Div for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(NumericValue {
            magnitude: (self.magnitude.spanned(&self) / rhs.magnitude.spanned(&rhs))?,
            unit: self.unit / rhs.unit,
        })
    }
}

impl std::ops::Mul for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(NumericValue {
            magnitude: (self.magnitude.spanned(&self) * rhs.magnitude.spanned(&rhs))?,
            unit: self.value.unit * rhs.value.unit,
        })
    }
}

impl std::ops::Add for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn add(self, rhs: Self) -> Self::Output {
        if self.unit != rhs.unit {
            return Err(RunnerError::UnitMismatch {
                lhs: (self.start..self.end).into(),
                lhs_unit: self.unit.to_string(),
                rhs: (rhs.start..rhs.end).into(),
                rhs_unit: rhs.unit.to_string(),
                src: self.source,
            });
        }

        Ok(NumericValue {
            magnitude: (self.value.magnitude.spanned(&self) + rhs.value.magnitude.spanned(&rhs))?,
            unit: self.value.unit,
        })
    }
}

impl std::ops::Sub for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn sub(self, rhs: Self) -> Self::Output {
        if self.unit != rhs.unit {
            return Err(RunnerError::UnitMismatch {
                lhs: (self.start..self.end).into(),
                lhs_unit: self.unit.to_string(),
                rhs: (rhs.start..rhs.end).into(),
                rhs_unit: rhs.unit.to_string(),
                src: self.source,
            });
        }

        Ok(NumericValue {
            magnitude: (self.value.magnitude.spanned(&self) - rhs.value.magnitude.spanned(&rhs))?,
            unit: self.value.unit,
        })
    }
}

impl std::ops::Shl for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn shl(self, rhs: Self) -> Self::Output {
        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        let self_span = self.span();
        let rhs_span = rhs.span();
        let unit = self.unit;

        Ok(NumericValue {
            magnitude: (self.value.magnitude.spanned(&self_span)
                << rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }
}

impl std::ops::Shr for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn shr(self, rhs: Self) -> Self::Output {
        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        let self_span = self.span();
        let rhs_span = rhs.span();
        let unit = self.unit;

        Ok(NumericValue {
            magnitude: (self.value.magnitude.spanned(&self_span)
                >> rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }
}

impl std::ops::Rem for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn rem(self, rhs: Self) -> Self::Output {
        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        let self_span = self.span();
        let rhs_span = rhs.span();
        let unit = self.unit;

        Ok(NumericValue {
            magnitude: (self.value.magnitude.spanned(&self_span)
                % rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }
}

impl std::ops::BitOr for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn bitor(self, rhs: Self) -> Self::Output {
        if !self.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (self.start..self.end).into(),
                src: self.source,
            });
        }

        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        let self_span = self.span();
        let rhs_span = rhs.span();
        let unit = self.unit;

        Ok(NumericValue {
            magnitude: (self.value.magnitude.spanned(&self_span)
                | rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }
}

impl std::ops::BitAnd for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        if !self.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (self.start..self.end).into(),
                src: self.source,
            });
        }

        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        let self_span = self.span();
        let rhs_span = rhs.span();
        let unit = self.unit;

        Ok(NumericValue {
            magnitude: (self.value.magnitude.spanned(&self_span)
                & rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }
}

impl std::ops::BitXor for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        if !self.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (self.start..self.end).into(),
                src: self.source,
            });
        }

        if !rhs.unit.is_dimensionless() {
            return Err(RunnerError::InvalidType {
                ty: "dimensioned numeric value",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        let self_span = self.span();
        let rhs_span = rhs.span();
        let unit = self.unit;

        Ok(NumericValue {
            magnitude: (self.value.magnitude.spanned(&self_span)
                ^ rhs.value.magnitude.spanned(&rhs_span))?,
            unit,
        })
    }
}

impl std::ops::Neg for SpannedValue<NumericValue> {
    type Output = Result<NumericValue, RunnerError>;

    fn neg(self) -> Self::Output {
        Ok(NumericValue {
            magnitude: (-self.value.magnitude.spanned(&self))?,
            unit: self.value.unit,
        })
    }
}
