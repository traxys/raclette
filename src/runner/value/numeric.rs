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

impl std::ops::Div for NumericValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        NumericValue {
            magnitude: self.magnitude / rhs.magnitude,
            unit: self.unit / rhs.unit,
        }
    }
}

impl std::ops::Mul for NumericValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        NumericValue {
            magnitude: self.magnitude * rhs.magnitude,
            unit: self.unit * rhs.unit,
        }
    }
}

impl std::ops::Add for NumericValue {
    type Output = Result<Self, ()>;

    fn add(self, rhs: Self) -> Self::Output {
        if self.unit != rhs.unit {
            return Err(());
        }

        Ok(Self {
            magnitude: self.magnitude + rhs.magnitude,
            unit: self.unit,
        })
    }
}

impl std::ops::Sub for NumericValue {
    type Output = Result<Self, ()>;

    fn sub(self, rhs: Self) -> Self::Output {
        if self.unit != rhs.unit {
            return Err(());
        }

        Ok(Self {
            magnitude: self.magnitude - rhs.magnitude,
            unit: self.unit,
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

#[derive(Debug)]
pub enum Void {}

impl std::ops::Neg for NumericValue {
    type Output = Result<Self, Void>;

    fn neg(self) -> Self::Output {
        Ok(Self {
            magnitude: -self.magnitude,
            unit: self.unit,
        })
    }
}
