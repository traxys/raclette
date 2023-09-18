use crate::{runner::RunnerError, span::SpannedValue};

#[derive(Debug, Clone, Copy)]
pub enum ValueMagnitude {
    Int(i64),
    Float(f64),
}

impl TryFrom<SpannedValue<ValueMagnitude>> for u64 {
    type Error = RunnerError;

    fn try_from(value: SpannedValue<ValueMagnitude>) -> Result<Self, Self::Error> {
        match value.value {
            ValueMagnitude::Int(i) if i >= 0 => Ok(i as u64),
            ValueMagnitude::Float(f) if f == (f as u64) as f64 => Ok(f as u64),
            _ => Err(RunnerError::UintConversion {
                location: (value.start..value.end).into(),
                src: value.source,
            }),
        }
    }
}

impl ValueMagnitude {
    pub fn as_string(&self, precision: Option<usize>) -> String {
        match self {
            ValueMagnitude::Int(n) => n.to_string(),
            ValueMagnitude::Float(f) => match precision {
                None => f.to_string(),
                Some(p) => format!("{f:.*}", p),
            },
        }
    }

    pub fn is_zero(&self) -> bool {
        match *self {
            ValueMagnitude::Int(i) => i == 0,
            ValueMagnitude::Float(f) => f == 0.,
        }
    }

    pub fn as_float(&self) -> f64 {
        match *self {
            ValueMagnitude::Int(i) => i as _,
            ValueMagnitude::Float(f) => f,
        }
    }
}

impl std::ops::Mul for ValueMagnitude {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // TODO: Switch to float if result is too large
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) => ValueMagnitude::Int(a * b),
            (ValueMagnitude::Int(a), ValueMagnitude::Float(b)) => {
                ValueMagnitude::Float(a as f64 * b)
            }
            (ValueMagnitude::Float(a), ValueMagnitude::Int(b)) => {
                ValueMagnitude::Float(a * b as f64)
            }
            (ValueMagnitude::Float(a), ValueMagnitude::Float(b)) => ValueMagnitude::Float(a * b),
        }
    }
}

impl std::ops::Div for ValueMagnitude {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) if a % b == 0 => {
                ValueMagnitude::Int(a / b)
            }
            (lhs, rhs) => ValueMagnitude::Float(lhs.as_float() / rhs.as_float()),
        }
    }
}

impl std::ops::Add for ValueMagnitude {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) => ValueMagnitude::Int(a + b),
            (ValueMagnitude::Int(a), ValueMagnitude::Float(b)) => {
                ValueMagnitude::Float(a as f64 + b)
            }
            (ValueMagnitude::Float(a), ValueMagnitude::Int(b)) => {
                ValueMagnitude::Float(a + b as f64)
            }
            (ValueMagnitude::Float(a), ValueMagnitude::Float(b)) => ValueMagnitude::Float(a + b),
        }
    }
}

impl std::ops::Sub for ValueMagnitude {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) => ValueMagnitude::Int(a - b),
            (ValueMagnitude::Int(a), ValueMagnitude::Float(b)) => {
                ValueMagnitude::Float(a as f64 - b)
            }
            (ValueMagnitude::Float(a), ValueMagnitude::Int(b)) => {
                ValueMagnitude::Float(a - b as f64)
            }
            (ValueMagnitude::Float(a), ValueMagnitude::Float(b)) => ValueMagnitude::Float(a - b),
        }
    }
}

impl std::ops::Neg for ValueMagnitude {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            ValueMagnitude::Int(i) => ValueMagnitude::Int(-i),
            ValueMagnitude::Float(f) => ValueMagnitude::Float(f),
        }
    }
}

impl std::ops::Shl for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, RunnerError>;

    fn shl(self, rhs: Self) -> Self::Output {
        let lhs: u64 = self.try_into()?;
        let rhs: u64 = rhs.try_into()?;

        Ok(ValueMagnitude::Int((lhs << rhs) as i64))
    }
}

impl std::ops::Shr for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, RunnerError>;

    fn shr(self, rhs: Self) -> Self::Output {
        let lhs: u64 = self.try_into()?;
        let rhs: u64 = rhs.try_into()?;

        Ok(ValueMagnitude::Int((lhs >> rhs) as i64))
    }
}

impl std::ops::BitOr for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, RunnerError>;

    fn bitor(self, rhs: Self) -> Self::Output {
        let lhs: u64 = self.try_into()?;
        let rhs: u64 = rhs.try_into()?;

        Ok(ValueMagnitude::Int((lhs | rhs) as i64))
    }
}

impl std::ops::BitAnd for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, RunnerError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        let lhs: u64 = self.try_into()?;
        let rhs: u64 = rhs.try_into()?;

        Ok(ValueMagnitude::Int((lhs & rhs) as i64))
    }
}

impl std::ops::BitXor for SpannedValue<ValueMagnitude> {
    type Output = Result<ValueMagnitude, RunnerError>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let lhs: u64 = self.try_into()?;
        let rhs: u64 = rhs.try_into()?;

        Ok(ValueMagnitude::Int((lhs ^ rhs) as i64))
    }
}
