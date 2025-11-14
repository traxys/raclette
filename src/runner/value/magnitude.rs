use std::fmt::Write;

use malachite::{
    Integer, Natural, Rational,
    base::{
        num::{
            arithmetic::traits::{Abs, Factorial, Pow, RoundToMultiple, Sign},
            basic::traits::{One, Zero},
            comparison::traits::PartialOrdAbs,
            conversion::{
                string::options::ToSciOptions,
                traits::{ExactInto, IsInteger, ToSci, WrappingInto},
            },
        },
        rounding_modes::RoundingMode,
    },
};

use crate::{
    ast::DecimalLiteral,
    runner::{CastError, DisplayConfig, RunnerError},
    span::{NO_SPAN, Span, SpannedValue, SpanningExt},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ValueMagnitude(malachite::Rational);

impl TryFrom<SpannedValue<ValueMagnitude>> for Integer {
    type Error = CastError;

    fn try_from(value: SpannedValue<ValueMagnitude>) -> Result<Self, Self::Error> {
        if value.value.0.is_integer() {
            match value.value.0.sign() {
                std::cmp::Ordering::Less => {
                    let int: Integer = value.value.0.into_numerator().into();
                    Ok(int * Integer::from(-1))
                }
                _ => Ok(value.value.0.into_numerator().into()),
            }
        } else {
            Err(CastError {
                from: value.ty(),
                to: "int",
                location: (value.start..value.end).into(),
                src: value.source,
            })
        }
    }
}

macro_rules! try_from_int {
    ($ty:ty) => {
        impl TryFrom<SpannedValue<ValueMagnitude>> for $ty {
            type Error = CastError;

            fn try_from(value: SpannedValue<ValueMagnitude>) -> Result<Self, Self::Error> {
                let span = value.span();
                let ty = value.ty();
                let integer: Integer = value.try_into()?;

                if (<$ty>::MIN..=<$ty>::MAX).contains(&integer) {
                    Err(CastError {
                        from: ty,
                        to: stringify!($ty),
                        location: (span.start..span.end).into(),
                        src: span.source,
                    })
                } else {
                    Ok((&integer).wrapping_into())
                }
            }
        }
    };
}

try_from_int!(i64);
try_from_int!(i128);
try_from_int!(u32);
try_from_int!(u64);

impl From<i128> for ValueMagnitude {
    fn from(value: i128) -> Self {
        Self::new(value)
    }
}

impl ValueMagnitude {
    pub fn to_string(&self, config: &DisplayConfig) -> String {
        if let Some(thrs) = config.large_threshold
            && self.0 >= thrs
        {
            let precision = thrs.ilog10() as usize;
            let mut options = ToSciOptions::default();
            options.set_precision(precision as u64);

            return self.0.to_sci_with_options(options).to_string();
        }

        match self.0.is_integer() {
            true => self.0.to_string(),
            false => {
                let (int, frac) = self.0.to_digits(&10u8.into());
                let (base, repeat) = frac.slices_ref();
                let mut s = String::with_capacity(int.len());

                if int.is_empty() {
                    s += "0";
                }

                for d in int.iter().rev() {
                    write!(&mut s, "{d}").unwrap();
                }

                s += ".";

                match config.round {
                    Some(n) if base.len() + repeat.len() > n => {
                        s.reserve(n);

                        for d in frac.iter().take(n) {
                            write!(&mut s, "{d}").unwrap();
                        }

                        write!(&mut s, "…").unwrap();
                    }
                    _ => {
                        s.reserve(base.len() + repeat.len() + 3);
                        for d in base.iter() {
                            write!(&mut s, "{d}").unwrap();
                        }
                        for d in repeat.iter() {
                            write!(&mut s, "{d}\u{0332}").unwrap();
                        }
                    }
                }

                s
            }
        }
    }

    pub fn new(value: i128) -> Self {
        Self(value.into())
    }

    pub fn new_decimal(literal: DecimalLiteral) -> Self {
        Self(
            Rational::from(literal.integer)
                + (Rational::from(literal.decimals)
                    / Rational::from(10).pow(literal.decimal_count)),
        )
    }

    pub fn ge_abs(&self, other: &Self) -> bool {
        self.0.ge_abs(&other.0)
    }

    pub fn lt_abs(&self, other: &Self) -> bool {
        self.0.lt_abs(&other.0)
    }

    pub fn ty(&self) -> &'static str {
        match self.0.is_integer() {
            true => "int",
            false => "rational",
        }
    }

    pub fn is_zero(&self) -> bool {
        self.0.eq(&Rational::ZERO)
    }

    pub fn is_usize(&self) -> bool {
        self.0.is_integer() && self.0 >= usize::MIN && self.0 <= usize::MAX
    }

    pub fn as_usize(&self) -> usize {
        assert!(self.is_usize());
        self.0.numerator_ref().exact_into()
    }

    pub fn abs(self) -> Self {
        Self(self.0.abs())
    }

    pub fn round_to_int(self) -> Self {
        Self(
            self.0
                .round_to_multiple(Rational::ONE, RoundingMode::Nearest)
                .0,
        )
    }

    pub fn factorial(v: u64) -> Self {
        Self(Natural::factorial(v).into())
    }

    pub fn pow(
        self,
        _op_span: Span,
        exponent: SpannedValue<Self>,
    ) -> Result<ValueMagnitude, RunnerError> {
        let exponent: i64 = exponent.try_into()?;
        Ok(ValueMagnitude(self.0.pow(exponent)))
    }

    pub fn div(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        if rhs.is_zero() {
            return Err(RunnerError::DivideByZero {
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            });
        }

        Ok(ValueMagnitude(lhs.value.0 / rhs.value.0))
    }

    pub fn div_ok(lhs: Self, rhs: Self) -> Self {
        Self::div(
            NO_SPAN.clone(),
            lhs.spanned(&NO_SPAN),
            rhs.spanned(&NO_SPAN),
        )
        .unwrap()
    }

    pub fn neg(_span: Span, val: SpannedValue<Self>) -> Result<Self, RunnerError> {
        Ok(ValueMagnitude(-val.value.0))
    }

    pub fn shl(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude(lhs.value.0 << rhs))
    }

    pub fn shr(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let rhs: i128 = rhs.try_into()?;

        Ok(ValueMagnitude(lhs.value.0 >> rhs))
    }

    pub fn rem(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs: Integer = lhs.try_into()?;
        let rhs_span = rhs.span();
        let rhs: Integer = rhs.try_into()?;

        if rhs.eq(&0) {
            return Err(RunnerError::DivideByZero {
                location: (rhs_span.start..rhs_span.end).into(),
                src: rhs_span.source,
            });
        }

        Ok(ValueMagnitude((lhs % rhs).into()))
    }

    pub fn bit_or(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs: Integer = lhs.try_into()?;
        let rhs: Integer = rhs.try_into()?;

        Ok(ValueMagnitude((lhs | rhs).into()))
    }

    pub fn bit_and(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs: Integer = lhs.try_into()?;
        let rhs: Integer = rhs.try_into()?;

        Ok(ValueMagnitude((lhs & rhs).into()))
    }

    pub fn bit_xor(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        let lhs: Integer = lhs.try_into()?;
        let rhs: Integer = rhs.try_into()?;

        Ok(ValueMagnitude((lhs ^ rhs).into()))
    }

    pub fn mul(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        Ok(ValueMagnitude(lhs.value.0 * rhs.value.0))
    }

    pub fn mul_ok(lhs: Self, rhs: Self) -> Self {
        Self::mul(
            NO_SPAN.clone(),
            lhs.spanned(&NO_SPAN),
            rhs.spanned(&NO_SPAN),
        )
        .unwrap()
    }

    pub fn add(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        Ok(ValueMagnitude(lhs.value.0 + rhs.value.0))
    }

    pub fn sub(
        _span: Span,
        lhs: SpannedValue<Self>,
        rhs: SpannedValue<Self>,
    ) -> Result<Self, RunnerError> {
        Ok(ValueMagnitude(lhs.value.0 - rhs.value.0))
    }
}
