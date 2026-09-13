mod magnitude;
mod numeric;
mod unit;

use std::sync::Arc;

use super::{CastError, RunnerError};
use crate::{
    runner::{
        VarSet,
        functions::{self, Function},
    },
    span::{Span, SpannedValue, SpanningExt},
};

pub use magnitude::ValueMagnitude;
pub use numeric::NumericValue;
pub use unit::{
    BYTE_UNIT, KNOWN_UNITS, MASS_UNIT, ScaleRender, ScaleStep, ScaleType, TIME_UNIT, Unit,
};

#[derive(Debug, Clone)]
pub enum Value {
    Numeric(NumericValue),
    Str(String),
    Bool(bool),
    Atom(Arc<str>),
    Config,
    Functions,
    Units,
    Func(Function),
}

impl Value {
    pub fn is_zero(&self) -> bool {
        match self {
            Value::Numeric(n) => n.magnitude.is_zero(),
            Value::Str(_)
            | Value::Atom(_)
            | Value::Bool(_)
            | Value::Config
            | Value::Functions
            | Value::Func(_)
            | Value::Units => false,
        }
    }

    pub fn ty(&self) -> &'static str {
        match self {
            Value::Numeric(_) => "number",
            Value::Str(_) => "str",
            Value::Bool(_) => "bool",
            Value::Atom(_) => "atom",
            Value::Config => "config",
            Value::Functions => "functions",
            Value::Func(_) => "function",
            Value::Units => "units",
        }
    }

    pub(super) fn to_varset(&self) -> VarSet {
        match self {
            Value::Config => VarSet::Config,
            Value::Functions => VarSet::Functions,
            Value::Units => VarSet::Units,
            _ => panic!("value is not a varset: {self:?}"),
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
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(NumericValue::pow(
                span,
                a.spanned(&lhs_span),
                b.spanned(&rhs_span),
            )?)),
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

impl SpannedValue<Value> {
    fn raise_cast<S>(self) -> RunnerError
    where
        S: ValueCast,
    {
        CastError::from_val(self, S::NAME).into()
    }

    pub(crate) fn cast<S: ValueCast>(self) -> Result<S, RunnerError> {
        S::convert(self)
    }
}

impl From<NumericValue> for Value {
    fn from(value: NumericValue) -> Self {
        Self::Numeric(value)
    }
}

pub(crate) trait ValueCast
where
    Self: Sized,
{
    const NAME: &'static str;

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError>;
}

impl ValueCast for bool {
    const NAME: &'static str = "bool";

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        match v.value {
            Value::Bool(b) => Ok(b),
            _ => Err(v.raise_cast::<Self>()),
        }
    }
}

impl ValueCast for SpannedValue<String> {
    const NAME: &'static str = "string";

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        let span = v.span();
        match v.value {
            Value::Str(s) => Ok(s.spanned(&span)),
            _ => Err(v.raise_cast::<Self>()),
        }
    }
}

impl ValueCast for String {
    const NAME: &'static str = <SpannedValue<String> as ValueCast>::NAME;

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        Ok(<SpannedValue<String> as ValueCast>::convert(v)?.value)
    }
}

macro_rules! int_from_value {
    ($ty:ty) => {
        impl ValueCast for $ty {
            const NAME: &'static str = stringify!($ty);

            fn convert(value: SpannedValue<Value>) -> Result<Self, RunnerError> {
                let span = value.span();
                match value.value {
                    Value::Numeric(numeric_value) => {
                        Ok(numeric_value.magnitude.spanned(&span).try_into()?)
                    }
                    _ => Err(value.raise_cast::<Self>()),
                }
            }
        }
    };
}

int_from_value!(u64);
int_from_value!(u32);
int_from_value!(i128);

impl ValueCast for NumericValue {
    const NAME: &'static str = "numeric";

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        match v.value {
            Value::Numeric(n) => Ok(n),
            _ => Err(v.raise_cast::<Self>()),
        }
    }
}

impl ValueCast for VarSet {
    const NAME: &'static str = "map";

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        match v.value {
            Value::Config | Value::Functions | Value::Units => Ok(v.value.to_varset()),
            _ => Err(v.raise_cast::<Self>()),
        }
    }
}

impl ValueCast for functions::Function {
    const NAME: &'static str = "function";

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        match v.value {
            Value::Func(f) => Ok(f),
            _ => Err(v.raise_cast::<Self>()),
        }
    }
}
