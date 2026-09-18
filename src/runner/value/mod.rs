mod magnitude;
mod numeric;
mod unit;

use std::borrow::Cow;

use super::{CastError, RunnerError};
use crate::{
    ast::{Lambda, Variable},
    runner::{VarSet, functions::Function},
    span::{MaybeNamed, Span, SpannedValue, SpanningExt},
};

use arcstr::ArcStr;
pub use magnitude::ValueMagnitude;
pub use numeric::NumericValue;
use serde::{Deserialize, Serialize};
pub use unit::{
    BYTE_UNIT, KNOWN_UNITS, MASS_UNIT, ScaleRender, ScaleStep, ScaleType, TIME_UNIT, Unit,
};

mod builtin_fn {
    use serde::{Deserialize, Deserializer, Serialize, Serializer, de};

    use crate::{
        ast::Variable,
        runner::functions::{self, Function},
    };

    pub fn serialize<S>(&v: &Function, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let (name, _) = functions::FUNCTIONS
            .iter()
            .find(|&(_, &f)| std::ptr::eq(v, f))
            .expect("builtin function not found");

        name.serialize(s)
    }

    pub fn deserialize<'de, D>(de: D) -> Result<Function, D::Error>
    where
        D: Deserializer<'de>,
        D::Error: de::Error,
    {
        let name = Variable::deserialize(de)?;

        Ok(*functions::FUNCTIONS.get(&name).ok_or_else(|| todo!())?)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound = "Src: for<'d> serde::de::Deserialize<'d> + Serialize + Clone")]
pub enum Callable<Src = MaybeNamed> {
    #[serde(with = "builtin_fn")]
    Func(Function),
    Lambda {
        f: Lambda<Src>,
        scope: im::HashMap<Variable, Value<Src>>,
    },
}

impl<Src: Clone> Callable<Src> {
    pub fn map_source<New>(self, mut map: &mut dyn FnMut(Src) -> New) -> Callable<New>
    where
        New: Clone,
    {
        match self {
            Callable::Func(value_fn) => Callable::Func(value_fn),
            Callable::Lambda { f, scope } => Callable::Lambda {
                f: f.map_source(&mut map),
                scope: scope
                    .into_iter()
                    .map(|(n, v)| (n, v.map_source(&mut map)))
                    .collect(),
            },
        }
    }
}

impl Callable {
    pub fn help(&self) -> String {
        match self {
            Callable::Func(f) => f.help(),
            Callable::Lambda { f, .. } => format!("lambda ({} arguments)", f.arguments.len()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound = "Src: for<'d> serde::de::Deserialize<'d> + Serialize + Clone")]
pub enum Value<Src = MaybeNamed> {
    Numeric(NumericValue),
    Str(String),
    Bool(bool),
    Atom(ArcStr),
    Config,
    Functions,
    Units,
    Variables,
    Callable(Callable<Src>),
}

impl<Src: Clone> Value<Src> {
    pub fn map_source<New, F>(self, mut f: F) -> Value<New>
    where
        New: Clone,
        F: FnMut(Src) -> New,
    {
        match self {
            Value::Numeric(numeric_value) => Value::Numeric(numeric_value),
            Value::Str(s) => Value::Str(s),
            Value::Bool(b) => Value::Bool(b),
            Value::Atom(arc_str) => Value::Atom(arc_str),
            Value::Config => Value::Config,
            Value::Functions => Value::Functions,
            Value::Units => Value::Units,
            Value::Variables => Value::Variables,
            Value::Callable(callable) => Value::Callable(callable.map_source(&mut f)),
        }
    }
}

impl Value {
    pub fn help(&self) -> String {
        match self {
            Value::Numeric(_) => "numeric value".into(),
            Value::Str(_) => "string value".into(),
            Value::Bool(_) => "boolean value".into(),
            Value::Atom(_) => "atom".into(),
            Value::Config => "raclette configuration".into(),
            Value::Functions => "available functions".into(),
            Value::Units => "known units".into(),
            Value::Variables => "known variables".into(),
            Value::Callable(c) => c.help(),
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Value::Numeric(n) => n.magnitude.is_zero(),
            Value::Str(_)
            | Value::Atom(_)
            | Value::Bool(_)
            | Value::Config
            | Value::Functions
            | Value::Callable(_)
            | Value::Units
            | Value::Variables => false,
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
            Value::Callable(_) => "function",
            Value::Units => "units",
            Value::Variables => "variables",
        }
    }

    pub(super) fn to_varset(&self) -> VarSet {
        match self {
            Value::Config => VarSet::Config,
            Value::Functions => VarSet::Functions,
            Value::Units => VarSet::Units,
            Value::Variables => VarSet::Base,
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
        CastError::from_val(self, &S::name()).into()
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
    fn name() -> Cow<'static, str>;

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError>;
}

impl<S: ValueCast> ValueCast for SpannedValue<S> {
    fn name() -> Cow<'static, str> {
        S::name()
    }

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        let span = v.span();
        Ok(S::convert(v)?.spanned(&span))
    }
}

impl<S: ValueCast> ValueCast for Option<S> {
    fn name() -> Cow<'static, str> {
        format!(":none | {}", S::name()).into()
    }

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        match &v.value {
            Value::Atom(a) if &**a == "none" => Ok(None),
            _ => S::convert(v).map(Some),
        }
    }
}

impl ValueCast for bool {
    fn name() -> Cow<'static, str> {
        "bool".into()
    }

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        match v.value {
            Value::Bool(b) => Ok(b),
            _ => Err(v.raise_cast::<Self>()),
        }
    }
}

impl ValueCast for String {
    fn name() -> Cow<'static, str> {
        "string".into()
    }

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        match v.value {
            Value::Str(s) => Ok(s),
            _ => Err(v.raise_cast::<Self>()),
        }
    }
}

macro_rules! int_from_value {
    ($ty:ty) => {
        impl ValueCast for $ty {
            fn name() -> Cow<'static, str> {
                stringify!($ty).into()
            }

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
int_from_value!(usize);
int_from_value!(i64);

impl ValueCast for NumericValue {
    fn name() -> Cow<'static, str> {
        "numeric".into()
    }

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        match v.value {
            Value::Numeric(n) => Ok(n),
            _ => Err(v.raise_cast::<Self>()),
        }
    }
}

impl ValueCast for VarSet {
    fn name() -> Cow<'static, str> {
        "map".into()
    }

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        match v.value {
            Value::Config | Value::Functions | Value::Units | Value::Variables => {
                Ok(v.value.to_varset())
            }
            _ => Err(v.raise_cast::<Self>()),
        }
    }
}

impl ValueCast for Callable {
    fn name() -> Cow<'static, str> {
        "function".into()
    }

    fn convert(v: SpannedValue<Value>) -> Result<Self, RunnerError> {
        match v.value {
            Value::Callable(f) => Ok(f),
            _ => Err(v.raise_cast::<Self>()),
        }
    }
}
