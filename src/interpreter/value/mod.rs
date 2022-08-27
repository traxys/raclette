use either::Either;
use serde::{
    de::{MapAccess, Visitor},
    ser::{Error, SerializeMap, SerializeSeq},
    Deserialize, Serialize,
};
use std::{cell::RefCell, collections::HashMap, fmt::Display, fs::File, rc::Rc};

use crate::{
    ast::RcStr,
    span::{Span, SpannedValue, SpanningExt, UNKNOWN_SPAN},
};

use super::{Result, RuntimeError};

pub mod func;

use func::FunctionValue;

pub type Val = Rc<RefCell<SpannedValue<Value>>>;

#[derive(Copy, Clone)]
pub(super) enum Num {
    Float(f64),
    Int(i64),
}

impl Num {
    pub fn f64(self) -> f64 {
        match self {
            Num::Float(f) => f,
            Num::Int(i) => i as f64,
        }
    }
}

pub(super) enum Nums {
    Float(f64, f64),
    Int(i64, i64),
}

impl From<(Num, Num)> for Nums {
    fn from((lhs, rhs): (Num, Num)) -> Self {
        match (lhs, rhs) {
            (Num::Float(l), Num::Float(r)) => Nums::Float(l, r),
            (Num::Float(l), Num::Int(r)) => Nums::Float(l, r as f64),
            (Num::Int(l), Num::Float(r)) => Nums::Float(l as f64, r),
            (Num::Int(l), Num::Int(r)) => Nums::Int(l, r),
        }
    }
}

pub(super) fn power_nums(lhs: Num, rhs: Num) -> Num {
    match (lhs, rhs) {
        (Num::Float(f), Num::Float(e)) => Num::Float(f.powf(e)),
        (Num::Float(f), Num::Int(i)) => Num::Float(f.powi(i as _)),
        (Num::Int(i), Num::Float(e)) => Num::Float((i as f64).powf(e)),
        (Num::Int(i), Num::Int(e)) if e < 0 => Num::Float((i as f64).powi(e as _)),
        (Num::Int(i), Num::Int(e)) => Num::Int(i.pow(e as _)),
    }
}

type RangeIterator = std::iter::StepBy<Either<std::ops::Range<i64>, std::ops::RangeFrom<i64>>>;

#[derive(Debug)]
pub enum Value {
    Range(RangeIterator),
    Func(FunctionValue),
    Map(HashMap<HashableValue, Val>),
    Array(Vec<Val>),
    File(File),
    Hashable(HashableValue),
    Float(f64),
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Value::Map(m) => {
                let mut map = serializer.serialize_map(Some(m.len()))?;
                for (k, v) in m {
                    map.serialize_entry(k, &**v.borrow())?;
                }
                map.end()
            }
            Value::Hashable(v) => v.serialize(serializer),
            &Value::Float(f) => serializer.serialize_f64(f),
            Value::Array(s) => {
                let mut seq = serializer.serialize_seq(Some(s.len()))?;
                for e in s {
                    seq.serialize_element(&**e.borrow())?;
                }
                seq.end()
            }
            _ => Err(S::Error::custom(format!(
                "`{}` is not serializable",
                self.name()
            ))),
        }
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ValueVisitor;

        type Result<T, E> = std::result::Result<T, E>;

        impl<'de> Visitor<'de> for ValueVisitor {
            type Value = Value;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("any valid self describing value")
            }

            fn visit_i64<E>(self, value: i64) -> Result<Value, E> {
                Ok(Value::Hashable(HashableValue::Number(value)))
            }

            fn visit_u64<E>(self, value: u64) -> Result<Value, E> {
                Ok(Value::Hashable(HashableValue::Number(value as i64)))
            }

            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::Hashable(HashableValue::Null))
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::Hashable(HashableValue::Bool(v)))
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::Float(v))
            }

            fn visit_str<E: serde::de::Error>(self, value: &str) -> Result<Value, E> {
                self.visit_string(String::from(value))
            }

            fn visit_string<E>(self, value: String) -> Result<Value, E> {
                Ok(Value::Hashable(HashableValue::Str(RcStr(Rc::from(value)))))
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let mut vec = Vec::new();

                while let Some(v) = seq.next_element::<Value>()? {
                    vec.push(Value::new(v.spanned(&*UNKNOWN_SPAN)))
                }

                Ok(Value::Array(vec))
            }

            fn visit_map<V>(self, mut visitor: V) -> Result<Value, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut values = HashMap::new();

                loop {
                    match visitor.next_entry::<_, Value>() {
                        Ok(Some((k, v))) => {
                            values.insert(k, Value::new(v.spanned(&*UNKNOWN_SPAN)));
                        }
                        Ok(None) => break,
                        Err(e) => return Err(e),
                    }
                }

                Ok(Value::Map(values))
            }
        }

        deserializer.deserialize_any(ValueVisitor)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Func(_) => write!(f, "<function>"),
            Value::File(_) => write!(f, "<file>"),
            Value::Hashable(h) => write!(f, "{}", h),
            Value::Array(a) => {
                write!(f, "[")?;

                let mut iter = a.iter();
                if let Some(v) = iter.next() {
                    write!(f, "{}", **v.borrow())?;
                }

                for v in iter {
                    write!(f, ", {}", **v.borrow())?;
                }

                write!(f, "]")
            }
            Value::Map(m) => {
                write!(f, "{{")?;
                let mut iter = m.iter();
                if let Some((k, v)) = iter.next() {
                    write!(f, "{} = {}", k, **v.borrow())?;
                }

                for (k, v) in iter {
                    write!(f, ", {} = {}", k, **v.borrow())?;
                }
                write!(f, "}}")
            }
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Range(_) => write!(f, "<range>"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum HashableValue {
    Str(RcStr),
    Number(i64),
    Null,
    Bool(bool),
}

impl Serialize for HashableValue {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            HashableValue::Str(v) => serializer.serialize_str(v),
            &HashableValue::Number(n) => serializer.serialize_i64(n),
            HashableValue::Null => serializer.serialize_none(),
            &HashableValue::Bool(b) => serializer.serialize_bool(b),
        }
    }
}

impl<'de> Deserialize<'de> for HashableValue {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct HashableValueVisitor;

        type Result<T, E> = std::result::Result<T, E>;

        impl<'de> Visitor<'de> for HashableValueVisitor {
            type Value = HashableValue;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("expected self-describing hashable value")
            }

            fn visit_i64<E>(self, value: i64) -> Result<HashableValue, E> {
                Ok(HashableValue::Number(value))
            }

            fn visit_u64<E>(self, value: u64) -> Result<HashableValue, E> {
                Ok(HashableValue::Number(value as i64))
            }

            fn visit_str<E: serde::de::Error>(self, value: &str) -> Result<HashableValue, E> {
                self.visit_string(String::from(value))
            }

            fn visit_string<E>(self, value: String) -> Result<HashableValue, E> {
                Ok(HashableValue::Str(RcStr(Rc::from(value))))
            }

            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(HashableValue::Null)
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(HashableValue::Bool(v))
            }
        }

        deserializer.deserialize_any(HashableValueVisitor)
    }
}

impl Display for HashableValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HashableValue::Str(s) => write!(f, "{}", s),
            HashableValue::Number(n) => write!(f, "{}", n),
            HashableValue::Null => write!(f, "Null"),
            HashableValue::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl Value {
    pub fn index(&self, idx: Val, expr_span: &Span, index_span: &Span) -> Result<Val> {
        match (self, &**idx.borrow()) {
            (Value::Array(a), &Value::Hashable(HashableValue::Number(n))) => {
                if (n >= 0 && n as usize >= a.len()) || (n < 0 && (-n) as usize > a.len()) {
                    return Err(RuntimeError::no_such_index(n.to_string(), index_span));
                }

                if n >= 0 {
                    Ok(a[n as usize].clone())
                } else {
                    Ok(a[a.len() - (-n) as usize].clone())
                }
            }
            (Value::Range(r), &Value::Hashable(HashableValue::Number(n))) => {
                if n < 0 {
                    return Err(RuntimeError::no_such_index(n.to_string(), index_span));
                }

                match r.clone().nth(n as usize) {
                    Some(v) => Ok(Value::new_number(v, expr_span)),
                    None => Err(RuntimeError::no_such_index(n.to_string(), index_span)),
                }
            }
            (Value::Map(m), Value::Hashable(h)) => match m.get(h) {
                None => Err(RuntimeError::no_such_index(h.to_string(), index_span)),
                Some(v) => Ok(v.clone()),
            },
            (e, i) => Err(RuntimeError::not_indexable_with(
                e.name().into(),
                i.name().into(),
                index_span.into(),
                expr_span,
            )),
        }
    }

    pub fn set_index(
        &mut self,
        idx: Val,
        value: Val,
        expr_span: &Span,
        index_span: &Span,
    ) -> Result<()> {
        match (self, &**idx.borrow()) {
            (Value::Array(a), &Value::Hashable(HashableValue::Number(n))) => {
                if (n >= 0 && n as usize >= a.len()) || (n < 0 && -n as usize > a.len()) {
                    return Err(RuntimeError::no_such_index(n.to_string(), index_span));
                }

                if n >= 0 {
                    a[n as usize] = value;
                } else {
                    let l = a.len();
                    a[l - (-n) as usize] = value;
                }

                Ok(())
            }
            (Value::Range(_), &Value::Hashable(HashableValue::Number(n))) => {
                if n < 0 {
                    return Err(RuntimeError::no_such_index(n.to_string(), index_span));
                }

                Err(RuntimeError::index_not_assignable(
                    n.to_string(),
                    index_span,
                ))
            }
            (Value::Map(m), Value::Hashable(h)) => {
                m.insert(h.clone(), value);

                Ok(())
            }
            (e, i) => Err(RuntimeError::not_indexable_with(
                e.name().into(),
                i.name().into(),
                expr_span.into(),
                index_span,
            )),
        }
    }

    pub fn field(&self, f: RcStr) -> Option<Val> {
        match self {
            Value::Map(m) => m.get(&HashableValue::Str(f)).cloned(),
            _ => None,
        }
    }

    pub fn set_field(&mut self, field: SpannedValue<RcStr>, value: Val) -> Result<()> {
        match self {
            Value::Map(m) => {
                m.insert(HashableValue::Str(field.value), value);
                Ok(())
            }
            _ => Err(RuntimeError::field_not_assignable(
                field.value.to_string(),
                &field,
            )),
        }
    }

    pub fn cast_slice<U>(&self, span: &SpannedValue<U>) -> Result<&[Val]> {
        match self {
            Value::Array(a) => Ok(a),
            v => Err(RuntimeError::cast_error(
                "array".into(),
                v.name().into(),
                span,
            )),
        }
    }

    pub(super) fn cast_num<U>(&self, span: &SpannedValue<U>) -> Result<Num> {
        match self {
            &Value::Hashable(HashableValue::Number(n)) => Ok(Num::Int(n)),
            &Value::Float(f) => Ok(Num::Float(f)),
            v => Err(RuntimeError::cast_error(
                "num".into(),
                v.name().into(),
                span,
            )),
        }
    }

    pub fn cast_number<U>(&self, span: &SpannedValue<U>) -> Result<i64> {
        match self {
            &Value::Hashable(HashableValue::Number(n)) => Ok(n),
            v => Err(RuntimeError::cast_error(
                "number".into(),
                v.name().into(),
                span,
            )),
        }
    }

    pub fn cast_function<U>(&self, span: &SpannedValue<U>) -> Result<FunctionValue> {
        match self {
            Value::Func(f) => Ok(f.clone()),
            v => Err(RuntimeError::cast_error(
                "function".into(),
                v.name().into(),
                span,
            )),
        }
    }

    pub fn cast_str<U>(&self, span: &SpannedValue<U>) -> Result<RcStr> {
        match self {
            Value::Hashable(HashableValue::Str(f)) => Ok(f.clone()),
            v => Err(RuntimeError::cast_error(
                "str".into(),
                v.name().into(),
                span,
            )),
        }
    }

    pub fn cast_hashable<U>(&self, span: &SpannedValue<U>) -> Result<HashableValue> {
        match self {
            Value::Hashable(h) => Ok(h.clone()),
            _ => Err(RuntimeError::not_hashable(self.name().into(), span)),
        }
    }

    pub fn cast_iterable<U>(
        &self,
        span: &SpannedValue<U>,
    ) -> Result<Box<dyn Iterator<Item = Val> + '_>> {
        match self {
            Value::Map(v) => Ok(Box::new(v.values().cloned())),
            Value::Array(a) => Ok(Box::new(a.iter().cloned())),
            Value::Range(r) => Ok(Box::new(
                r.clone().map(|v| Value::new_number(v, &*UNKNOWN_SPAN)),
            )),
            _ => Err(RuntimeError::not_iterable(self.name().into(), span)),
        }
    }

    fn int_iterable(&self) -> Option<impl Iterator<Item = i64> + '_> {
        match self {
            Value::Range(r) => Some(r.clone()),
            _ => None,
        }
    }

    pub(in crate::interpreter) fn new_num<U>(v: Num, span: &SpannedValue<U>) -> Val {
        match v {
            Num::Float(f) => Self::new_float(f, span),
            Num::Int(i) => Self::new_number(i, span),
        }
    }

    pub fn new_range_iterator<U>(v: RangeIterator, span: &SpannedValue<U>) -> Val {
        Self::new(Self::Range(v).spanned(span))
    }

    pub fn new_file<U>(v: File, span: &SpannedValue<U>) -> Val {
        Self::new(Self::File(v).spanned(span))
    }

    pub fn new_float<U>(v: f64, span: &SpannedValue<U>) -> Val {
        Self::new(Self::Float(v).spanned(span))
    }

    pub fn new_number<U>(v: i64, span: &SpannedValue<U>) -> Val {
        Self::new(Self::Hashable(HashableValue::Number(v)).spanned(span))
    }

    pub fn new_function<U>(v: FunctionValue, span: &SpannedValue<U>) -> Val {
        Self::new(Self::Func(v).spanned(span))
    }

    pub fn new_str<U>(v: String, span: &SpannedValue<U>) -> Val {
        Self::new(Self::Hashable(HashableValue::Str(RcStr(Rc::from(v)))).spanned(span))
    }

    pub fn new_map<U>(v: HashMap<HashableValue, Val>, span: &SpannedValue<U>) -> Val {
        Self::new(Self::Map(v).spanned(span))
    }

    pub fn new_array<U>(v: Vec<Val>, span: &SpannedValue<U>) -> Val {
        Self::new(Self::Array(v).spanned(span))
    }

    pub(crate) fn new(v: SpannedValue<Value>) -> Val {
        Rc::new(RefCell::new(v))
    }

    fn respan_in_place(&mut self, span: &Span) {
        match self {
            Value::Range(_) => (),
            Value::Func(_) => (),
            Value::File(_) => (),
            Value::Float(_) => (),
            Value::Hashable(_) => (),
            Value::Map(m) => {
                for v in m.values() {
                    let mut v = v.borrow_mut();
                    v.respan_in_place(span);
                    v.swap_span(span);
                }
            }
            Value::Array(a) => {
                for v in a.iter() {
                    let mut v = v.borrow_mut();
                    v.respan_in_place(span);
                    v.swap_span(span);
                }
            }
        }
    }

    pub(crate) fn respan(self, span: &Span) -> SpannedValue<Value> {
        let value = match self {
            Value::Range(_) => self,
            Value::Func(_) => self,
            Value::File(_) => self,
            Value::Hashable(_) => self,
            Value::Float(_) => self,
            Value::Map(m) => Value::Map(
                m.into_iter()
                    .map(|(k, v)| {
                        (k, {
                            v.borrow_mut().respan_in_place(span);
                            v
                        })
                    })
                    .collect(),
            ),
            Value::Array(a) => Value::Array(
                a.into_iter()
                    .map(|v| {
                        v.borrow_mut().respan_in_place(span);
                        v
                    })
                    .collect(),
            ),
        };
        value.spanned(span)
    }

    pub(crate) fn name(&self) -> &'static str {
        match self {
            Value::Hashable(HashableValue::Number(_)) => "number",
            Value::Hashable(HashableValue::Str(_)) => "str",
            Value::Hashable(HashableValue::Null) => "null",
            Value::Hashable(HashableValue::Bool(_)) => "bool",
            Value::Map(_) => "map",
            Value::Func(_) => "function",
            Value::File(_) => "file",
            Value::Array(_) => "array",
            Value::Float(_) => "float",
            Value::Range(_) => "range",
        }
    }
}
