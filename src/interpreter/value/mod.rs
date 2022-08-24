use either::Either;
use serde::{
    de::{MapAccess, Visitor},
    Deserialize,
};
use std::{cell::RefCell, collections::HashMap, fmt::Display, fs::File, rc::Rc};

use crate::{
    ast::RcStr,
    span::{GenerationSpan, GenerationSpanned, SpannedValue, Spanning, SpanningExt, UNKNOWN_SPAN},
};

use super::{Result, RuntimeError, SpannedResult};

pub mod func;

use func::FunctionValue;

pub type Val = Rc<RefCell<GenerationSpanned<Value>>>;

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
                    vec.push(Value::new(v.spanned_gen(UNKNOWN_SPAN, 0)))
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
                            values.insert(k, Value::new(v.spanned_gen(UNKNOWN_SPAN, 0)));
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
        }

        deserializer.deserialize_any(HashableValueVisitor)
    }
}

impl Display for HashableValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HashableValue::Str(s) => write!(f, "{}", s),
            HashableValue::Number(n) => write!(f, "{}", n),
        }
    }
}

impl Value {
    pub fn index(
        &self,
        idx: Val,
        expr_span: &GenerationSpan,
        index_span: &GenerationSpan,
        gen: u64,
    ) -> Result<Val> {
        match (self, &**idx.borrow()) {
            (Value::Array(a), &Value::Hashable(HashableValue::Number(n))) => {
                if (n >= 0 && n as usize >= a.len()) || (n < 0 && (-n) as usize > a.len()) {
                    return Err(RuntimeError::NoSuchIndex {
                        idx: n.to_string(),
                        location: index_span.source_span(gen),
                    });
                }

                if n >= 0 {
                    Ok(a[n as usize].clone())
                } else {
                    Ok(a[a.len() - (-n) as usize].clone())
                }
            }
            (Value::Range(r), &Value::Hashable(HashableValue::Number(n))) => {
                if n < 0 {
                    return Err(RuntimeError::NoSuchIndex {
                        idx: n.to_string(),
                        location: index_span.source_span(gen),
                    });
                }

                match r.clone().nth(n as usize) {
                    Some(v) => Ok(Value::new_number(v, expr_span, gen)),
                    None => Err(RuntimeError::NoSuchIndex {
                        idx: n.to_string(),
                        location: index_span.source_span(gen),
                    }),
                }
            }
            (Value::Map(m), Value::Hashable(h)) => match m.get(h) {
                None => Err(RuntimeError::NoSuchIndex {
                    idx: h.to_string(),
                    location: index_span.source_span(gen),
                }),
                Some(v) => Ok(v.clone()),
            },
            (e, i) => Err(RuntimeError::NotIndexableWith {
                ty: e.name().into(),
                idx_ty: i.name().into(),
                location: expr_span.source_span(gen),
                idx_location: index_span.source_span(gen),
            }),
        }
    }

    pub fn set_index(
        &mut self,
        idx: Val,
        value: Val,
        expr_span: &GenerationSpan,
        index_span: &GenerationSpan,
        gen: u64,
    ) -> Result<()> {
        match (self, &**idx.borrow()) {
            (Value::Array(a), &Value::Hashable(HashableValue::Number(n))) => {
                if (n >= 0 && n as usize >= a.len()) || (n < 0 && -n as usize > a.len()) {
                    return Err(RuntimeError::NoSuchIndex {
                        idx: n.to_string(),
                        location: index_span.source_span(gen),
                    });
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
                    return Err(RuntimeError::NoSuchIndex {
                        idx: n.to_string(),
                        location: index_span.source_span(gen),
                    });
                }

                Err(RuntimeError::IndexNotAssignable {
                    idx: n.to_string(),
                    location: index_span.source_span(gen),
                })
            }
            (Value::Map(m), Value::Hashable(h)) => {
                m.insert(h.clone(), value);

                Ok(())
            }
            (e, i) => Err(RuntimeError::NotIndexableWith {
                ty: e.name().into(),
                idx_ty: i.name().into(),
                location: expr_span.source_span(gen),
                idx_location: index_span.source_span(gen),
            }),
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
            _ => Err(RuntimeError::FieldNotAssignable {
                field: field.value.to_string(),
                location: None,
            })
            .with_span(&field),
        }
    }

    pub fn cast_slice(&self) -> Result<&[Val]> {
        match self {
            Value::Array(a) => Ok(a),
            v => Err(RuntimeError::CastError {
                into: "array".into(),
                from: v.name().into(),
                location: None,
            }),
        }
    }

    pub(super) fn cast_num(&self) -> Result<Num> {
        match self {
            &Value::Hashable(HashableValue::Number(n)) => Ok(Num::Int(n)),
            &Value::Float(f) => Ok(Num::Float(f)),
            v => Err(RuntimeError::CastError {
                into: "num".into(),
                from: v.name().into(),
                location: None,
            }),
        }
    }

    pub fn cast_number(&self) -> Result<i64> {
        match self {
            &Value::Hashable(HashableValue::Number(n)) => Ok(n),
            v => Err(RuntimeError::CastError {
                into: "number".into(),
                from: v.name().into(),
                location: None,
            }),
        }
    }

    pub fn cast_function(&self) -> Result<FunctionValue> {
        match self {
            Value::Func(f) => Ok(f.clone()),
            v => Err(RuntimeError::CastError {
                into: "function".into(),
                from: v.name().into(),
                location: None,
            }),
        }
    }

    pub fn cast_str(&self) -> Result<RcStr> {
        match self {
            Value::Hashable(HashableValue::Str(f)) => Ok(f.clone()),
            v => Err(RuntimeError::CastError {
                into: "str".into(),
                from: v.name().into(),
                location: None,
            }),
        }
    }

    pub fn cast_hashable(&self) -> Result<HashableValue> {
        match self {
            Value::Hashable(h) => Ok(h.clone()),
            _ => Err(RuntimeError::NotHashable {
                ty: self.name().into(),
                location: None,
            }),
        }
    }

    pub fn cast_iterable(&self) -> Result<Box<dyn Iterator<Item = Val> + '_>> {
        match self {
            Value::Map(v) => Ok(Box::new(v.values().cloned())),
            Value::Array(a) => Ok(Box::new(a.iter().cloned())),
            Value::Range(r) => Ok(Box::new(
                r.clone().map(|v| Value::new_number(v, UNKNOWN_SPAN, 0)),
            )),
            _ => Err(RuntimeError::NotIterable {
                ty: self.name().into(),
                location: None,
            }),
        }
    }

    fn int_iterable(&self) -> Option<impl Iterator<Item = i64> + '_> {
        match self {
            Value::Range(r) => Some(r.clone()),
            _ => None,
        }
    }

    pub(in crate::interpreter) fn new_num<U, S>(v: Num, span: &S, gen: u64) -> Val
    where
        S: Spanning<U>,
    {
        match v {
            Num::Float(f) => Self::new_float(f, span, gen),
            Num::Int(i) => Self::new_number(i, span, gen),
        }
    }

    pub fn new_range_iterator<U, S>(v: RangeIterator, span: &S, gen: u64) -> Val
    where
        S: Spanning<U>,
    {
        Self::new(Self::Range(v).spanned_gen(span, gen))
    }

    pub fn new_file<U, S>(v: File, span: &S, gen: u64) -> Val
    where
        S: Spanning<U>,
    {
        Self::new(Self::File(v).spanned_gen(span, gen))
    }

    pub fn new_float<U, S>(v: f64, span: &S, gen: u64) -> Val
    where
        S: Spanning<U>,
    {
        Self::new(Self::Float(v).spanned_gen(span, gen))
    }

    pub fn new_number<U, S>(v: i64, span: &S, gen: u64) -> Val
    where
        S: Spanning<U>,
    {
        Self::new(Self::Hashable(HashableValue::Number(v)).spanned_gen(span, gen))
    }

    pub fn new_function<U, S>(v: FunctionValue, span: &S, gen: u64) -> Val
    where
        S: Spanning<U>,
    {
        Self::new(Self::Func(v).spanned_gen(span, gen))
    }

    pub fn new_str<U, S>(v: String, span: &S, gen: u64) -> Val
    where
        S: Spanning<U>,
    {
        Self::new(Self::Hashable(HashableValue::Str(RcStr(Rc::from(v)))).spanned_gen(span, gen))
    }

    pub fn new_map<U, S>(v: HashMap<HashableValue, Val>, span: &S, gen: u64) -> Val
    where
        S: Spanning<U>,
    {
        Self::new(Self::Map(v).spanned_gen(span, gen))
    }

    pub fn new_array<U, S>(v: Vec<Val>, span: &S, gen: u64) -> Val
    where
        S: Spanning<U>,
    {
        Self::new(Self::Array(v).spanned_gen(span, gen))
    }

    pub(crate) fn new(v: GenerationSpanned<Value>) -> Val {
        Rc::new(RefCell::new(v))
    }

    pub(crate) fn name(&self) -> &'static str {
        match self {
            Value::Hashable(HashableValue::Number(_)) => "number",
            Value::Hashable(HashableValue::Str(_)) => "str",
            Value::Map(_) => "map",
            Value::Func(_) => "function",
            Value::File(_) => "file",
            Value::Array(_) => "array",
            Value::Float(_) => "float",
            Value::Range(_) => "range",
        }
    }
}
