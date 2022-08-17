use std::{
    collections::HashMap,
    fmt::Display,
    fs::File,
    io::{Read, Write},
    process::{ExitStatus, Stdio},
    rc::Rc,
};

use crate::{
    ast,
    span::{
        GcSpannedValue, GenerationSpan, GenerationSpanned, SpannedValue, Spanning, SpanningExt,
        UNKNOWN_SPAN,
    },
};
use bstr::BString;
use gc::{Finalize, Gc, GcCell, Trace};
use miette::{Diagnostic, SourceSpan};
use serde::{
    de::{MapAccess, Visitor},
    Deserialize,
};

pub trait SpannedResult {
    type Ok;

    fn with_span<U, S>(self, span: &S) -> Result<Self::Ok>
    where
        S: Spanning<U>;
}

impl<T> SpannedResult for Result<T> {
    type Ok = T;

    fn with_span<U, S>(self, span: &S) -> Result<T>
    where
        S: Spanning<U>,
    {
        match self {
            Err(e) => Err(e.add_span(span)),
            v => v,
        }
    }
}

pub struct Interpreter {
    generation: u64,
    scopes: Vec<Scope>,
    global: Scope,
}

struct Scope {
    vars: HashMap<String, Val>,
}

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum RuntimeError {
    #[error("Invalid Type. Expected `{into}` got `{from}`")]
    CastError {
        into: String,
        from: String,
        #[label("this value is of type {from}")]
        location: Option<SourceSpan>,
    },
    #[error("Invalid Argument count. Expected {expected} got {got}")]
    ArgumentCountMismatch {
        expected: usize,
        got: usize,
        #[label("this call supplies {got} arguments")]
        location: Option<SourceSpan>,
    },
    #[error("Named argument {name} does not exist")]
    InvalidNamedArgument {
        name: String,
        #[label("this named argument is not valid")]
        location: Option<SourceSpan>,
    },
    #[error("{ty} is not hashable")]
    NotHashable {
        ty: String,
        #[label("value is not hashable")]
        location: Option<SourceSpan>,
    },
    #[error("{ty} is not iterable")]
    NotIterable {
        ty: String,
        #[label("value is not iterable")]
        location: Option<SourceSpan>,
    },
    #[error("Field {field} does not exist")]
    NoSuchField {
        field: String,
        #[label("in this expression")]
        expr_location: Option<SourceSpan>,
        #[label("this field does not exist")]
        location: Option<SourceSpan>,
    },
    #[error("Field {field} is not assignable")]
    FieldNotAssignable {
        field: String,
        #[label("this field can't be assigned")]
        location: Option<SourceSpan>,
    },
    #[error("{ident} is not defined")]
    UndefinedIdentifier {
        ident: String,
        #[label("identifier is not defined")]
        location: Option<SourceSpan>,
    },
    #[error("Could not process utf8")]
    InvalidUtf8 {
        #[label("produced a value that is not utf8")]
        location: Option<SourceSpan>,
    },
    #[error("Io error")]
    IoError {
        #[source]
        err: std::io::Error,
        #[label("returned an IO error")]
        location: Option<SourceSpan>,
    },
    #[error("Subprocess failed with error {status}.\nError output: {error}")]
    ProcessFailure {
        status: ExitStatus,
        error: BString,
        #[label("this expression launched a subprocess")]
        location: Option<SourceSpan>,
    },
    #[error("Value of type `{ty}` is not indexable with values of type `{idx_ty}`")]
    NotIndexableWith {
        ty: String,
        idx_ty: String,
        #[label("This value is of type `{ty}`")]
        location: Option<SourceSpan>,
        #[label("This index is of type `{idx_ty}`")]
        idx_location: Option<SourceSpan>,
    },
    #[error("Index `{idx}` could not index value")]
    NoSuchIndex {
        idx: String,
        #[label("This value is invalid for the indexed expression")]
        location: Option<SourceSpan>,
    },
    #[error("The accessed index is read only")]
    IndexNotAssignable {
        idx: String,
        #[label("This index is read-only")]
        location: Option<SourceSpan>,
    },
    #[error("JSON error")]
    JsonError(#[source] serde_json::Error),
    #[error("Could not parse integer")]
    ParseIntError,
}

impl From<std::io::Error> for RuntimeError {
    fn from(err: std::io::Error) -> Self {
        Self::IoError {
            err,
            location: None,
        }
    }
}

macro_rules! impl_add_span {
    ($($spanning:ident),* $(,)?) => {
        fn add_span<U, S>(self, span: &S) -> Self where S: Spanning<U>{
            match self {
                $(
                    mut v @ RuntimeError::$spanning {..} => {
                        if let RuntimeError::$spanning { ref mut location, .. } = &mut v {
                            if location.is_none() {
                                let span = span.span();
                                *location = Some((span.start..span.end).into());
                            }
                        };
                        v
                    }
                )*
                _ => self,
            }
        }
    };
}

impl RuntimeError {
    impl_add_span! {
        CastError,
        ArgumentCountMismatch,
        InvalidNamedArgument,
        NotHashable,
        NotIterable,
        NoSuchField,
        FieldNotAssignable,
        UndefinedIdentifier,
        IoError,
        InvalidUtf8,
        ProcessFailure,
        NotIndexableWith,
        NoSuchIndex,
    }
}

type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, Trace, Finalize, Clone)]
pub struct RangeIterator {
    start: i64,
    end: Option<i64>,
    step: i64,
    current: i64,
}

impl RangeIterator {
    fn new(start: i64, end: Option<i64>, step: i64) -> Self {
        Self {
            start,
            end,
            step,
            current: start,
        }
    }
}

impl Iterator for RangeIterator {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        match self.end {
            Some(end) if self.current >= end => return None,
            _ => (),
        }
        let v = self.current;
        self.current += self.step;
        Some(v)
    }
}

#[derive(Trace, Finalize, Debug)]
pub enum Value {
    Range(RangeIterator),
    Func(FunctionValue),
    Map(HashMap<HashableValue, Val>),
    Array(Vec<Val>),
    File(#[unsafe_ignore_trace] File),
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
                Ok(Value::Hashable(HashableValue::Str(Rc::from(value))))
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
            Value::Range(r) => match r.end {
                None => write!(f, "{}::{}", r.start, r.step),
                Some(end) => write!(f, "{}:{}:{}", r.start, end, r.step),
            },
        }
    }
}

#[derive(Trace, Finalize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum HashableValue {
    Str(Rc<str>),
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
                Ok(HashableValue::Str(Rc::from(value)))
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

#[derive(Trace, Finalize, Clone, Debug)]
pub struct FunctionValue {
    pub(crate) arity: usize,
    pub(crate) action: FunctionKind,
}

pub type FolderFn = fn(Val, Val, &mut Interpreter, &GenerationSpan) -> Result<Val>;

#[derive(Trace, Finalize, Clone)]
pub enum Folder {
    Op(#[unsafe_ignore_trace] FolderFn, Val),
    User(Val, Val),
}

impl std::fmt::Debug for Folder {
    fn fmt<'a>(&'a self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Op(arg0, arg1) => f
                .debug_tuple("Op")
                .field(
                    &(*arg0 as fn(Val, Val, &'a mut Interpreter, &'a GenerationSpan) -> Result<Val>)
                        as &dyn std::fmt::Debug,
                )
                .field(arg1)
                .finish(),
            Self::User(arg0, arg1) => f.debug_tuple("User").field(arg0).field(arg1).finish(),
        }
    }
}

type BuiltinFn =
    fn(Vec<Val>, HashMap<GcSpannedValue<Rc<str>>, Val>, GenerationSpan, u64) -> Result<Val>;

#[derive(Trace, Finalize, Clone, Debug)]
pub enum FunctionKind {
    Builtin(BuiltinFn),
    Folder(Folder),
    Mapper(Val),
    User {
        args: Vec<String>,
        #[unsafe_ignore_trace]
        ret: SpannedValue<ast::Expr>,
    },
    SpecifyNamed {
        func: Box<FunctionValue>,
        named: HashMap<GcSpannedValue<Rc<str>>, Val>,
    },
    Shell(Rc<str>),
}

#[derive(Copy, Clone)]
enum Num {
    Float(f64),
    Int(i64),
}

impl Num {
    fn f64(self) -> f64 {
        match self {
            Num::Float(f) => f,
            Num::Int(i) => i as f64,
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

    pub fn field(&self, f: Rc<str>) -> Option<Val> {
        match self {
            Value::Map(m) => m.get(&HashableValue::Str(f)).cloned(),
            _ => None,
        }
    }

    pub fn set_field(&mut self, field: SpannedValue<Rc<str>>, value: Val) -> Result<()> {
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

    fn cast_num(&self) -> Result<Num> {
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

    pub fn cast_str(&self) -> Result<Rc<str>> {
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

    fn new_num<U, S>(v: Num, span: &S, gen: u64) -> Val
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
        Self::new(Self::Hashable(HashableValue::Str(Rc::from(v))).spanned_gen(span, gen))
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
        Gc::new(GcCell::new(v))
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

impl FunctionValue {
    pub fn call(
        &self,
        args: Vec<Val>,
        mut named: HashMap<GcSpannedValue<Rc<str>>, Val>,
        scope: &mut Interpreter,
        span: &GenerationSpan,
    ) -> Result<Val> {
        if args.len() != self.arity {
            return Err(RuntimeError::ArgumentCountMismatch {
                expected: self.arity,
                got: args.len(),
                location: None,
            });
        }

        match &self.action {
            FunctionKind::Builtin(f) => f(args, named, span.clone(), scope.generation),
            FunctionKind::SpecifyNamed {
                func,
                named: already_specified,
            } => {
                for (key, val) in already_specified {
                    if !named.contains_key(key) {
                        named.insert(key.clone(), val.clone());
                    }
                }
                func.call(args, named, scope, span)
            }
            FunctionKind::Folder(folder) => {
                let iter = args[0].borrow();
                let mut iter = iter.cast_iterable()?;
                let first = iter.next();
                match folder {
                    Folder::Op(f, def) => iter
                        .try_fold(first.unwrap_or_else(|| def.clone()), |acc, e| {
                            f(acc, e, scope, span)
                        }),
                    Folder::User(f, def) => {
                        let v = f.borrow();
                        let func = v.cast_function()?;
                        iter.try_fold(first.unwrap_or_else(|| def.clone()), |acc, e| {
                            let spn = e.borrow().gen_span();
                            func.call(vec![acc, e], HashMap::new(), scope, &spn)
                        })
                    }
                }
            }
            FunctionKind::Mapper(m) => {
                let iter = args[0].borrow();
                let m = m.borrow();
                let mapper = m.cast_function()?;
                match &**iter {
                    Value::Map(map) => {
                        let mut new_map = map.clone();
                        for (_, v) in new_map.iter_mut() {
                            let spn = &v.borrow().gen_span();
                            *v = mapper.call(vec![v.clone()], HashMap::new(), scope, spn)?;
                        }

                        Ok(Value::new_map(new_map, span, scope.generation))
                    }
                    Value::Array(a) => Ok(Value::new_array(
                        a.iter()
                            .map(|e| {
                                mapper.call(
                                    vec![e.clone()],
                                    HashMap::new(),
                                    scope,
                                    &e.borrow().gen_span(),
                                )
                            })
                            .collect::<Result<_>>()?,
                        span,
                        scope.generation,
                    )),
                    _ => Err(RuntimeError::NotIterable {
                        ty: iter.name().into(),
                        location: None,
                    }),
                }
            }
            FunctionKind::User { args: names, ret } => {
                let s = Scope {
                    vars: names.iter().cloned().zip(args).collect(),
                };

                scope.push_scope(s);
                let ret = scope.run_expr(ret.clone());
                scope.pop_scope();
                ret
            }
            FunctionKind::Shell(cmd) => {
                let arg = args[0].borrow();
                let arg = arg.cast_str().with_span(&arg.span())?;

                let mut command = std::process::Command::new("/bin/sh");
                let mut process = command
                    .arg("-c")
                    .arg(&**cmd)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .stderr(Stdio::piped())
                    .spawn()
                    .map_err(Into::into)
                    .with_span(span)?;

                let stdin = process.stdin.as_mut().unwrap();
                stdin
                    .write_all(arg.as_bytes())
                    .map_err(Into::into)
                    .with_span(span)?;

                let output = process
                    .wait_with_output()
                    .map_err(Into::into)
                    .with_span(span)?;

                if !output.status.success() {
                    return Err(RuntimeError::ProcessFailure {
                        error: output.stderr.into(),
                        status: output.status,
                        location: span.source_span(scope.generation),
                    });
                }

                let output =
                    String::from_utf8(output.stdout).map_err(|_| RuntimeError::InvalidUtf8 {
                        location: span.source_span(scope.generation),
                    })?;

                Ok(Value::new_str(output, span, scope.generation))
            }
        }
    }

    pub fn user_folder(f: Val, def: Val, gen: u64) -> Val {
        let span = def.borrow().span();
        Value::new_function(
            FunctionValue {
                arity: 1,
                action: FunctionKind::Folder(Folder::User(f, def)),
            },
            &span,
            gen,
        )
    }

    pub fn op_folder(f: FolderFn, def: Val, gen: u64) -> Val {
        let span = def.borrow().span();
        Value::new_function(
            FunctionValue {
                arity: 1,
                action: FunctionKind::Folder(Folder::Op(f, def)),
            },
            &span,
            gen,
        )
    }
}

fn power_nums(lhs: Num, rhs: Num) -> Num {
    match (lhs, rhs) {
        (Num::Float(f), Num::Float(e)) => Num::Float(f.powf(e)),
        (Num::Float(f), Num::Int(i)) => Num::Float(f.powi(i as _)),
        (Num::Int(i), Num::Float(e)) => Num::Float((i as f64).powf(e)),
        (Num::Int(i), Num::Int(e)) if e < 0 => Num::Float((i as f64).powi(e as _)),
        (Num::Int(i), Num::Int(e)) => Num::Int(i.pow(e as _)),
    }
}

pub type Val = Gc<GcCell<GenerationSpanned<Value>>>;

macro_rules! int_operator_function {
    ($name:ident, $op:tt) => {
        fn $name(a: Val, b: Val, s: &mut Interpreter, span: &GenerationSpan) -> Result<Val> {
            let a = a.borrow().cast_number()?;
            let b = b.borrow().cast_number()?;

            Ok(Value::new_number(a $op b, span, s.generation))
        }
    };
}

int_operator_function! {bitwise_or, |}
int_operator_function! {bitwise_and, &}
int_operator_function! {int_division, /}
int_operator_function! {left_shift, <<}
int_operator_function! {right_shift, >>}
int_operator_function! {modulo, %}

macro_rules! nums_op {
    ($nums:expr, $op:tt) => {
        match $nums {
            Nums::Float(l, r) => Num::Float(l $op r),
            Nums::Int(l, r) => Num::Int(l $op r),
        }
    };
}

macro_rules! num_operator_function {
    ($name:ident, $op:tt) => {
        fn $name(a: Val, b: Val, s: &mut Interpreter, span: &GenerationSpan) -> Result<Val> {
            let a = a.borrow().cast_num().with_span(&*a.borrow())?;
            let b = b.borrow().cast_num().with_span(&*b.borrow())?;

            Ok(Value::new_num(
                nums_op!((a, b).into(), $op),
                span,
                s.generation,
            ))
        }
    };
}

num_operator_function! {times, *}
num_operator_function! {sum, +}
num_operator_function! {difference, -}

fn power(a: Val, b: Val, s: &mut Interpreter, span: &GenerationSpan) -> Result<Val> {
    let a = a.borrow().cast_num().with_span(&*a.borrow())?;
    let b = b.borrow().cast_num().with_span(&*b.borrow())?;

    Ok(Value::new_num(power_nums(a, b), span, s.generation))
}

fn divide(a: Val, b: Val, s: &mut Interpreter, span: &GenerationSpan) -> Result<Val> {
    let a = a.borrow().cast_num().with_span(&*a.borrow())?;
    let b = b.borrow().cast_num().with_span(&*b.borrow())?;

    Ok(Value::new_float(a.f64() / b.f64(), span, s.generation))
}

fn redirect(a: Val, b: Val, scope: &mut Interpreter, span: &GenerationSpan) -> Result<Val> {
    let b = b.borrow().cast_function()?;

    b.call(vec![a], HashMap::new(), scope, span)
}

enum Nums {
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

impl Interpreter {
    pub fn new_generation(&mut self) {
        self.generation += 1;
    }

    pub fn resolve(&self, name: &str) -> Result<Val> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.vars.get(name) {
                return Ok(v.clone());
            }
        }

        match self.global.vars.get(name) {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError::UndefinedIdentifier {
                ident: name.into(),
                location: None,
            }),
        }
    }

    pub fn set(&mut self, name: &str, val: Val) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(v) = scope.vars.get_mut(name) {
                *v = val;
                return;
            }
        }

        match self.global.vars.get_mut(name) {
            Some(v) => *v = val,
            None => {
                self.scopes
                    .last_mut()
                    .unwrap_or(&mut self.global)
                    .vars
                    .insert(name.to_owned(), val);
            }
        }
    }

    fn push_scope(&mut self, s: Scope) {
        self.scopes.push(s);
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn new() -> Self {
        Self {
            generation: 0,
            scopes: Vec::new(),
            global: Scope {
                vars: crate::builtins::create(),
            },
        }
    }

    pub fn run_statement(&mut self, statement: ast::Statement) -> Result<()> {
        match statement {
            ast::Statement::Expr(e) => {
                self.run_expr(e)?;
            }
            ast::Statement::Assign(place, expr) => {
                let e = self.run_expr(expr)?;
                match place.value {
                    ast::Place::Ident(ident) => {
                        self.set(&ident, e);
                    }
                    ast::Place::Deref(p, field) => {
                        let p = self.run_expr(*p)?;
                        p.borrow_mut().set_field(field, e)?;
                    }
                    ast::Place::Index(se, i) => {
                        let ses = se.with_generation(self.generation);
                        let se = self.run_expr(*se)?;

                        let is = i.with_generation(self.generation);
                        let i = self.run_expr(*i)?;

                        se.borrow_mut()
                            .set_index(i, e, &ses, &is, self.generation)?;
                    }
                }
            }
        };

        Ok(())
    }

    fn parse_num_operands(
        &mut self,
        lhs: SpannedValue<ast::Expr>,
        rhs: SpannedValue<ast::Expr>,
    ) -> Result<Nums> {
        let ls = lhs.span();
        let lhs = self.run_expr(lhs)?.borrow().cast_num().with_span(&ls)?;
        let rs = rhs.span();
        let rhs = self.run_expr(rhs)?.borrow().cast_num().with_span(&rs)?;
        Ok((lhs, rhs).into())
    }

    pub fn run_expr(&mut self, expr: SpannedValue<ast::Expr>) -> Result<Val> {
        let expr_span = &expr.span();
        match expr.value {
            ast::Expr::Literal(l) => self.run_literal(l),
            ast::Expr::Binary(op, lhs, rhs) => match op {
                ast::BinaryOp::BitwiseOr
                | ast::BinaryOp::BitwiseAnd
                | ast::BinaryOp::IntDivide
                | ast::BinaryOp::LShift
                | ast::BinaryOp::RShift
                | ast::BinaryOp::Modulo => {
                    let ls = lhs.span();
                    let lhs = self.run_expr(*lhs)?.borrow().cast_number().with_span(&ls)?;
                    let rs = rhs.span();
                    let rhs = self.run_expr(*rhs)?.borrow().cast_number().with_span(&rs)?;
                    let v = match op {
                        ast::BinaryOp::BitwiseOr => lhs | rhs,
                        ast::BinaryOp::BitwiseAnd => lhs & rhs,
                        ast::BinaryOp::IntDivide => lhs / rhs,
                        ast::BinaryOp::LShift => lhs << rhs,
                        ast::BinaryOp::RShift => lhs >> rhs,
                        ast::BinaryOp::Modulo => lhs % rhs,
                        _ => unreachable!(),
                    };
                    Ok(Value::new_number(v, expr_span, self.generation))
                }
                ast::BinaryOp::Power | ast::BinaryOp::Divide => {
                    let ls = lhs.span();
                    let lhs = self.run_expr(*lhs)?.borrow().cast_num().with_span(&ls)?;
                    let rs = rhs.span();
                    let rhs = self.run_expr(*rhs)?.borrow().cast_num().with_span(&rs)?;

                    match op {
                        ast::BinaryOp::Power => Ok(Value::new_num(
                            power_nums(lhs, rhs),
                            expr_span,
                            self.generation,
                        )),
                        ast::BinaryOp::Divide => Ok(Value::new_float(
                            lhs.f64() / rhs.f64(),
                            expr_span,
                            self.generation,
                        )),
                        _ => unreachable!(),
                    }
                }
                ast::BinaryOp::Plus | ast::BinaryOp::Minus | ast::BinaryOp::Times => {
                    let nums = self.parse_num_operands(*lhs, *rhs)?;
                    let v = match op {
                        ast::BinaryOp::Plus => nums_op!(nums, +),
                        ast::BinaryOp::Minus => nums_op!(nums, -),
                        ast::BinaryOp::Times => nums_op!(nums, *),
                        _ => unreachable!(),
                    };
                    Ok(Value::new_num(v, expr_span, self.generation))
                }
                ast::BinaryOp::Redirect => {
                    let ls = lhs.span();
                    let input = self.run_expr(*lhs).with_span(&ls)?;
                    let f = rhs.span();
                    let func = self
                        .run_expr(*rhs)?
                        .borrow()
                        .cast_function()
                        .with_span(&f)?;
                    func.call(
                        vec![input],
                        HashMap::new(),
                        self,
                        &expr_span.with_generation(self.generation),
                    )
                }
            },
            ast::Expr::Place(p) => match p.value {
                ast::Place::Ident(n) => self.resolve(&n).with_span(&n),
                ast::Place::Deref(e, f) => {
                    let e_span = e.span();
                    let e = self.run_expr(*e)?;
                    let e = e.borrow();
                    e.field(f.value.clone())
                        .ok_or_else(|| RuntimeError::NoSuchField {
                            field: f.to_string(),
                            expr_location: e_span
                                .with_generation(self.generation)
                                .source_span(self.generation),
                            location: None,
                        })
                        .with_span(&f)
                }
                ast::Place::Index(e, i) => {
                    let es = e.span().with_generation(self.generation);
                    let e = self.run_expr(*e)?;
                    let e = e.borrow();

                    let is = i.span().with_generation(self.generation);
                    let i = self.run_expr(*i)?;

                    e.index(i, &es, &is, self.generation)
                }
            },
            ast::Expr::Call { func, args } => {
                let f = func.span();
                let func = self
                    .run_expr(*func)?
                    .borrow()
                    .cast_function()
                    .with_span(&f)?;

                if func.arity != 0 && args.is_empty() {
                    todo!("Short hand for defining a new function")
                } else {
                    let args = args
                        .into_iter()
                        .map(|e| self.run_expr(e))
                        .collect::<Result<_>>()?;
                    func.call(
                        args,
                        HashMap::new(),
                        self,
                        &expr_span.with_generation(self.generation),
                    )
                    .with_span(&f)
                }
            }
            ast::Expr::NamedCall { func, named } => {
                let f = func.span();
                let func = Box::new(
                    self.run_expr(*func)?
                        .borrow()
                        .cast_function()
                        .with_span(&f)?,
                );
                let named = named
                    .into_iter()
                    .map(|(key, val)| Ok((key.into(), self.run_expr(val)?)))
                    .collect::<Result<_>>()?;

                Ok(Value::new_function(
                    FunctionValue {
                        arity: func.arity,
                        action: FunctionKind::SpecifyNamed { func, named },
                    },
                    expr_span,
                    self.generation,
                ))
            }
            ast::Expr::Map(m) => {
                let map: Result<_> = m
                    .into_iter()
                    .map(|(k, v)| -> Result<_> {
                        let ks = k.span();
                        Ok((
                            self.run_expr(k)?.borrow().cast_hashable().with_span(&ks)?,
                            self.run_expr(v)?,
                        ))
                    })
                    .collect();
                Ok(Value::new_map(map?, expr_span, self.generation))
            }
            ast::Expr::Tilde(e) => {
                let span = e.span();
                let v = self.run_expr(*e)?;
                let str = match &mut **v.borrow_mut() {
                    Value::File(ref mut f) => {
                        let mut s = String::new();
                        f.read_to_string(&mut s)
                            .map_err(Into::into)
                            .with_span(&span)?;
                        s
                    }
                    any => any.to_string(),
                };
                Ok(Value::new_str(str, expr_span, self.generation))
            }
            ast::Expr::Array(a) => {
                let vec: Result<_> = a.into_iter().map(|a| self.run_expr(a)).collect();
                Ok(Value::new_array(vec?, expr_span, self.generation))
            }
            ast::Expr::Fold(folder) => match folder.value {
                ast::Folder::Operator(op) => {
                    let (f, def): (FolderFn, _) = match op {
                        ast::BinaryOp::BitwiseOr => {
                            (bitwise_or, Value::new_number(0, expr_span, self.generation))
                        }
                        ast::BinaryOp::BitwiseAnd => (
                            bitwise_and,
                            Value::new_number(-1, expr_span, self.generation),
                        ),
                        ast::BinaryOp::Redirect => {
                            (redirect, Value::new_number(0, expr_span, self.generation))
                        }
                        ast::BinaryOp::Plus => {
                            (sum, Value::new_number(0, expr_span, self.generation))
                        }
                        ast::BinaryOp::Minus => {
                            (difference, Value::new_number(0, expr_span, self.generation))
                        }
                        ast::BinaryOp::Times => {
                            (times, Value::new_number(1, expr_span, self.generation))
                        }
                        ast::BinaryOp::Divide => {
                            (divide, Value::new_number(0, expr_span, self.generation))
                        }
                        ast::BinaryOp::IntDivide => (
                            int_division,
                            Value::new_number(0, expr_span, self.generation),
                        ),
                        ast::BinaryOp::Power => {
                            (power, Value::new_number(0, expr_span, self.generation))
                        }
                        ast::BinaryOp::Modulo => {
                            (modulo, Value::new_number(0, expr_span, self.generation))
                        }
                        ast::BinaryOp::LShift => {
                            (left_shift, Value::new_number(0, expr_span, self.generation))
                        }
                        ast::BinaryOp::RShift => (
                            right_shift,
                            Value::new_number(0, expr_span, self.generation),
                        ),
                    };
                    Ok(FunctionValue::op_folder(f, def, self.generation))
                }
                ast::Folder::Args { func, def } => {
                    let func = self.run_expr(*func)?;
                    let def = self.run_expr(*def)?;
                    Ok(FunctionValue::user_folder(func, def, self.generation))
                }
            },
            ast::Expr::Mapper(m) => {
                let mapper = self.run_expr(*m)?;
                Ok(Value::new_function(
                    FunctionValue {
                        arity: 1,
                        action: FunctionKind::Mapper(mapper),
                    },
                    expr_span,
                    self.generation,
                ))
            }
            ast::Expr::FuncDef { args, ret } => Ok(Value::new_function(
                FunctionValue {
                    arity: args.len(),
                    action: FunctionKind::User { args, ret: *ret },
                },
                expr_span,
                self.generation,
            )),
            ast::Expr::Range { start, end, step } => {
                let ss = start.span();
                let start = self
                    .run_expr(*start)?
                    .borrow()
                    .cast_number()
                    .with_span(&ss)?;
                let end = end
                    .map(|end| {
                        let es = end.span();

                        self.run_expr(*end)?.borrow().cast_number().with_span(&es)
                    })
                    .transpose()?;
                let sts = step.span();
                let step = self
                    .run_expr(*step)?
                    .borrow()
                    .cast_number()
                    .with_span(&sts)?;

                Ok(Value::new_range_iterator(
                    RangeIterator::new(start, end, step),
                    expr_span,
                    self.generation,
                ))
            }
        }
    }

    fn run_literal(&mut self, lit: SpannedValue<ast::Literal>) -> Result<Val> {
        let span = &lit.span();
        Ok(match lit.value {
            ast::Literal::Number(n) => Value::new_number(n, span, self.generation),
            ast::Literal::String(s) => Value::new_str(s, span, self.generation),
        })
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
