use std::{collections::HashMap, fmt::Display, fs::File, io::Read, rc::Rc};

use crate::ast;
use gc::{Finalize, Gc, GcCell, Trace};
use serde::{
    de::{MapAccess, Visitor},
    Deserialize,
};

pub struct Interpreter {
    scopes: Vec<Scope>,
    global: Scope,
}

struct Scope {
    vars: HashMap<String, Val>,
}

#[derive(Debug)]
pub enum RuntimeError {
    CastError { into: String, from: String },
    ArgumentCountMismatch { expected: usize, got: usize },
    InvalidNamedArgument { name: String },
    NotHashable { ty: String },
    UndefinedIdentifier(String),
    IoError(std::io::Error),
    JsonError(serde_json::Error),
    ParseIntError,
}

impl From<std::io::Error> for RuntimeError {
    fn from(v: std::io::Error) -> Self {
        Self::IoError(v)
    }
}

impl From<serde_json::Error> for RuntimeError {
    fn from(v: serde_json::Error) -> Self {
        Self::JsonError(v)
    }
}

type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Trace, Finalize, Debug)]
pub enum Value {
    Func(FunctionValue),
    Map(HashMap<HashableValue, Val>),
    Array(Vec<Val>),
    File(#[unsafe_ignore_trace] File),
    Hashable(HashableValue),
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
                Ok(Value::Hashable(HashableValue::Str(Rc::new(value))))
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let mut vec = Vec::new();

                while let Some(v) = seq.next_element()? {
                    vec.push(Value::new(v))
                }

                Ok(Value::Array(vec))
            }

            fn visit_map<V>(self, mut visitor: V) -> Result<Value, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut values = HashMap::new();

                loop {
                    match visitor.next_entry() {
                        Ok(Some((k, v))) => {
                            values.insert(k, Value::new(v));
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
                    write!(f, "{}", v.borrow())?;
                }

                for v in iter {
                    write!(f, ", {}", v.borrow())?;
                }

                write!(f, "]")
            }
            Value::Map(m) => {
                write!(f, "{{")?;
                let mut iter = m.iter();
                if let Some((k, v)) = iter.next() {
                    write!(f, "{} = {}", k, v.borrow())?;
                }

                for (k, v) in iter {
                    write!(f, ", {} = {}", k, v.borrow())?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Trace, Finalize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum HashableValue {
    Str(Rc<String>),
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
                Ok(HashableValue::Str(Rc::new(value)))
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

#[derive(Trace, Finalize, Clone, Debug)]
pub enum FunctionKind {
    Builtin(fn(Vec<Val>, HashMap<String, Val>) -> Result<Val>),
    SpecifyNamed {
        func: Box<FunctionValue>,
        named: HashMap<String, Val>,
    },
}

impl Value {
    pub fn cast_number(&self) -> Result<i64> {
        match self {
            &Value::Hashable(HashableValue::Number(n)) => Ok(n),
            v => Err(RuntimeError::CastError {
                into: "number".into(),
                from: v.name().into(),
            }),
        }
    }

    pub fn cast_function(&self) -> Result<FunctionValue> {
        match self {
            Value::Func(f) => Ok(f.clone()),
            v => Err(RuntimeError::CastError {
                into: "function".into(),
                from: v.name().into(),
            }),
        }
    }

    pub fn cast_str(&self) -> Result<Rc<String>> {
        match self {
            Value::Hashable(HashableValue::Str(f)) => Ok(f.clone()),
            v => Err(RuntimeError::CastError {
                into: "str".into(),
                from: v.name().into(),
            }),
        }
    }

    pub fn cast_hashable(&self) -> Result<HashableValue> {
        match self {
            Value::Hashable(h) => Ok(h.clone()),
            _ => Err(RuntimeError::NotHashable {
                ty: self.name().into(),
            }),
        }
    }

    pub fn new_file(v: File) -> Val {
        Self::new(Self::File(v))
    }

    pub fn new_number(v: i64) -> Val {
        Self::new(Self::Hashable(HashableValue::Number(v)))
    }

    pub fn new_function(v: FunctionValue) -> Val {
        Self::new(Self::Func(v))
    }

    pub fn new_str(v: String) -> Val {
        Self::new(Self::Hashable(HashableValue::Str(Rc::new(v))))
    }

    pub fn new_map(v: HashMap<HashableValue, Val>) -> Val {
        Self::new(Self::Map(v))
    }

    pub fn new_array(v: Vec<Val>) -> Val {
        Self::new(Self::Array(v))
    }

    pub(crate) fn new(v: Value) -> Val {
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
        }
    }
}

impl FunctionValue {
    pub fn call(&self, args: Vec<Val>, mut named: HashMap<String, Val>) -> Result<Val> {
        if args.len() != self.arity {
            return Err(RuntimeError::ArgumentCountMismatch {
                expected: self.arity,
                got: args.len(),
            });
        }

        match &self.action {
            FunctionKind::Builtin(f) => f(args, named),
            FunctionKind::SpecifyNamed {
                func,
                named: already_specified,
            } => {
                for (key, val) in already_specified {
                    if !named.contains_key(key) {
                        named.insert(key.clone(), val.clone());
                    }
                }
                func.call(args, named)
            }
        }
    }
}

pub type Val = Gc<GcCell<Value>>;

impl Interpreter {
    pub fn resolve(&self, name: &str) -> Result<Val> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.vars.get(name) {
                return Ok(v.clone());
            }
        }

        match self.global.vars.get(name) {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError::UndefinedIdentifier(name.into())),
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

    pub fn new() -> Self {
        Self {
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
                match place {
                    ast::Place::Ident(ident) => {
                        self.set(&ident, e);
                    }
                    ast::Place::Deref(_, _) => todo!(),
                }
            }
        };

        Ok(())
    }

    pub fn run_expr(&mut self, expr: ast::Expr) -> Result<Val> {
        match expr {
            ast::Expr::Literal(l) => self.run_literal(l),
            ast::Expr::Binary(op, lhs, rhs) => match op {
                ast::BinaryOp::BitwiseOr | ast::BinaryOp::BitwiseAnd => {
                    let lhs = self.run_expr(*lhs)?.borrow().cast_number()?;
                    let rhs = self.run_expr(*rhs)?.borrow().cast_number()?;
                    let v = match op {
                        ast::BinaryOp::BitwiseOr => lhs | rhs,
                        ast::BinaryOp::BitwiseAnd => lhs & rhs,
                        _ => unreachable!(),
                    };
                    Ok(Value::new_number(v))
                }
                ast::BinaryOp::Redirect => {
                    let input = self.run_expr(*lhs)?;
                    let func = self.run_expr(*rhs)?.borrow().cast_function()?;
                    func.call(vec![input], HashMap::new())
                }
            },
            ast::Expr::Ident(n) => self.resolve(&n),
            ast::Expr::Call { func, args } => {
                let func = self.run_expr(*func)?.borrow().cast_function()?;

                if func.arity != 0 && args.is_empty() {
                    todo!("Short hand for defining a new function")
                } else {
                    let args = args
                        .into_iter()
                        .map(|e| self.run_expr(e))
                        .collect::<Result<_>>()?;
                    func.call(args, HashMap::new())
                }
            }
            ast::Expr::NamedCall { func, named } => {
                let func = Box::new(self.run_expr(*func)?.borrow().cast_function()?);
                let named = named
                    .into_iter()
                    .map(|(key, val)| Ok((key, self.run_expr(val)?)))
                    .collect::<Result<_>>()?;

                Ok(Value::new_function(FunctionValue {
                    arity: func.arity,
                    action: FunctionKind::SpecifyNamed { func, named },
                }))
            }
            ast::Expr::Map(m) => {
                let map: Result<_> = m
                    .into_iter()
                    .map(|(k, v)| -> Result<_> {
                        Ok((
                            self.run_expr(k)?.borrow().cast_hashable()?,
                            self.run_expr(v)?,
                        ))
                    })
                    .collect();
                Ok(Value::new_map(map?))
            }
            ast::Expr::Tilde(v) => {
                let v = self.run_expr(*v)?;
                let str = match &mut *v.borrow_mut() {
                    Value::File(ref mut f) => {
                        let mut s = String::new();
                        f.read_to_string(&mut s)?;
                        s
                    }
                    any => any.to_string(),
                };
                Ok(Value::new_str(str))
            }
            ast::Expr::Array(a) => {
                let vec: Result<_> = a.into_iter().map(|a| self.run_expr(a)).collect();
                Ok(Value::new_array(vec?))
            }
        }
    }

    fn run_literal(&mut self, lit: ast::Literal) -> Result<Val> {
        Ok(match lit {
            ast::Literal::Number(n) => Value::new_number(n),
            ast::Literal::String(s) => Value::new_str(s),
        })
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
