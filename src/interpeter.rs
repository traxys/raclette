use std::{collections::HashMap, rc::Rc};

use crate::ast;
use gc::{Finalize, Gc, GcCell, Trace};

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
    UndefinedIdentifier(String),
    ParseIntError,
}

type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Trace, Finalize, Debug)]
pub enum Value {
    Number(i64),
    Func(FunctionValue),
    Str(Rc<String>),
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
            &Value::Number(n) => Ok(n),
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
            Value::Str(f) => Ok(f.clone()),
            v => Err(RuntimeError::CastError {
                into: "str".into(),
                from: v.name().into(),
            }),
        }
    }

    pub fn new_number(v: i64) -> Val {
        Self::new(Self::Number(v))
    }

    pub fn new_function(v: FunctionValue) -> Val {
        Self::new(Self::Func(v))
    }

    pub fn new_str(v: String) -> Val {
        Self::new(Self::Str(Rc::new(v)))
    }

    fn new(v: Value) -> Val {
        Gc::new(GcCell::new(v))
    }

    fn name(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::Func(_) => "function",
            Value::Str(_) => "str",
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
        }
    }

    fn run_literal(&mut self, lit: ast::Literal) -> Result<Val> {
        Ok(match lit {
            ast::Literal::Number(n) => Value::new_number(n),
        })
    }

    pub fn display(&mut self, val: Val) -> Result<String> {
        match &*val.borrow() {
            Value::Number(n) => Ok(n.to_string()),
            Value::Func(_) => Ok("<function>".into()),
            Value::Str(s) => Ok(s.to_string()),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
