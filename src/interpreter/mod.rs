use std::{collections::HashMap, fmt::Debug, io::Read, process::ExitStatus};

use crate::{
    ast::{self, RangeExpr},
    span::{SpannedValue, Spanning},
};
use bstr::BString;
use either::Either;
use miette::{Diagnostic, SourceSpan};

mod builtins;
pub mod value;

use value::{
    func::{folder::*, FunctionKind, FunctionValue},
    power_nums, Val, Value,
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
    #[error("The range is reversed")]
    ReversedRange {
        #[label("This range has `start > end`")]
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
        ReversedRange,
    }
}

type Result<T> = std::result::Result<T, RuntimeError>;

macro_rules! nums_op {
    ($nums:expr, $op:tt) => {
        match $nums {
            value::Nums::Float(l, r) => value::Num::Float(l $op r),
            value::Nums::Int(l, r) => value::Num::Int(l $op r),
        }
    };
}

pub struct Interpreter {
    generation: u64,
    scopes: Vec<Scope>,
    global: Scope,
}

struct Scope {
    vars: HashMap<String, Val>,
}

type RangeIteratorDef = (
    Either<std::ops::Range<i64>, std::ops::RangeFrom<i64>>,
    usize,
);

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
                vars: builtins::create(),
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
    ) -> Result<value::Nums> {
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
                    .map(|(key, val)| Ok((key, self.run_expr(val)?)))
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
                    let folder = match op {
                        ast::BinaryOp::BitwiseOr => BitwiseOrFolder::start(),
                        ast::BinaryOp::BitwiseAnd => BitwiseAndFolder::start(),
                        ast::BinaryOp::Redirect => RedirectFolder::start(),
                        ast::BinaryOp::Plus => SumFolder::start(),
                        ast::BinaryOp::Minus => DifferenceFolder::start(),
                        ast::BinaryOp::Times => TimesFolder::start(),
                        ast::BinaryOp::Divide => DivideFolder::start(),
                        ast::BinaryOp::IntDivide => IntDivideFolder::start(),
                        ast::BinaryOp::Power => PowerFolder::start(),
                        ast::BinaryOp::Modulo => ModuloFolder::start(),
                        ast::BinaryOp::LShift => LeftShiftFolder::start(),
                        ast::BinaryOp::RShift => RightShiftFolder::start(),
                    };
                    Ok(FunctionValue::op_folder(folder, expr_span, self.generation))
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
            ast::Expr::Range(r) => {
                let (range, step) = self.run_range(*r)?;
                Ok(Value::new_range_iterator(
                    range.step_by(step),
                    expr_span,
                    self.generation,
                ))
            }
            ast::Expr::Slice { iterable, slice } => {
                let is = iterable.span();
                let iterable = self.run_expr(*iterable)?;
                let it = iterable.borrow();
                let arr = it.cast_slice().with_span(&is)?;

                let rs = slice.span();

                let (range, step) = self.run_range(slice.value)?;

                match range {
                    Either::Left(bounded) => {
                        if bounded.start > bounded.end {
                            return Err(RuntimeError::ReversedRange {
                                location: rs
                                    .with_generation(self.generation)
                                    .source_span(self.generation),
                            });
                        }

                        if bounded.start < 0 || bounded.start as usize >= arr.len() {
                            return Err(RuntimeError::NoSuchIndex {
                                idx: bounded.start.to_string(),
                                location: rs
                                    .with_generation(self.generation)
                                    .source_span(self.generation),
                            });
                        }

                        if bounded.end < 0 || bounded.end as usize >= arr.len() {
                            return Err(RuntimeError::NoSuchIndex {
                                idx: bounded.end.to_string(),
                                location: rs
                                    .with_generation(self.generation)
                                    .source_span(self.generation),
                            });
                        }

                        Ok(Value::new_array(
                            arr[bounded.start as usize..bounded.end as usize]
                                .iter()
                                .step_by(step)
                                .cloned()
                                .collect(),
                            expr_span,
                            self.generation,
                        ))
                    }
                    Either::Right(unbounded) => {
                        if unbounded.start < 0 || unbounded.start as usize >= arr.len() {
                            return Err(RuntimeError::NoSuchIndex {
                                idx: unbounded.start.to_string(),
                                location: rs
                                    .with_generation(self.generation)
                                    .source_span(self.generation),
                            });
                        }

                        Ok(Value::new_array(
                            arr[unbounded.start as usize..]
                                .iter()
                                .step_by(step)
                                .cloned()
                                .collect(),
                            expr_span,
                            self.generation,
                        ))
                    }
                }
            }
        }
    }

    fn run_range(&mut self, range: RangeExpr) -> Result<RangeIteratorDef> {
        let RangeExpr { start, end, step } = range;
        let ss = start.span();
        let start = self
            .run_expr(start)?
            .borrow()
            .cast_number()
            .with_span(&ss)?;
        let end = end
            .map(|end| {
                let es = end.span();

                self.run_expr(end)?.borrow().cast_number().with_span(&es)
            })
            .transpose()?;
        let sts = step.span();
        let step = self
            .run_expr(step)?
            .borrow()
            .cast_number()
            .with_span(&sts)?;

        Ok((
            match end {
                Some(end) => Either::Left(start..end),
                None => Either::Right(start..),
            },
            step as usize,
        ))
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
