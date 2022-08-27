use std::{
    collections::HashMap,
    fmt::Debug,
    io::Read,
    ops::{Deref, DerefMut},
    process::ExitStatus,
};

use crate::{
    ast::{self, RangeExpr},
    raclette::FileParser,
    span::{MaybeNamed, SpannedValue},
};
use bstr::BString;
use either::Either;
use miette::{Diagnostic, NamedSource, SourceSpan};

mod builtins;
pub mod value;

use value::{
    func::{folder::*, FunctionKind, FunctionValue},
    power_nums, Val, Value,
};

pub static STDLIB: &str = include_str!("../stdlib.rcl");

macro_rules! define_error {
    (
        $(#[$($meta:meta)*])?
        pub enum $name:ident {
            $(
                $(#[$($item_meta:meta)*])?
                $item:ident {
                    $(
                        $(#[$($field_meta:meta)*])?
                        $field:ident: $ty:ty
                    ),*

                    $(,)?

                    ;where

                    location = $loc:literal $(,)?
                }
            ),*

            $(,)?
        }
    ) => {
        $(#[$($meta)*])?
        pub enum $name {
            $(
                $(#[$($item_meta)*])?
                $item {
                    $(
                        $(#[$($field_meta)*])?
                        $field: $ty,
                    )*
                    #[label($loc)]
                    location: SourceSpan,
                    #[source_code]
                    src: MaybeNamed,
                    __private: (),
                },
            )*
        }

        impl $name {
            paste::paste! {
            $(
                pub fn [< $item:snake >]<U>($($field: $ty,)* span: &SpannedValue<U>) -> Self {
                    Self::$item {
                        $($field,)*
                        location: (span.start..span.end).into(),
                        src: span.source.clone(),
                        __private: (),
                    }
                }
            )*
            }
        }
    }
}

define_error! {
    #[derive(Debug, thiserror::Error, Diagnostic)]
    pub enum RuntimeError {
        #[error("Invalid Type. Expected `{into}` got `{from}`")]
        CastError {
            into: String,
            from: String,
            ;where
                location = "this value is of type {from}",
        },
        #[error("Invalid Argument count. Expected {expected} got {got}")]
        ArgumentCountMismatch {
            expected: usize,
            got: usize,
            ;where location = "this call supplies {got} arguments",
        },
        #[error("Named argument {name} does not exist")]
        InvalidNamedArgument {
            name: String,
            ;where location = "this named argument is not valid",
        },
        #[error("{ty} is not hashable")]
        NotHashable {
            ty: String,
            ;where location = "value is not hashable"
        },
        #[error("{ty} is not iterable")]
        NotIterable {
            ty: String,
            ;where location = "value is not iterable"
        },
        #[error("Field {field} does not exist")]
        NoSuchField {
            field: String,
            #[label("in this expression")]
            expr_location: SourceSpan,
            ;where location = "this field does not exist",
        },
        #[error("Field {field} is not assignable")]
        FieldNotAssignable {
            field: String,
            ;where location = "this field can't be assigned"
        },
        #[error("{ident} is not defined")]
        UndefinedIdentifier {
            ident: String,
            ;where location = "identifier is not defined"
        },
        #[error("Could not process utf8")]
        InvalidUtf8 {
            ;where location = "produced a value that is not utf8"
        },
        #[error("Io error")]
        IoError {
            #[source]
            err: std::io::Error,
            ;where location = "returned an IO error"
        },
        #[error("Subprocess failed with error {status}.\nError output: {error}")]
        ProcessFailure {
            status: ExitStatus,
            error: BString,
            ;where location = "this expression launched a subprocess"
        },
        #[error("Value of type `{ty}` is not indexable with values of type `{idx_ty}`")]
        NotIndexableWith {
            ty: String,
            idx_ty: String,
            #[label("This index is of type `{idx_ty}`")]
            idx_location: SourceSpan,
            ;where location = "This value is of type `{ty}`"
        },
        #[error("Index `{idx}` could not index value")]
        NoSuchIndex {
            idx: String,
            ;where location = "This value is invalid for the indexed expression"
        },
        #[error("The accessed index is read only")]
        IndexNotAssignable {
            idx: String,
            ;where location = "This index is read-only"
        },
        #[error("The range is reversed")]
        ReversedRange {
            ;where location = "This range has `start > end`"
        },
        #[error("The format `{format}` is not supported for ser/de")]
        UnsupportedSerdeFormat {
            format: String,
            ;where location = "This is not a valid format"
        },
        #[error("ser/de error")]
        SerdeError{
            #[source]
            err: SerdeError
            ;where location = "This expression could not be parsed/serialized"
        },
        #[error("Could not parse integer")]
        ParseIntError{
            ;where location = "This expression is not a valid number"
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SerdeError {
    #[error("Could not parse json value")]
    Json(#[from] serde_json::Error),
    #[error("Could not parse yaml value")]
    Yaml(#[from] serde_yaml::Error),
    #[error("Could not parse ron value")]
    RonDe(#[from] ron::error::SpannedError),
    #[error("Could not serialize ron value")]
    RonSer(#[from] ron::Error),
    #[error("Could not serialize toml value")]
    TomlSer(#[from] toml::ser::Error),
    #[error("Could not parse toml value")]
    TomlDe(#[from] toml::de::Error),
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
    scopes: Vec<Scope>,
    global: Scope,
}

#[derive(Clone)]
pub struct Scope {
    vars: HashMap<String, Val>,
}

type RangeIteratorDef = (
    Either<std::ops::Range<i64>, std::ops::RangeFrom<i64>>,
    usize,
);

struct ClosureScoped<'a> {
    interpreter: &'a mut Interpreter,
    old_scope: Vec<Scope>,
}

impl<'a> Deref for ClosureScoped<'a> {
    type Target = Interpreter;

    fn deref(&self) -> &Self::Target {
        self.interpreter
    }
}

impl<'a> DerefMut for ClosureScoped<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.interpreter
    }
}

impl<'a> Drop for ClosureScoped<'a> {
    fn drop(&mut self) {
        self.interpreter.scopes = std::mem::take(&mut self.old_scope);
    }
}

impl Interpreter {
    pub(in crate::interpreter) fn closure_scope(&mut self, mut scope: Vec<Scope>) -> ClosureScoped {
        std::mem::swap(&mut scope, &mut self.scopes);
        ClosureScoped {
            interpreter: self,
            old_scope: scope,
        }
    }

    pub fn resolve(&self, name: &SpannedValue<String>) -> Result<Val> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.vars.get(&**name) {
                return Ok(v.clone());
            }
        }

        match self.global.vars.get(&**name) {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError::undefined_identifier((&**name).into(), name)),
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
        let mut interpreter = Self {
            scopes: Vec::new(),
            global: Scope {
                vars: builtins::create(),
            },
        };

        let stdlib = FileParser::new()
            .parse(
                &NamedSource::new("stdlib", STDLIB).into(),
                ast::lexer(STDLIB),
            )
            .expect("could not parse stdlib");

        for statement in stdlib {
            interpreter
                .run_statement(statement)
                .expect("could not parse stdlib");
        }

        interpreter
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
                        let ses = se.span();
                        let se = self.run_expr(*se)?;

                        let is = i.span();
                        let i = self.run_expr(*i)?;

                        se.borrow_mut().set_index(i, e, &ses, &is)?;
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
        let lhs = self.run_expr(lhs)?.borrow().cast_num(&ls)?;
        let rs = rhs.span();
        let rhs = self.run_expr(rhs)?.borrow().cast_num(&rs)?;
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
                    let lhs = self.run_expr(*lhs)?.borrow().cast_number(&ls)?;
                    let rs = rhs.span();
                    let rhs = self.run_expr(*rhs)?.borrow().cast_number(&rs)?;
                    let v = match op {
                        ast::BinaryOp::BitwiseOr => lhs | rhs,
                        ast::BinaryOp::BitwiseAnd => lhs & rhs,
                        ast::BinaryOp::IntDivide => lhs / rhs,
                        ast::BinaryOp::LShift => lhs << rhs,
                        ast::BinaryOp::RShift => lhs >> rhs,
                        ast::BinaryOp::Modulo => lhs % rhs,
                        _ => unreachable!(),
                    };
                    Ok(Value::new_number(v, expr_span))
                }
                ast::BinaryOp::Power | ast::BinaryOp::Divide => {
                    let ls = lhs.span();
                    let lhs = self.run_expr(*lhs)?.borrow().cast_num(&ls)?;
                    let rs = rhs.span();
                    let rhs = self.run_expr(*rhs)?.borrow().cast_num(&rs)?;

                    match op {
                        ast::BinaryOp::Power => Ok(Value::new_num(power_nums(lhs, rhs), expr_span)),
                        ast::BinaryOp::Divide => {
                            Ok(Value::new_float(lhs.f64() / rhs.f64(), expr_span))
                        }
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
                    Ok(Value::new_num(v, expr_span))
                }
                ast::BinaryOp::Redirect => {
                    let input = self.run_expr(*lhs)?;
                    let f = rhs.span();
                    let func = self.run_expr(*rhs)?.borrow().cast_function(&f)?;
                    func.call(vec![input], HashMap::new(), self, expr_span)
                }
            },
            ast::Expr::Place(p) => match p.value {
                ast::Place::Ident(n) => self.resolve(&n),
                ast::Place::Deref(e, f) => {
                    let e_span = e.span();
                    let e = self.run_expr(*e)?;
                    let e = e.borrow();
                    e.field(f.value.clone()).ok_or_else(|| {
                        RuntimeError::no_such_field(f.to_string(), e_span.into(), &f)
                    })
                }
                ast::Place::Index(e, i) => {
                    let es = e.span();
                    let e = self.run_expr(*e)?;
                    let e = e.borrow();

                    let is = i.span();
                    let i = self.run_expr(*i)?;

                    e.index(i, &es, &is)
                }
            },
            ast::Expr::Call { func, args } => {
                let f = func.span();
                let func = self.run_expr(*func)?.borrow().cast_function(&f)?;

                let args = args
                    .into_iter()
                    .map(|e| self.run_expr(e))
                    .collect::<Result<_>>()?;
                func.call(args, HashMap::new(), self, expr_span)
            }
            ast::Expr::NamedCall { func, named } => {
                let f = func.span();
                let func = Box::new(self.run_expr(*func)?.borrow().cast_function(&f)?);
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
                ))
            }
            ast::Expr::Map(m) => {
                let map: Result<_> = m
                    .into_iter()
                    .map(|(k, v)| -> Result<_> {
                        let ks = k.span();
                        Ok((
                            self.run_expr(k)?.borrow().cast_hashable(&ks)?,
                            self.run_expr(v)?,
                        ))
                    })
                    .collect();
                Ok(Value::new_map(map?, expr_span))
            }
            ast::Expr::Tilde(e) => {
                let v = self.run_expr(*e)?;
                let vs = v.borrow().span();
                let str = match &mut **v.borrow_mut() {
                    Value::File(ref mut f) => {
                        let mut s = String::new();
                        f.read_to_string(&mut s)
                            .map_err(|e| RuntimeError::io_error(e, &vs))?;
                        s
                    }
                    any => any.to_string(),
                };
                Ok(Value::new_str(str, expr_span))
            }
            ast::Expr::Array(a) => {
                let vec: Result<_> = a.into_iter().map(|a| self.run_expr(a)).collect();
                Ok(Value::new_array(vec?, expr_span))
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
                    Ok(FunctionValue::op_folder(folder, expr_span))
                }
                ast::Folder::Args { func, def } => {
                    let func = self.run_expr(*func)?;
                    let def = self.run_expr(*def)?;
                    Ok(FunctionValue::user_folder(func, def, expr_span))
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
                ))
            }
            ast::Expr::FuncDef { args, ret } => Ok(Value::new_function(
                FunctionValue {
                    arity: args.len(),
                    action: FunctionKind::User {
                        args,
                        ret: *ret,
                        scope: self.scopes.clone(),
                    },
                },
                expr_span,
            )),
            ast::Expr::Range(r) => {
                let (range, step) = self.run_range(*r)?;
                Ok(Value::new_range_iterator(range.step_by(step), expr_span))
            }
            ast::Expr::Slice { iterable, slice } => {
                let is = iterable.span();
                let iterable = self.run_expr(*iterable)?;
                let it = iterable.borrow();
                let arr = it.cast_slice(&is)?;

                let rs = slice.span();

                let (range, step) = self.run_range(slice.value)?;

                match range {
                    Either::Left(bounded) => {
                        if bounded.start > bounded.end {
                            return Err(RuntimeError::reversed_range(&rs));
                        }

                        if bounded.start < 0 || bounded.start as usize >= arr.len() {
                            return Err(RuntimeError::no_such_index(
                                bounded.start.to_string(),
                                &rs,
                            ));
                        }

                        if bounded.end < 0 || bounded.end as usize >= arr.len() {
                            return Err(RuntimeError::no_such_index(bounded.end.to_string(), &rs));
                        }

                        Ok(Value::new_array(
                            arr[bounded.start as usize..bounded.end as usize]
                                .iter()
                                .step_by(step)
                                .cloned()
                                .collect(),
                            expr_span,
                        ))
                    }
                    Either::Right(unbounded) => {
                        if unbounded.start < 0 || unbounded.start as usize >= arr.len() {
                            return Err(RuntimeError::no_such_index(
                                unbounded.start.to_string(),
                                &rs,
                            ));
                        }

                        Ok(Value::new_array(
                            arr[unbounded.start as usize..]
                                .iter()
                                .step_by(step)
                                .cloned()
                                .collect(),
                            expr_span,
                        ))
                    }
                }
            }
        }
    }

    fn run_range(&mut self, range: RangeExpr) -> Result<RangeIteratorDef> {
        let RangeExpr { start, end, step } = range;
        let ss = start.span();
        let start = self.run_expr(start)?.borrow().cast_number(&ss)?;
        let end = end
            .map(|end| {
                let es = end.span();

                self.run_expr(end)?.borrow().cast_number(&es)
            })
            .transpose()?;
        let sts = step.span();
        let step = self.run_expr(step)?.borrow().cast_number(&sts)?;

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
            ast::Literal::Float(f) => Value::new_float(f, span),
            ast::Literal::Number(n) => Value::new_number(n, span),
            ast::Literal::String(s) => Value::new_str(s, span),
        })
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
