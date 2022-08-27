use std::{collections::HashMap, fmt::Debug, io::Write, process::Stdio};

use crate::{
    ast::{self, RcStr},
    interpreter::{Interpreter, Result, RuntimeError, Scope},
    span::{Span, SpannedValue},
    Val,
};

use super::Value;

pub(in crate::interpreter) mod folder;

use folder::{Folder, NativeFolder};

#[derive(Clone, Debug)]
pub struct FunctionValue {
    pub(crate) arity: usize,
    pub(crate) action: FunctionKind,
}

type BuiltinFnSpec<'a> = fn(Vec<Val>, HashMap<SpannedValue<RcStr>, Val>, &'a Span) -> Result<Val>;
type BuiltinFn = fn(Vec<Val>, HashMap<SpannedValue<RcStr>, Val>, &Span) -> Result<Val>;

#[derive(Clone)]
pub enum FunctionKind {
    Builtin(BuiltinFn),
    Folder(folder::Folder),
    Mapper(Val),
    User {
        args: Vec<String>,
        scope: Vec<Scope>,
        ret: SpannedValue<ast::Expr>,
    },
    SpecifyNamed {
        func: Box<FunctionValue>,
        named: HashMap<SpannedValue<RcStr>, Val>,
    },
    Shell(RcStr),
}

impl Debug for FunctionKind {
    fn fmt<'a>(&'a self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin(arg0) => f
                .debug_tuple("Builtin")
                .field(&(*arg0 as BuiltinFnSpec<'a>) as &dyn Debug)
                .finish(),
            Self::Folder(arg0) => f.debug_tuple("Folder").field(arg0).finish(),
            Self::Mapper(arg0) => f.debug_tuple("Mapper").field(arg0).finish(),
            Self::User { args, ret, scope: _ } => f
                .debug_struct("User")
                .field("args", args)
                .field("ret", ret)
                .field("scope", &"<...>")
                .finish(),
            Self::SpecifyNamed { func, named } => f
                .debug_struct("SpecifyNamed")
                .field("func", func)
                .field("named", named)
                .finish(),
            Self::Shell(arg0) => f.debug_tuple("Shell").field(arg0).finish(),
        }
    }
}

impl FunctionValue {
    pub fn call(
        &self,
        args: Vec<Val>,
        named: HashMap<SpannedValue<RcStr>, Val>,
        interpreter: &mut Interpreter,
        call_span: &Span,
    ) -> Result<Val> {
        if args.len() != self.arity {
            return Err(RuntimeError::argument_count_mismatch(
                self.arity,
                args.len(),
                call_span,
            ));
        }

        let mut named = named;
        match &self.action {
            FunctionKind::Builtin(f) => f(args, named, call_span),
            FunctionKind::SpecifyNamed {
                func,
                named: already_specified,
            } => {
                for (key, val) in already_specified {
                    if !named.contains_key(key) {
                        named.insert(key.clone(), val.clone());
                    }
                }
                func.call(args, named, interpreter, call_span)
            }
            FunctionKind::Folder(folder) => {
                let iter = args[0].borrow();
                let int_iterable = iter.int_iterable();
                if let Some(mut iter) = int_iterable {
                    let first = iter.next();
                    match folder {
                        Folder::Op(f) => {
                            let mut f = f.restart();
                            if let Some(first) = first {
                                f.seed(Value::new_number(first, call_span))?;
                            }

                            for item in iter {
                                f.int_accumulate(item, interpreter)?;
                            }

                            f.finish(call_span)
                        }
                        Folder::User(f, def) => {
                            let v = f.borrow();
                            let func = v.cast_function(&v)?;
                            iter.map(|n| Value::new_number(n, call_span)).try_fold(
                                first
                                    .map(|n| Value::new_number(n, call_span))
                                    .unwrap_or_else(|| def.clone()),
                                |acc, e| func.call(vec![acc, e], HashMap::new(), interpreter, call_span),
                            )
                        }
                    }
                } else {
                    let mut iter = iter.cast_iterable(&iter)?;
                    let first = iter.next();
                    match folder {
                        Folder::Op(f) => {
                            let mut f = f.restart();
                            if let Some(first) = first {
                                f.seed(first)?;
                            }

                            for item in iter {
                                f.accumulate(&item, interpreter)?;
                            }

                            f.finish(call_span)
                        }
                        Folder::User(f, def) => {
                            let v = f.borrow();
                            let func = v.cast_function(&v)?;
                            iter.try_fold(first.unwrap_or_else(|| def.clone()), |acc, e| {
                                func.call(vec![acc, e], HashMap::new(), interpreter, call_span)
                            })
                        }
                    }
                }
            }
            FunctionKind::Mapper(m) => {
                let iter = args[0].borrow();
                let m = m.borrow();
                let mapper = m.cast_function(&m)?;
                match &**iter {
                    Value::Map(map) => {
                        let mut new_map = map.clone();
                        for (_, v) in new_map.iter_mut() {
                            let spn = &v.borrow().span();
                            *v = mapper.call(vec![v.clone()], HashMap::new(), interpreter, spn)?;
                        }

                        Ok(Value::new_map(new_map, call_span))
                    }
                    Value::Array(a) => Ok(Value::new_array(
                        a.iter()
                            .map(|e| {
                                mapper.call(
                                    vec![e.clone()],
                                    HashMap::new(),
                                    interpreter,
                                    &e.borrow().span(),
                                )
                            })
                            .collect::<Result<_>>()?,
                        call_span,
                    )),
                    _ => Err(RuntimeError::not_iterable(iter.name().into(), &iter)),
                }
            }
            FunctionKind::User { args: names, ret, scope } => {
                let s = Scope {
                    vars: names.iter().cloned().zip(args).collect(),
                };

                let mut scoped = interpreter.closure_scope(scope.clone());
                scoped.push_scope(s);
                let ret = scoped.run_expr(ret.clone());
                scoped.pop_scope();
                ret
            }
            FunctionKind::Shell(cmd) => {
                let arg = args[0].borrow();
                let arg = arg.cast_str(&arg)?;

                let io_error = |s: std::io::Error| RuntimeError::io_error(s, call_span);

                let mut command = std::process::Command::new("/bin/sh");
                let mut process = command
                    .arg("-c")
                    .arg(&**cmd)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .stderr(Stdio::piped())
                    .spawn()
                    .map_err(io_error)?;

                let stdin = process.stdin.as_mut().unwrap();
                stdin.write_all(arg.as_bytes()).map_err(io_error)?;

                let output = process.wait_with_output().map_err(io_error)?;

                if !output.status.success() {
                    return Err(RuntimeError::process_failure(
                        output.status,
                        output.stderr.into(),
                        call_span,
                    ));
                }

                let output = String::from_utf8(output.stdout)
                    .map_err(|_| RuntimeError::invalid_utf8(call_span))?;

                Ok(Value::new_str(output, call_span))
            }
        }
    }

    pub fn user_folder(f: Val, def: Val, def_span: &Span) -> Val {
        Value::new_function(
            FunctionValue {
                arity: 1,
                action: FunctionKind::Folder(Folder::User(f, def)),
            },
            def_span,
        )
    }

    pub fn op_folder<U>(f: Box<dyn NativeFolder>, span: &SpannedValue<U>) -> Val {
        Value::new_function(
            FunctionValue {
                arity: 1,
                action: FunctionKind::Folder(Folder::Op(f)),
            },
            span,
        )
    }
}
