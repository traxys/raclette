use std::{collections::HashMap, io::Write, process::Stdio};

use crate::{
    ast::{self, RcStr},
    interpreter::{Interpreter, Result, RuntimeError, Scope, SpannedResult},
    span::{GenerationSpan, SpannedValue, Spanning},
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

type BuiltinFn =
    fn(Vec<Val>, HashMap<SpannedValue<RcStr>, Val>, GenerationSpan, u64) -> Result<Val>;

#[derive(Clone, Debug)]
pub enum FunctionKind {
    Builtin(BuiltinFn),
    Folder(folder::Folder),
    Mapper(Val),
    User {
        args: Vec<String>,
        ret: SpannedValue<ast::Expr>,
    },
    SpecifyNamed {
        func: Box<FunctionValue>,
        named: HashMap<SpannedValue<RcStr>, Val>,
    },
    Shell(RcStr),
}

impl FunctionValue {
    pub fn call(
        &self,
        args: Vec<Val>,
        mut named: HashMap<SpannedValue<RcStr>, Val>,
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
                let int_iterable = iter.int_iterable();
                if let Some(mut iter) = int_iterable {
                    let first = iter.next();
                    match folder {
                        Folder::Op(f) => {
                            let mut f = f.restart();
                            if let Some(first) = first {
                                f.seed(Value::new_number(first, span, scope.generation))?;
                            }

                            for item in iter {
                                f.int_accumulate(item, scope)?;
                            }

                            f.finish(span, scope.generation)
                        }
                        Folder::User(f, def) => {
                            let v = f.borrow();
                            let func = v.cast_function()?;
                            let gen = scope.generation;
                            iter.map(|n| Value::new_number(n, span, gen)).try_fold(
                                first
                                    .map(|n| Value::new_number(n, span, gen))
                                    .unwrap_or_else(|| def.clone()),
                                |acc, e| {
                                    let spn = e.borrow().gen_span();
                                    func.call(vec![acc, e], HashMap::new(), scope, &spn)
                                },
                            )
                        }
                    }
                } else {
                    let mut iter = iter.cast_iterable()?;
                    let first = iter.next();
                    match folder {
                        Folder::Op(f) => {
                            let mut f = f.restart();
                            if let Some(first) = first {
                                f.seed(first)?;
                            }

                            for item in iter {
                                f.accumulate(&item, scope)?;
                            }

                            f.finish(span, scope.generation)
                        }
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

    pub fn op_folder<S, U>(f: Box<dyn NativeFolder>, span: &S, gen: u64) -> Val
    where
        S: Spanning<U>,
    {
        Value::new_function(
            FunctionValue {
                arity: 1,
                action: FunctionKind::Folder(Folder::Op(f)),
            },
            span,
            gen,
        )
    }
}
