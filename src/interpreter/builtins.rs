use itertools::Itertools;

use crate::{
    ast::RcStr,
    span::{GenerationSpan, SpannedValue, Spanning, SpanningExt, UNKNOWN_SPAN},
    Val, Value,
};
use once_cell::unsync::Lazy;
use std::{collections::HashMap, fs::File, io::BufReader, ops::Deref, rc::Rc};

use super::{
    value::{
        func::{FunctionKind, FunctionValue},
        HashableValue,
    },
    RuntimeError, SpannedResult,
};

macro_rules! define_builtin {
    ($(
        $name:ident($arity:literal) => $func:ident;
    )*) => {
        pub fn create() -> HashMap<String, Val> {
            let mut map = HashMap::new();
            $(
                map.insert(
                    stringify!($name).to_string(),
                    Value::new_function(FunctionValue {
                        arity: $arity,
                        action: FunctionKind::Builtin($func),
                    }, UNKNOWN_SPAN, 0)
                );
            )*
            map
        }
    };
}

fn separate_string(s: String, sep: i64) -> String {
    let mut new = s[0..2].to_owned();

    let chunks = s[2..].chars().rev().chunks(sep as usize);
    let mut chunks = chunks.into_iter();

    // At least one more char than 0X
    let start: String = chunks.next().unwrap().collect();

    let separated: String = chunks
        .flat_map(|c| std::iter::once('_').chain(c.into_iter()))
        .collect();
    new.extend(separated.chars().rev());
    new.extend(start.chars().rev());

    new
}

macro_rules! named_args {
    (struct $name:ident {
        $($field:ident with $caster:ident($ty:ty)),*
        $(,)?
    }) => {
        struct $name {
            $(
                $field: Option<$ty>,
            )*
        }

        thread_local! {
            $(
                #[allow(non_upper_case_globals)]
                static $field: Lazy<SpannedValue<RcStr>> = Lazy::new(|| {
                    let s: RcStr = RcStr(Rc::from(stringify!($field)));
                    s.spanned(UNKNOWN_SPAN)
                });
            )*
        }


        impl $name {
            pub fn from_named(named: HashMap<SpannedValue<RcStr>, Val>, gen: u64) -> Result<$name, RuntimeError> {
                if let Some(name) = named.keys().find(|key|
                    ![$(stringify!($field))*].contains(&(***key).deref())
                ) {
                    return Err(RuntimeError::InvalidNamedArgument {
                        location: name.span().with_generation(gen).source_span(gen),
                        name: name.value.to_string(),
                    });
                }


                Ok($name {
                    $(
                        $field: $field.with(|field| -> Result<_, RuntimeError> {match named.get(&*field) {
                            None => Ok(None),
                            Some(v) => {
                                let v = v.borrow();
                                Ok(Some(v.$caster().with_span(&*v)?))
                            }
                        }})?
                    )*
                })
            }
        }
    };
}

fn hex(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    span: GenerationSpan,
    gen: u64,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
            sep with cast_number(i64)
        }
    }
    let named = Named::from_named(named, gen)?;

    let v = args[0]
        .borrow()
        .cast_number()
        .with_span(&*args[0].borrow())?;
    let str = format!("{v:#x}");
    match named.sep {
        None => Ok(Value::new_str(str, &span, gen)),
        Some(v) => Ok(Value::new_str(separate_string(str, v), &span, gen)),
    }
}

fn bin(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    span: GenerationSpan,
    gen: u64,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
            sep with cast_number(i64)
        }
    }
    let named = Named::from_named(named, gen)?;

    let v = args[0]
        .borrow()
        .cast_number()
        .with_span(&*args[0].borrow())?;
    let str = format!("{v:#b}");
    match named.sep {
        None => Ok(Value::new_str(str, &span, gen)),
        Some(v) => Ok(Value::new_str(separate_string(str, v), &span, gen)),
    }
}

fn parse_int(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    span: GenerationSpan,
    gen: u64,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named, gen)?;

    let v = args[0].borrow().cast_str().with_span(&*args[0].borrow())?;
    let n = if v.starts_with("0x") {
        i64::from_str_radix(v.trim_start_matches("0x"), 16)
    } else if v.starts_with("0b") {
        i64::from_str_radix(v.trim_start_matches("0b"), 2)
    } else {
        v.parse()
    }
    .map_err(|_| RuntimeError::ParseIntError)?;

    Ok(Value::new_number(n, &span, gen))
}

fn open_file(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    span: GenerationSpan,
    gen: u64,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named, gen)?;

    let v = args[0].borrow().cast_str().with_span(&*args[0].borrow())?;
    let f = File::open(&*v).map_err(Into::into).with_span(&span)?;

    Ok(Value::new_file(f, &span, gen))
}

fn parse_json_value(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    span: GenerationSpan,
    gen: u64,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named, gen)?;

    let arg_span = args[0].borrow().gen_span();
    let parsed: Value = match &mut **args[0].borrow_mut() {
        Value::File(f) => {
            let mut buf_reader = BufReader::new(f);
            serde_json::from_reader(&mut buf_reader).map_err(RuntimeError::JsonError)?
        }
        Value::Hashable(HashableValue::Str(s)) => {
            serde_json::from_str(s).map_err(RuntimeError::JsonError)?
        }
        v => {
            return Err(RuntimeError::CastError {
                into: "str-like".into(),
                from: v.name().into(),
                location: arg_span.source_span(gen),
            })
        }
    };

    Ok(Value::new(parsed.spanned_gen(&span, gen)))
}

fn shell_function(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    span: GenerationSpan,
    gen: u64,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named, gen)?;

    let arg = args[0]
        .borrow()
        .cast_str()
        .with_span(&args[0].borrow().span())?;

    Ok(Value::new_function(
        FunctionValue {
            arity: 1,
            action: FunctionKind::Shell(arg),
        },
        &span,
        gen,
    ))
}

fn to_list(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    span: GenerationSpan,
    gen: u64,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named, gen)?;

    let arg = args[0].borrow();
    let iter = arg.cast_iterable().with_span(&args[0].borrow().span())?;

    Ok(Value::new_array(iter.collect(), &span, gen))
}

define_builtin! {
    X(1) => hex;
    B(1) => bin;
    int(1) => parse_int;
    open(1) => open_file;
    json(1) => parse_json_value;
    shf(1) => shell_function;
    list(1) => to_list;
}
