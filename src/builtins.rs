use itertools::Itertools;

use crate::interpeter::{
    FunctionKind, FunctionValue, HashableValue, RuntimeError, SpannedResult, Val, Value,
};
use crate::span::{GcSpannedValue, Span, Spanning, SpanningExt, UNKNOWN_SPAN};
use once_cell::unsync::Lazy;
use std::{collections::HashMap, fs::File, io::BufReader, rc::Rc};

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
                    }, UNKNOWN_SPAN)
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
                static $field: Lazy<GcSpannedValue<Rc<str>>> = Lazy::new(|| {
                    let s: Rc<str> = Rc::from(stringify!($field));
                    s.spanned_gc(UNKNOWN_SPAN)
                });
            )*
        }


        impl $name {
            pub fn from_named(named: HashMap<GcSpannedValue<Rc<str>>, Val>) -> Result<$name, RuntimeError> {
                if let Some(name) = named.keys().find(|key|
                    ![$(stringify!($field))*].contains(&(***key).as_ref())
                ) {
                    return Err(RuntimeError::InvalidNamedArgument {
                        location: Some(name.span().into()),
                        name: name.value.to_string(),
                    });
                }


                Ok($name {
                    $(
                        $field: $field.with(|field| match named.get(&*field) {
                            None => Ok(None),
                            Some(v) => {
                                let v = v.borrow();
                                Ok(Some(v.$caster().with_span(&*v)?))
                            }
                        })?
                    )*
                })
            }
        }
    };
}

fn hex(
    args: Vec<Val>,
    named: HashMap<GcSpannedValue<Rc<str>>, Val>,
    span: Span,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
            sep with cast_number(i64)
        }
    }
    let named = Named::from_named(named)?;

    let v = args[0]
        .borrow()
        .cast_number()
        .with_span(&*args[0].borrow())?;
    let str = format!("{v:#x}");
    match named.sep {
        None => Ok(Value::new_str(str, &span)),
        Some(v) => Ok(Value::new_str(separate_string(str, v), &span)),
    }
}

fn bin(
    args: Vec<Val>,
    named: HashMap<GcSpannedValue<Rc<str>>, Val>,
    span: Span,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
            sep with cast_number(i64)
        }
    }
    let named = Named::from_named(named)?;

    let v = args[0]
        .borrow()
        .cast_number()
        .with_span(&*args[0].borrow())?;
    let str = format!("{v:#b}");
    match named.sep {
        None => Ok(Value::new_str(str, &span)),
        Some(v) => Ok(Value::new_str(separate_string(str, v), &span)),
    }
}

fn parse_int(
    args: Vec<Val>,
    named: HashMap<GcSpannedValue<Rc<str>>, Val>,
    span: Span,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    let v = args[0].borrow().cast_str().with_span(&*args[0].borrow())?;
    let n = if v.starts_with("0x") {
        i64::from_str_radix(v.trim_start_matches("0x"), 16)
    } else if v.starts_with("0b") {
        i64::from_str_radix(v.trim_start_matches("0b"), 2)
    } else {
        v.parse()
    }
    .map_err(|_| RuntimeError::ParseIntError)?;

    Ok(Value::new_number(n, &span))
}

fn open_file(
    args: Vec<Val>,
    named: HashMap<GcSpannedValue<Rc<str>>, Val>,
    span: Span,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    let v = args[0].borrow().cast_str().with_span(&*args[0].borrow())?;
    let f = File::open(&*v).map_err(RuntimeError::IoError)?;

    Ok(Value::new_file(f, &span))
}

fn parse_json_value(
    args: Vec<Val>,
    named: HashMap<GcSpannedValue<Rc<str>>, Val>,
    span: Span,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    let arg_span = args[0].borrow().span();
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
                location: Some(arg_span.into()),
            })
        }
    };

    Ok(Value::new(parsed.spanned_gc(&span)))
}

define_builtin! {
    X(1) => hex;
    B(1) => bin;
    int(1) => parse_int;
    open(1) => open_file;
    json(1) => parse_json_value;
}
