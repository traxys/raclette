use itertools::Itertools;
use miette::SourceCode;

use crate::{
    ast::RcStr,
    interpreter::{value::func::BuiltinFnSpec, SerdeError},
    span::{Span, SpannedValue, SpanningExt, UNKNOWN_SPAN},
    Val, Value,
};
use once_cell::sync::Lazy;
use std::{
    collections::HashMap,
    fs::File,
    io::{BufReader, Read},
    ops::Deref,
    rc::Rc,
};

use super::{
    value::{
        func::{FunctionKind, FunctionValue},
        HashableValue,
    },
    Interpreter, RuntimeError,
};

static BUILTIN_SPAN: Lazy<Span> = Lazy::new(|| Span {
    start: 0,
    end: "<builtin>".len(),
    source: "<builtin>".into(),
    value: (),
});

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
                    }, &*BUILTIN_SPAN)
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
                    s.spanned(&*UNKNOWN_SPAN)
                });
            )*
        }


        impl $name {
            pub fn from_named(named: HashMap<SpannedValue<RcStr>, Val>) -> Result<$name, RuntimeError> {
                if let Some(name) = named.keys().find(|key|
                    ![$(stringify!($field))*].contains(&(***key).deref())
                ) {
                    return Err(RuntimeError::invalid_named_argument(
                        name.value.to_string(),
                        &name.span(),
                    ));
                }


                Ok($name {
                    $(
                        $field: $field.with(|field| -> Result<_, RuntimeError> {match named.get(&*field) {
                            None => Ok(None),
                            Some(v) => {
                                let v = v.borrow();
                                Ok(Some(v.$caster(&*v)?))
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
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
            sep with cast_number(i64)
        }
    }
    let named = Named::from_named(named)?;

    let v = args[0].borrow().cast_number(&*args[0].borrow())?;
    let str = format!("{v:#x}");
    match named.sep {
        None => Ok(Value::new_str(str, call_span)),
        Some(v) => Ok(Value::new_str(separate_string(str, v), call_span)),
    }
}

fn bin(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
            sep with cast_number(i64)
        }
    }
    let named = Named::from_named(named)?;

    let v = args[0].borrow().cast_number(&*args[0].borrow())?;
    let str = format!("{v:#b}");
    match named.sep {
        None => Ok(Value::new_str(str, call_span)),
        Some(v) => Ok(Value::new_str(separate_string(str, v), call_span)),
    }
}

fn parse_int(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    let v = args[0].borrow().cast_str(&*args[0].borrow())?;
    let n = if v.starts_with("0x") {
        i64::from_str_radix(v.trim_start_matches("0x"), 16)
    } else if v.starts_with("0b") {
        i64::from_str_radix(v.trim_start_matches("0b"), 2)
    } else {
        v.parse()
    }
    .map_err(|_| RuntimeError::parse_int_error(&*args[0].borrow()))?;

    Ok(Value::new_number(n, call_span))
}

fn open_file(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    let v = args[0].borrow().cast_str(&*args[0].borrow())?;
    let f = File::open(&*v).map_err(|e| RuntimeError::io_error(e, &args[0].borrow()))?;

    Ok(Value::new_file(f, call_span))
}

fn ser_value(
    args: Vec<Val>,
    named_map: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
            fmt with cast_str(RcStr)
        }
    }
    let fmt_span = fmt.with(|k| named_map.get(k).map(|v| v.borrow().span()));
    let named = Named::from_named(named_map)?;

    enum Format {
        Yaml,
        Toml,
        Ron,
        Json,
    }

    let format = match named.fmt {
        None => Format::Json,
        Some(s) => match s.to_lowercase().trim() {
            "yaml" => Format::Yaml,
            "toml" => Format::Toml,
            "ron" => Format::Ron,
            "json" => Format::Json,
            _ => {
                return Err(RuntimeError::unsupported_serde_format(
                    s.to_string(),
                    &fmt_span.unwrap(),
                ));
            }
        },
    };

    let arg = args[0].borrow();

    let serde_error = |s: SerdeError| RuntimeError::serde_error(s, &*arg);

    let stringified = match format {
        Format::Json => serde_json::to_string(&**arg)
            .map_err(Into::into)
            .map_err(serde_error),
        Format::Yaml => serde_yaml::to_string(&**arg)
            .map_err(Into::into)
            .map_err(serde_error),
        Format::Ron => ron::to_string(&**arg)
            .map_err(Into::into)
            .map_err(serde_error),
        Format::Toml => toml::to_string(&**arg)
            .map_err(Into::into)
            .map_err(serde_error),
    }?;

    Ok(Value::new_str(stringified, call_span))
}

fn deser_value(
    args: Vec<Val>,
    named_map: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
            fmt with cast_str(RcStr)
        }
    }
    let fmt_span = fmt.with(|k| named_map.get(k).map(|v| v.borrow().span()));
    let named = Named::from_named(named_map)?;

    enum Format {
        Yaml,
        Toml,
        Ron,
        Json,
    }

    let format = match named.fmt {
        None => Format::Json,
        Some(s) => match s.to_lowercase().trim() {
            "yaml" => Format::Yaml,
            "toml" => Format::Toml,
            "ron" => Format::Ron,
            "json" => Format::Json,
            _ => {
                return Err(RuntimeError::unsupported_serde_format(
                    s.to_string(),
                    &fmt_span.unwrap(),
                ));
            }
        },
    };

    let arg_span = args[0].borrow().span();
    let serde_error = |s: SerdeError| RuntimeError::serde_error(s, &arg_span);

    let parsed: Value = match &mut **args[0].borrow_mut() {
        Value::File(f) => {
            let mut buf_reader = BufReader::new(f);
            match format {
                Format::Json => serde_json::from_reader(&mut buf_reader)
                    .map_err(Into::into)
                    .map_err(serde_error)?,
                Format::Yaml => serde_yaml::from_reader(&mut buf_reader)
                    .map_err(Into::into)
                    .map_err(serde_error)?,
                Format::Ron => ron::de::from_reader(&mut buf_reader)
                    .map_err(Into::into)
                    .map_err(serde_error)?,
                Format::Toml => {
                    let mut data = Vec::new();
                    buf_reader
                        .read_to_end(&mut data)
                        .map_err(|e| RuntimeError::io_error(e, &arg_span))?;
                    toml::de::from_slice(&data)
                        .map_err(Into::into)
                        .map_err(serde_error)?
                }
            }
        }
        Value::Hashable(HashableValue::Str(s)) => match format {
            Format::Json => serde_json::from_str(s)
                .map_err(Into::into)
                .map_err(serde_error)?,
            Format::Yaml => serde_yaml::from_str(s)
                .map_err(Into::into)
                .map_err(serde_error)?,
            Format::Ron => ron::from_str(s).map_err(Into::into).map_err(serde_error)?,
            Format::Toml => toml::from_str(s).map_err(Into::into).map_err(serde_error)?,
        },
        v => {
            return Err(RuntimeError::cast_error(
                "str-like".into(),
                v.name().into(),
                &arg_span,
            ))
        }
    };

    Ok(Value::new(parsed.respan(call_span)))
}

fn shell_function(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    let arg = args[0].borrow().cast_str(&args[0].borrow())?;

    Ok(Value::new_function(
        FunctionValue {
            arity: 1,
            action: FunctionKind::Shell(arg),
        },
        call_span,
    ))
}

fn to_list(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    let arg = args[0].borrow();
    let iter = arg.cast_iterable(&args[0].borrow())?;

    Ok(Value::new_array(iter.collect(), call_span))
}

fn zero(
    _: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    Ok(Value::new_number(0, call_span))
}

fn ty(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    let arg = args[0].borrow().name();

    Ok(Value::new_str(arg.into(), call_span))
}

fn dump_vars(
    _: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    interpreter: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    for (i, scope) in interpreter.scopes.iter().rev().enumerate() {
        println!("# Scope {i}");
        for (name, value) in &scope.vars {
            println!(" - {} = {}", name, value.borrow().value)
        }
    }

    println!("# Global Scope");
    for (name, value) in &interpreter.global.vars {
        println!(" - {} = {}", name, value.borrow().value)
    }

    Ok(Value::new_null(call_span))
}

fn def_from_span<U>(v: &SpannedValue<U>) -> String {
    String::from_utf8(
        v.source
            .read_span(&(v.start..v.end).into(), 0, 0)
            .expect("could not read span")
            .data()
            .to_vec(),
    )
    .expect("source code is utf8")
}

fn fnkind_def<'a>(f: &'a FunctionKind) -> String {
    match f {
        &FunctionKind::Builtin(ptr) => format!("{:#?}", ptr as BuiltinFnSpec<'a>),
        FunctionKind::Folder(f) => match f {
            super::value::func::folder::Folder::Op(op) => format!("%({})", op.kind()),
            super::value::func::folder::Folder::User(_, _) => todo!(),
        },
        FunctionKind::Mapper(m) => format!("@({})", def_from_span(&m.borrow())),
        FunctionKind::User {
            args: _,
            scope: _,
            ret,
        } => def_from_span(ret),
        FunctionKind::SpecifyNamed { func, named } => {
            format!("({}){{{:?}}}", fnkind_def(&func.action), named)
        }
        FunctionKind::Shell(s) => format!("shf({})", s),
    }
}

fn fn_def(
    args: Vec<Val>,
    named: HashMap<SpannedValue<RcStr>, Val>,
    call_span: &Span,
    _: &mut Interpreter,
) -> Result<Val, RuntimeError> {
    named_args! {
        struct Named {
        }
    }
    let _named = Named::from_named(named)?;

    let arg = args[0].borrow().cast_function(&args[0].borrow())?;

    let def = fnkind_def(&arg.action);

    Ok(Value::new_str(def, call_span))
}

macro_rules! define_f64_fn {
    ($name:ident) => {
        fn $name(
            args: Vec<Val>,
            named: HashMap<SpannedValue<RcStr>, Val>,
            call_span: &Span,
            _: &mut Interpreter,
        ) -> Result<Val, RuntimeError> {
            named_args! {
                struct Named {
                }
            }
            let _named = Named::from_named(named)?;

            let arg = args[0].borrow().cast_num(&args[0].borrow())?.f64();

            Ok(Value::new_float(arg.$name(), call_span))
        }
    };
}

define_f64_fn! {cos}
define_f64_fn! {sin}
define_f64_fn! {tan}
define_f64_fn! {acos}
define_f64_fn! {asin}
define_f64_fn! {atan}
define_f64_fn! {exp}
define_f64_fn! {ln}
define_f64_fn! {log2}
define_f64_fn! {ceil}
define_f64_fn! {floor}

define_builtin! {
    ty(1) => ty;
    X(1) => hex;
    B(1) => bin;
    int(1) => parse_int;
    open(1) => open_file;
    shf(1) => shell_function;
    list(1) => to_list;
    des(1) => deser_value;
    ser(1) => ser_value;
    zero(1) => zero;
    __vars(0) => dump_vars;
    __fndef(1) => fn_def;
    cos(1) => cos;
    sin(1) => sin;
    tan(1) => tan;
    acos(1) => acos;
    asin(1) => asin;
    atan(1) => atan;
    exp(1) => exp;
    ln(1) => ln;
    log2(1) => log2;
    ceil(1) => ceil;
    floor(1) => floor;
}
