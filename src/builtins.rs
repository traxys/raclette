use crate::interpeter::{FunctionKind, FunctionValue, RuntimeError, Val, Value};
use std::collections::HashMap;

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
                    })
                );
            )*
            map
        }
    };
}

fn hex(args: Vec<Val>) -> Result<Val, RuntimeError> {
    let v = args[0].borrow().cast_number()?;
    Ok(Value::new_str(format!("{v:#x}")))
}

fn bin(args: Vec<Val>) -> Result<Val, RuntimeError> {
    let v = args[0].borrow().cast_number()?;
    Ok(Value::new_str(format!("{v:#b}")))
}

fn parse_int(args: Vec<Val>) -> Result<Val, RuntimeError> {
    let v = args[0].borrow().cast_str()?;
    let n = if v.starts_with("0x") {
        i64::from_str_radix(v.trim_start_matches("0x"), 16)
    } else if v.starts_with("0b") {
        i64::from_str_radix(v.trim_start_matches("0b"), 2)
    } else {
        v.parse()
    }
    .map_err(|_| RuntimeError::ParseIntError)?;

    Ok(Value::new_number(n))
}

define_builtin! {
    X(1) => hex;
    B(1) => bin;
    int(1) => parse_int;
}
