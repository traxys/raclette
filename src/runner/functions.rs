use std::{collections::HashMap, sync::Arc};

use itertools::Itertools;
use once_cell::sync::Lazy;

use crate::{ast::Variable, span::SpannedValue};

use super::{
    value::{NumericValue, Unit, ValueMagnitude},
    CastError, Value,
};

type ValueResult = Result<Value, miette::Report>;

pub trait ValueFn {
    fn invoke(&self, args: Vec<SpannedValue<Value>>) -> ValueResult {
        if self.arity() != args.len() {
            miette::bail!("Function was not provided with the correct number of arguments\nExpected {} got {}", self.arity(), args.len())
        }

        self.invoke_inner(args)
    }

    fn arity(&self) -> usize;
    fn invoke_inner(&self, args: Vec<SpannedValue<Value>>) -> ValueResult;
}

type VFn1<T> = fn(T) -> ValueResult;

impl<T> ValueFn for fn(T) -> ValueResult
where
    T: TryFrom<SpannedValue<Value>, Error = CastError>,
{
    fn arity(&self) -> usize {
        1
    }

    fn invoke_inner(&self, args: Vec<SpannedValue<Value>>) -> ValueResult {
        let (arg,) = args.into_iter().collect_tuple().unwrap();

        (self)(arg.try_into()?)
    }
}

impl From<Vec<&str>> for Variable {
    fn from(value: Vec<&str>) -> Self {
        Variable(value.into_iter().map(Arc::from).collect())
    }
}

pub static FUNCTIONS: Lazy<HashMap<Variable, &'static (dyn ValueFn + Sync + Send + 'static)>> =
    Lazy::new(|| {
        let mut funcs: HashMap<_, &'static (dyn ValueFn + Sync + Send + 'static)> = HashMap::new();

        funcs.insert(vec!["to", "binary"].into(), &(to_binary as VFn1<i64>));
        funcs.insert(vec!["to", "hex"].into(), &(to_hex as VFn1<i64>));
        funcs.insert(
            vec!["strip", "unit"].into(),
            &(strip_unit as VFn1<NumericValue>),
        );
        funcs.insert(vec!["to", "int"].into(), &(to_int as VFn1<NumericValue>));
        funcs.insert(vec!["factorial"].into(), &(factorial as VFn1<u32>));

        funcs
    });

fn to_binary(v: i64) -> ValueResult {
    Ok(Value::Str(format!("0b{v:b}")))
}

fn to_hex(v: i64) -> ValueResult {
    Ok(Value::Str(format!("0x{v:x}")))
}

fn to_int(v: NumericValue) -> ValueResult {
    Ok(Value::Numeric(NumericValue {
        magnitude: ValueMagnitude::Int(v.magnitude.into_int()),
        unit: v.unit,
    }))
}

fn strip_unit(v: NumericValue) -> ValueResult {
    Ok(Value::Numeric(NumericValue {
        magnitude: v.magnitude,
        unit: Unit::dimensionless(),
    }))
}

fn factorial(v: u32) -> ValueResult {
    Ok(Value::Numeric(NumericValue {
        magnitude: ValueMagnitude::Int(
            (1..=(v as i64))
                .try_fold(1, i64::checked_mul)
                .ok_or_else(|| miette::miette!("Value overflow"))?,
        ),
        unit: Unit::dimensionless(),
    }))
}
