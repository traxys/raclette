use std::{collections::HashMap, sync::Arc};

use itertools::Itertools;
use once_cell::sync::Lazy;

use crate::{ast::Variable, span::SpannedValue};

use super::{
    value::{NumericValue, Unit, ValueMagnitude},
    CastError, RunnerError, Value,
};

type ValueResult = Result<Value, RunnerError>;

pub trait ValueFn {
    fn invoke(
        &self,
        callee: SpannedValue<()>,
        call_site: SpannedValue<()>,
        args: Vec<SpannedValue<Value>>,
    ) -> ValueResult {
        if self.arity() != args.len() {
            return Err(RunnerError::FunctionArity {
                provided: args.len(),
                arity: self.arity(),
                f: (callee.start..callee.end).into(),
                src: callee.source,
            });
        }

        self.invoke_inner(call_site, args)
    }

    fn arity(&self) -> usize;
    fn invoke_inner(
        &self,
        call_site: SpannedValue<()>,
        args: Vec<SpannedValue<Value>>,
    ) -> ValueResult;
}

type VFn1<T> = fn(T) -> ValueResult;
type SpnVFn1<T> = fn(SpannedValue<()>, T) -> ValueResult;

impl<T> ValueFn for VFn1<T>
where
    T: TryFrom<SpannedValue<Value>, Error = CastError>,
{
    fn arity(&self) -> usize {
        1
    }

    fn invoke_inner(&self, _: SpannedValue<()>, args: Vec<SpannedValue<Value>>) -> ValueResult {
        let (arg,) = args.into_iter().collect_tuple().unwrap();

        (self)(arg.try_into()?)
    }
}

impl<T> ValueFn for SpnVFn1<T>
where
    T: TryFrom<SpannedValue<Value>, Error = CastError>,
{
    fn arity(&self) -> usize {
        1
    }

    fn invoke_inner(
        &self,
        call_site: SpannedValue<()>,
        args: Vec<SpannedValue<Value>>,
    ) -> ValueResult {
        let (arg,) = args.into_iter().collect_tuple().unwrap();

        (self)(call_site, arg.try_into()?)
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
        funcs.insert(vec!["factorial"].into(), &(factorial as SpnVFn1<u32>));

        funcs.insert(
            vec!["to", "bin"].into(),
            funcs[&vec!["to", "binary"].into()],
        );

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

fn factorial(call_site: SpannedValue<()>, v: u32) -> ValueResult {
    Ok(Value::Numeric(NumericValue {
        magnitude: ValueMagnitude::Int((1..=(v as i64)).try_fold(1, i64::checked_mul).ok_or_else(
            || RunnerError::Overflow {
                location: (call_site.start..call_site.end).into(),
                src: call_site.source,
            },
        )?),
        unit: Unit::dimensionless(),
    }))
}
