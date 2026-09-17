use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    fmt::Debug,
    fs::OpenOptions,
    io::Write,
};

use arcstr::ArcStr;
use itertools::Itertools;
use once_cell::sync::Lazy;

use crate::{
    ParseDiagnosticExt,
    ast::Variable,
    runner::{BoxedDiagnostic, Runner, RunnerParseError, eval_literal, value::ValueCast},
    span::{Span, SpannedValue, SpanningExt},
};

use super::{
    CastError, RunnerError, Value,
    value::{NumericValue, Unit, ValueMagnitude},
};

type ValueResult = Result<Value, RunnerError>;

pub trait ValueFn: Debug {
    fn invoke(
        &self,
        runner: &mut Runner,
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

        self.invoke_inner(runner, call_site, args)
    }

    fn arity(&self) -> usize {
        self.arguments().len()
    }

    fn is_help(&self) -> bool {
        false
    }

    fn description(&self) -> &'static str;

    fn help(&self) -> String {
        let mut arguments = String::new();
        for arg in self.arguments() {
            if !arguments.is_empty() {
                arguments.push(',');
            }
            arguments.push_str(&arg);
        }

        format!("{arguments} : {}", self.description())
    }

    fn arguments(&self) -> Vec<Cow<'static, str>>;
    fn invoke_inner(
        &self,
        runner: &mut Runner,
        call_site: SpannedValue<()>,
        args: Vec<SpannedValue<Value>>,
    ) -> ValueResult;
}

type VFn1<T, H> = fn(T, H) -> ValueResult;
type RunVFn1<T, H> = fn(&mut Runner, T, H) -> ValueResult;

trait HelpProvider: Default {
    fn help() -> &'static str;
}

impl<T, H> ValueFn for VFn1<T, H>
where
    T: ValueCast,
    H: HelpProvider,
{
    fn arguments(&self) -> Vec<Cow<'static, str>> {
        vec![T::name()]
    }

    fn description(&self) -> &'static str {
        H::help()
    }

    fn invoke_inner(
        &self,
        _: &mut Runner,
        _: SpannedValue<()>,
        args: Vec<SpannedValue<Value>>,
    ) -> ValueResult {
        let (arg,) = args.into_iter().collect_tuple().unwrap();

        (self)(arg.cast()?, H::default())
    }
}

impl<T, H> ValueFn for RunVFn1<T, H>
where
    T: ValueCast,
    H: HelpProvider,
{
    fn arguments(&self) -> Vec<Cow<'static, str>> {
        vec![T::name()]
    }

    fn description(&self) -> &'static str {
        H::help()
    }

    fn invoke_inner(
        &self,
        runner: &mut Runner,
        _: SpannedValue<()>,
        args: Vec<SpannedValue<Value>>,
    ) -> ValueResult {
        let (arg,) = args.into_iter().collect_tuple().unwrap();

        (self)(runner, arg.cast()?, H::default())
    }
}

impl From<Vec<&str>> for Variable {
    fn from(value: Vec<&str>) -> Self {
        Variable(value.into_iter().map(ArcStr::from).collect())
    }
}

#[derive(Debug)]
#[allow(unused)]
struct Help;

static HELP: Help = Help;

impl ValueFn for Help {
    fn arguments(&self) -> Vec<Cow<'static, str>> {
        vec!["item".into()]
    }

    fn invoke_inner(
        &self,
        _: &mut Runner,
        _: SpannedValue<()>,
        _: Vec<SpannedValue<Value>>,
    ) -> ValueResult {
        unreachable!("Help is a special function and can’t be called")
    }

    fn is_help(&self) -> bool {
        true
    }

    fn description(&self) -> &'static str {
        "display the help for the specified identifier"
    }
}

pub type Function = &'static (dyn ValueFn + Sync + Send + 'static);

pub static FUNCTIONS: Lazy<HashMap<Variable, Function>> = Lazy::new(|| {
    let mut funcs: HashMap<_, Function> = HashMap::new();

    funcs.insert(vec!["to", "binary"].into(), &(to_binary as VFn1<i128, _>));
    funcs.insert(vec!["to", "hex"].into(), &(to_hex as VFn1<i128, _>));
    funcs.insert(
        vec!["strip", "unit"].into(),
        &(strip_unit as VFn1<NumericValue, _>),
    );
    funcs.insert(vec!["to", "int"].into(), &(to_int as VFn1<NumericValue, _>));
    funcs.insert(vec!["factorial"].into(), &(factorial as VFn1<u64, _>));

    funcs.insert(
        vec!["to", "bin"].into(),
        funcs[&vec!["to", "binary"].into()],
    );

    funcs.insert(vec!["len"].into(), &(length as VFn1<_, _>));
    funcs.insert(vec!["parse"].into(), &(parse as RunVFn1<_, _>));

    funcs.insert(vec!["help"].into(), &HELP);

    funcs.insert(vec!["save"].into(), &(save as RunVFn1<_, _>));
    funcs.insert(vec!["restore"].into(), &(restore as RunVFn1<_, _>));

    funcs
});

macro_rules! help {
    ($name:ident, $desc:literal) => {
        #[derive(Default)]
        struct $name;

        impl HelpProvider for $name {
            fn help() -> &'static str {
                $desc
            }
        }
    };
}

help!(
    ToBinary,
    "generate a string of the binary representation of the number"
);
fn to_binary(v: i128, _: ToBinary) -> ValueResult {
    Ok(Value::Str(format!("0b{v:b}")))
}

help!(
    ToHex,
    "generate a string of the hexadecimal representation of the number"
);
fn to_hex(v: i128, _: ToHex) -> ValueResult {
    Ok(Value::Str(format!("0x{v:x}")))
}

help!(ToInt, "convert the number to an integer, by rounding");
fn to_int(v: NumericValue, _: ToInt) -> ValueResult {
    Ok(Value::Numeric(NumericValue {
        magnitude: v.magnitude.round_to_int(),
        unit: v.unit,
    }))
}

help!(StripUnit, "remove the unit of the specified number");
fn strip_unit(v: NumericValue, _: StripUnit) -> ValueResult {
    Ok(Value::Numeric(NumericValue {
        magnitude: v.magnitude,
        unit: Unit::dimensionless(),
    }))
}

help!(Factorial, "compute the factorial of the input");
fn factorial(v: u64, _: Factorial) -> ValueResult {
    Ok(Value::Numeric(NumericValue {
        magnitude: ValueMagnitude::factorial(v),
        unit: Unit::dimensionless(),
    }))
}

help!(Length, "compute the length of the input string");
fn length(v: String, _: Length) -> ValueResult {
    Ok(Value::Numeric(NumericValue {
        magnitude: ValueMagnitude::new(v.len() as _),
        unit: Unit::dimensionless(),
    }))
}

help!(Parse, "parse the input string as a raclette literral");
fn parse(runner: &mut Runner, value: SpannedValue<String>, _: Parse) -> ValueResult {
    let parser = crate::calc::DimensionedLiteralParser::new();

    let sub_input = value.as_str().into();

    match parser
        .parse(&sub_input, crate::ast::lexer(&value.value))
        .into_parse_diagnostic()
    {
        Ok((literal, unit)) => {
            let inner = eval_literal(&literal);

            let literal_span = Span {
                start: value.start + 1 + literal.start,
                end: value.start + 1 + literal.end,
                source: value.source.clone(),
                value: (),
            };

            match inner {
                Value::Numeric(numeric_value) => {
                    let (multiplied, unit) = runner.resolve_units(&unit)?;

                    Ok(NumericValue {
                        magnitude: ValueMagnitude::mul(
                            value.span(),
                            numeric_value.magnitude.spanned(&literal_span),
                            multiplied.spanned(&value.span()),
                        )?,
                        unit,
                    }
                    .into())
                }
                v => {
                    if !unit.is_empty() {
                        Err(
                            CastError::from_val(v.spanned(&literal_span), "dimensionless literal")
                                .into(),
                        )
                    } else {
                        Ok(v)
                    }
                }
            }
        }
        Err(error) => Err(RunnerError::ParseError(BoxedDiagnostic(Box::new(
            RunnerParseError {
                error,
                location: (value.start..value.end).into(),
                src: value.source.clone(),
            },
        )))),
    }
}

help!(Save, "save the environement to the specified path");
fn save(runner: &mut Runner, value: String, _: Save) -> ValueResult {
    let mut file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(value)?;

    let out = ron::to_string(&runner.values)?;

    file.write_all(out.as_bytes())?;

    Ok(Value::Atom(ArcStr::from("ok")))
}

help!(
    Restore,
    "restore a saved environement from the specified path"
);
fn restore(runner: &mut Runner, value: String, _: Restore) -> ValueResult {
    let values: BTreeMap<Variable, Value> = ron::from_str(&std::fs::read_to_string(value)?)?;

    runner.values.extend(values);

    Ok(Value::Atom(ArcStr::from("ok")))
}
