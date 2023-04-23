use std::{collections::HashMap, sync::Arc};

use either::Either;
use enum_map::{Enum, EnumMap};
use itertools::Itertools;
use miette::{Context, Diagnostic, SourceSpan};
use once_cell::sync::Lazy;

use crate::{
    ast::{self, Variable},
    span::{MaybeNamed, SpannedValue, SpanningExt},
};

use self::functions::ValueFn;

mod functions;

#[derive(Debug, Clone, Copy)]
pub enum ValueMagnitude {
    Int(i64),
    Float(f64),
}

impl ValueMagnitude {
    fn as_string(&self, precision: Option<usize>) -> String {
        match self {
            ValueMagnitude::Int(n) => n.to_string(),
            ValueMagnitude::Float(f) => match precision {
                None => f.to_string(),
                Some(p) => format!("{f:.*}", p),
            },
        }
    }

    fn is_zero(&self) -> bool {
        match *self {
            ValueMagnitude::Int(i) => i == 0,
            ValueMagnitude::Float(f) => f == 0.,
        }
    }

    fn as_float(&self) -> f64 {
        match *self {
            ValueMagnitude::Int(i) => i as _,
            ValueMagnitude::Float(f) => f,
        }
    }
}

impl std::ops::Mul for ValueMagnitude {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // TODO: Switch to float if result is too large
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) => ValueMagnitude::Int(a * b),
            (ValueMagnitude::Int(a), ValueMagnitude::Float(b)) => {
                ValueMagnitude::Float(a as f64 * b)
            }
            (ValueMagnitude::Float(a), ValueMagnitude::Int(b)) => {
                ValueMagnitude::Float(a * b as f64)
            }
            (ValueMagnitude::Float(a), ValueMagnitude::Float(b)) => ValueMagnitude::Float(a * b),
        }
    }
}

impl std::ops::Div for ValueMagnitude {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ValueMagnitude::Int(a), ValueMagnitude::Int(b)) if a % b == 0 => {
                ValueMagnitude::Int(a / b)
            }
            (lhs, rhs) => ValueMagnitude::Float(lhs.as_float() / rhs.as_float()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Dimension {
    // Expressed in Bytes
    Byte,
    // Expressed in Meters
    Length,
    // Expressed in Seconds
    Time,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Unit {
    pub dimensions: EnumMap<Dimension, i16>,
}

impl Unit {
    fn dimensionless() -> Self {
        Self {
            dimensions: EnumMap::from_array([0; 3]),
        }
    }

    fn is_dimensionless(&self) -> bool {
        self.dimensions.values().all(|&v| v == 0)
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
impl std::ops::Mul for Unit {
    type Output = Self;

    fn mul(mut self, rhs: Self) -> Self::Output {
        for (dim, e) in rhs.dimensions {
            self.dimensions[dim] += e;
        }

        self
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
impl std::ops::Div for Unit {
    type Output = Self;

    fn div(mut self, rhs: Self) -> Self::Output {
        for (dim, e) in rhs.dimensions {
            self.dimensions[dim] -= e;
        }

        self
    }
}

enum ScaleType {
    Metric,
    TimeMetric,
    Binary,
}

impl ScaleType {
    fn steps(&self) -> &'static [ScaleStep] {
        match self {
            ScaleType::Metric => METRIC_SCALE,
            ScaleType::TimeMetric => TIME_METRIC_SCALE,
            ScaleType::Binary => BINARY_SCALE,
        }
    }

    fn prefix(&self) -> impl Iterator<Item = (&'static str, f64)> {
        let steps = self.steps();
        steps
            .iter()
            .filter_map(|s| match s.render {
                ScaleRender::Prefix(p) => Some(Either::Left(std::iter::once((p, s.order)))),
                ScaleRender::EitherPrefix { main, alternative } => Some(Either::Right(
                    [(main, s.order), (alternative, s.order)].into_iter(),
                )),
                _ => None,
            })
            .flatten()
    }

    fn all_prefix() -> impl Iterator<Item = (&'static str, f64)> {
        Self::Metric
            .prefix()
            .chain(Self::TimeMetric.prefix())
            .chain(Self::Binary.prefix())
            .unique_by(|(p, _)| *p)
            .sorted_by(|(pa, _), (pb, _)| pb.len().cmp(&pa.len()))
    }
}

#[derive(Clone, Copy, Debug)]
enum ScaleRender {
    Override(&'static str),
    Prefix(&'static str),
    EitherPrefix {
        main: &'static str,
        alternative: &'static str,
    },
    AsIs,
}

#[derive(Clone, Copy, Debug)]
struct ScaleStep {
    order: f64,
    render: ScaleRender,
}

static TIME_METRIC_SCALE: &[ScaleStep] = &[
    ScaleStep {
        render: ScaleRender::Override("yr"),
        order: 60. * 60. * 24. * 364.25,
    },
    ScaleStep {
        render: ScaleRender::Override("day"),
        order: 60. * 60. * 24.,
    },
    ScaleStep {
        render: ScaleRender::Override("hr"),
        order: 60. * 60.,
    },
    ScaleStep {
        render: ScaleRender::Override("min"),
        order: 60.,
    },
    ScaleStep {
        render: ScaleRender::AsIs,
        order: 1.,
    },
    ScaleStep {
        render: ScaleRender::Prefix("m"),
        order: 0.001,
    },
    ScaleStep {
        render: ScaleRender::Prefix("μ"),
        order: 0.000001,
    },
    ScaleStep {
        render: ScaleRender::Prefix("u"),
        order: 0.000001,
    },
    ScaleStep {
        render: ScaleRender::Prefix("n"),
        order: 0.000000001,
    },
    ScaleStep {
        render: ScaleRender::Prefix("p"),
        order: 0.000000000001,
    },
];

static BINARY_SCALE: &[ScaleStep] = &[
    ScaleStep {
        render: ScaleRender::Prefix("Ti"),
        order: 1024. * 1024. * 1024. * 1024.,
    },
    ScaleStep {
        render: ScaleRender::Prefix("Gi"),
        order: 1024. * 1024. * 1024.,
    },
    ScaleStep {
        render: ScaleRender::Prefix("Mi"),
        order: 1024. * 1024.,
    },
    ScaleStep {
        render: ScaleRender::Prefix("Ki"),
        order: 1024.,
    },
    ScaleStep {
        render: ScaleRender::AsIs,
        order: 1.,
    },
];

static METRIC_SCALE: &[ScaleStep] = &[
    ScaleStep {
        render: ScaleRender::Prefix("E"),
        order: 1000000000000000000.,
    },
    ScaleStep {
        render: ScaleRender::Prefix("P"),
        order: 1000000000000000.,
    },
    ScaleStep {
        render: ScaleRender::Prefix("T"),
        order: 1000000000000.,
    },
    ScaleStep {
        render: ScaleRender::Prefix("G"),
        order: 1000000000.,
    },
    ScaleStep {
        render: ScaleRender::Prefix("M"),
        order: 1000000.,
    },
    ScaleStep {
        render: ScaleRender::Prefix("k"),
        order: 1000.,
    },
    ScaleStep {
        render: ScaleRender::AsIs,
        order: 1.,
    },
    ScaleStep {
        render: ScaleRender::Prefix("m"),
        order: 0.001,
    },
    ScaleStep {
        render: ScaleRender::EitherPrefix {
            main: "μ",
            alternative: "u",
        },
        order: 0.000001,
    },
    ScaleStep {
        render: ScaleRender::Prefix("n"),
        order: 0.000000001,
    },
    ScaleStep {
        render: ScaleRender::Prefix("p"),
        order: 0.000000000001,
    },
];

#[derive(Debug, Clone)]
pub struct NumericValue {
    pub magnitude: ValueMagnitude,
    pub unit: Unit,
}

impl std::ops::Div for NumericValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        NumericValue {
            magnitude: self.magnitude / rhs.magnitude,
            unit: self.unit / rhs.unit,
        }
    }
}

impl std::ops::Mul for NumericValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        NumericValue {
            magnitude: self.magnitude * rhs.magnitude,
            unit: self.unit * rhs.unit,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Numeric(NumericValue),
    Str(String),
    Atom(Arc<str>),
}

impl Value {
    fn is_zero(&self) -> bool {
        match self {
            Value::Numeric(n) => n.magnitude.is_zero(),
            Value::Str(_) => false,
            Value::Atom(_) => false,
        }
    }
}

impl From<NumericValue> for Value {
    fn from(value: NumericValue) -> Self {
        Self::Numeric(value)
    }
}

impl std::ops::Div for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a / b)),
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
        }
    }
}

impl std::ops::Mul for SpannedValue<Value> {
    type Output = Result<Value, RunnerError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self.value, rhs.value) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a * b)),
            (Value::Str(_), _) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Str(_)) => Err(RunnerError::InvalidType {
                ty: "str",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
            (Value::Atom(_), _) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (self.start..self.end).into(),
                src: self.source,
            }),
            (_, Value::Atom(_)) => Err(RunnerError::InvalidType {
                ty: "atom",
                location: (rhs.start..rhs.end).into(),
                src: rhs.source,
            }),
        }
    }
}

#[derive(thiserror::Error, Debug, Diagnostic)]
#[error("Could not cast from {from} to {to}")]
pub struct CastError {
    from: &'static str,
    to: &'static str,
    #[label("this value is of type {from}")]
    location: SourceSpan,
    #[source_code]
    src: MaybeNamed,
}

impl CastError {
    fn from_val(val: SpannedValue<Value>, to: &'static str) -> Self {
        Self {
            to,
            from: match &*val {
                Value::Numeric(n) => match n.magnitude {
                    ValueMagnitude::Int(_) => "int",
                    ValueMagnitude::Float(_) => "float",
                },
                Value::Str(_) => "str",
                Value::Atom(_) => "atom",
            },
            location: (val.start..val.end).into(),
            src: val.source,
        }
    }
}

impl TryFrom<SpannedValue<Value>> for i64 {
    type Error = CastError;

    fn try_from(value: SpannedValue<Value>) -> Result<Self, Self::Error> {
        match &*value {
            Value::Numeric(NumericValue {
                magnitude: ValueMagnitude::Int(i),
                ..
            }) => Ok(*i),
            _ => Err(CastError::from_val(value, "int")),
        }
    }
}

impl TryFrom<SpannedValue<Value>> for NumericValue {
    type Error = CastError;

    fn try_from(value: SpannedValue<Value>) -> Result<Self, Self::Error> {
        match value {
            SpannedValue {
                value: Value::Numeric(n),
                ..
            } => Ok(n),
            SpannedValue {
                value: Value::Str(_),
                ..
            } => Err(CastError::from_val(value, "numeric")),
            SpannedValue {
                value: Value::Atom(_),
                ..
            } => Err(CastError::from_val(value, "numeric")),
        }
    }
}

#[derive(thiserror::Error, Debug, Diagnostic)]
pub enum RunnerError {
    #[error("Undefined Identifier: '{name:?}'")]
    UndefinedIdentifier {
        name: Variable,
        #[label("this identifier")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Divide by zero")]
    DivideByZero {
        #[label("this expression is 0")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Invalid unit: '{unit}'")]
    InvalidUnit {
        unit: String,
        #[label("this is not a valid unit")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Could not perform operation on this type")]
    InvalidType {
        ty: &'static str,
        #[label("This value is of type '{ty}'")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("No value was stored")]
    NoStoredValue,
    #[error("Command does not exist")]
    UnknownCommand {
        #[label("this is not a valid command name")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Value '{val}' is not valid for this command")]
    InvalidCommandValue {
        val: String,
        #[label("this command does not accept this value")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Value missing for this command")]
    MissingCommandValue {
        #[label("this command requires a value")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
}

pub struct Runner {
    last: Option<SpannedValue<Value>>,
    values: HashMap<Variable, Value>,
    scales: HashMap<Unit, ScaleType>,
    default_scale: ScaleType,
    round: Option<usize>,
}

pub static BYTE_UNIT: Lazy<Unit> = Lazy::new(|| {
    let mut unit = Unit::dimensionless();
    unit.dimensions[Dimension::Byte] = 1;
    unit
});

pub static KNOWN_UNITS: Lazy<HashMap<Unit, &'static str>> = Lazy::new(|| {
    use Dimension::*;

    let mut units = HashMap::new();

    units.insert(*BYTE_UNIT, "B");

    let mut unit = Unit::dimensionless();
    unit.dimensions[Length] += 1;
    units.insert(unit, "m");

    let mut unit = Unit::dimensionless();
    unit.dimensions[Time] += 1;
    units.insert(unit, "s");

    let mut unit = Unit::dimensionless();
    unit.dimensions[Time] -= 1;
    units.insert(unit, "Hz");

    units
});

impl Runner {
    pub fn new() -> Self {
        let mut values = HashMap::new();

        values.insert(
            Variable(vec![Arc::from("pi")]),
            Value::Numeric(NumericValue {
                magnitude: ValueMagnitude::Float(std::f64::consts::PI),
                unit: Unit::dimensionless(),
            }),
        );

        let mut scales = HashMap::new();
        scales.insert(
            {
                let mut u = Unit::dimensionless();
                u.dimensions[Dimension::Time] = 1;
                u
            },
            ScaleType::TimeMetric,
        );
        scales.insert(*BYTE_UNIT, ScaleType::Binary);

        Self {
            last: None,
            values,
            scales,
            default_scale: ScaleType::Metric,
            round: Some(2),
        }
    }

    pub fn display_value(&self, value: &Value) -> String {
        match value {
            Value::Numeric(n) => self.display_numeric_value(n),
            Value::Str(s) => s.clone(),
            Value::Atom(a) => format!(":{a}"),
        }
    }

    fn resolve_function(
        &self,
        func: ast::Function,
    ) -> Result<&(dyn ValueFn + Send + Sync), RunnerError> {
        match func {
            ast::Function::Ref(name) => {
                functions::FUNCTIONS
                    .get(&*name)
                    .copied()
                    .ok_or(RunnerError::UndefinedIdentifier {
                        name: name.value,
                        location: (name.start..name.end).into(),
                        src: name.source,
                    })
            }
        }
    }

    fn display_numeric_value(&self, value: &NumericValue) -> String {
        let raw_magnitude = value.magnitude.as_string(self.round);

        if value.unit.is_dimensionless() {
            raw_magnitude
        } else {
            match KNOWN_UNITS.get(&value.unit) {
                None => {
                    let unit = value
                        .unit
                        .dimensions
                        .iter()
                        .filter_map(|(dim, &scale)| {
                            let name = match dim {
                                Dimension::Byte => "B",
                                Dimension::Length => "m",
                                Dimension::Time => "s",
                            };
                            match scale {
                                0 => None,
                                1 => Some(name.to_string()),
                                s => Some(format!("{name}{s}")),
                            }
                        })
                        .join(".");
                    format!("{raw_magnitude} {unit}")
                }
                Some(u) => {
                    let scale_prefixes = self
                        .scales
                        .get(&value.unit)
                        .unwrap_or(&self.default_scale)
                        .steps();

                    let mut magnitude = value.magnitude.as_float();
                    let mut render = scale_prefixes[0].render;

                    if magnitude >= scale_prefixes[0].order {
                        magnitude /= scale_prefixes[0].order;
                    } else if magnitude < scale_prefixes.iter().last().unwrap().order {
                        todo!()
                    } else {
                        for (&large, &small) in
                            scale_prefixes.iter().zip(scale_prefixes.iter().skip(1))
                        {
                            if magnitude < large.order && magnitude >= small.order {
                                magnitude /= small.order;
                                render = small.render;
                                break;
                            }
                        }
                    };

                    let unit_part = match render {
                        ScaleRender::Override(o) => Either::Left(o),
                        ScaleRender::Prefix(p) | ScaleRender::EitherPrefix { main: p, .. } => {
                            Either::Right(format!("{p}{u}"))
                        }
                        ScaleRender::AsIs => Either::Left(*u),
                    };
                    let magnitude = match &self.round {
                        None => magnitude.to_string(),
                        Some(r) => format!("{magnitude:.*}", r),
                    };
                    format!("{magnitude} {unit_part}")
                }
            }
        }
    }

    fn resolve_units(
        &self,
        units: &[SpannedValue<(Arc<str>, i16)>],
    ) -> Result<(f64, Unit), RunnerError> {
        let mut multiplier = 1.;
        let mut unit_acc = Unit::dimensionless();

        for span in units {
            let (unit, scale) = &span.value;
            if *scale == 0 {
                continue;
            };

            let mut unit_proper: &str = unit;
            for (prefix, mult) in ScaleType::all_prefix() {
                if unit.len() <= prefix.len() {
                    continue;
                }

                if let Some(u) = unit.strip_prefix(prefix) {
                    if *scale > 0 {
                        multiplier *= mult;
                    } else {
                        multiplier /= mult;
                    }
                    unit_proper = u;
                    break;
                }
            }

            let unit = match unit_proper {
                "B" => vec![(Dimension::Byte, 1)],
                "m" => vec![(Dimension::Length, 1)],
                "s" => vec![(Dimension::Time, 1)],
                _ => {
                    return Err(RunnerError::InvalidUnit {
                        unit: unit.to_string(),
                        location: (span.start..span.end).into(),
                        src: span.source.clone(),
                    })
                }
            };

            for (dim, dim_scale) in unit {
                unit_acc.dimensions[dim] += scale * dim_scale;
            }
        }

        Ok((multiplier, unit_acc))
    }

    fn eval_expr(&mut self, expr: &ast::Expr) -> Result<Value, miette::Report> {
        match expr {
            ast::Expr::Literal(l) => Ok(self.eval_literal(l)),
            ast::Expr::DimensionalLiteral(l, u) => {
                let value: NumericValue = self.eval_literal(l).spanned(&l.span()).try_into()?;
                let (multiplied, unit) = self.resolve_units(u)?;
                Ok(NumericValue {
                    magnitude: value.magnitude * ValueMagnitude::Float(multiplied),
                    unit,
                }
                .into())
            }
            ast::Expr::Variable(v) => {
                let span = v.span();
                match self.values.get(v) {
                    None => Err(RunnerError::UndefinedIdentifier {
                        name: (**v).clone(),
                        location: (span.start..span.end).into(),
                        src: span.source,
                    }
                    .into()),
                    Some(v) => Ok((*v).clone()),
                }
            }
            ast::Expr::Assign(v, e) => {
                let expr = self.eval_expr(e)?;
                self.values.insert((**v).clone(), expr.clone());
                Ok(expr)
            }
            ast::Expr::BinOp(b) => self.eval_bin_op(b),
        }
    }

    fn eval_bin_op(&mut self, b: &ast::BinOp) -> Result<Value, miette::Report> {
        let lhs_span = b.lhs.span();
        let lhs = self.eval_expr(&b.lhs)?;
        let rhs_span = b.rhs.span();
        let rhs = self.eval_expr(&b.rhs)?;
        match b.kind {
            ast::BinOpKind::Times => (lhs.spanned(&lhs_span) * rhs.spanned(&rhs_span))
                .wrap_err("could not multily operands"),
            ast::BinOpKind::Divide => {
                if rhs.is_zero() {
                    return Err(RunnerError::DivideByZero {
                        location: (b.rhs.start..b.rhs.end).into(),
                        src: b.rhs.source.clone(),
                    }
                    .into());
                }

                (lhs.spanned(&lhs_span) / rhs.spanned(&rhs_span))
                    .with_context(|| "could not divide operands")
            }
        }
    }

    fn eval_literal(&mut self, lit: &ast::Literal) -> Value {
        match lit {
            &ast::Literal::Number(v) => Value::Numeric(NumericValue {
                magnitude: ValueMagnitude::Int(v),
                unit: Unit::dimensionless(),
            }),
            &ast::Literal::Float(v) => Value::Numeric(NumericValue {
                magnitude: ValueMagnitude::Float(v),
                unit: Unit::dimensionless(),
            }),
            ast::Literal::Atom(a) => Value::Atom(a.clone()),
        }
    }

    pub fn handle_command(
        &mut self,
        name: SpannedValue<Arc<str>>,
        value: Option<Value>,
    ) -> Result<(), RunnerError> {
        let location = (name.start..name.end).into();
        let src = name.source;
        match &*name.value {
            "round" => {
                match value {
                    None => return Err(RunnerError::MissingCommandValue { location, src }),
                    Some(Value::Atom(v)) if &*v == "none" => self.round = None,
                    Some(Value::Numeric(NumericValue {
                        magnitude: ValueMagnitude::Int(n),
                        unit,
                    })) if unit.is_dimensionless() => self.round = Some(n as _),
                    Some(v) => {
                        return Err(RunnerError::InvalidCommandValue {
                            val: self.display_value(&v),
                            location,
                            src,
                        })
                    }
                };
                Ok(())
            }
            "byte_scale" => {
                match value {
                    None => return Err(RunnerError::MissingCommandValue { location, src }),
                    Some(Value::Atom(v)) if &*v == "binary" => {
                        self.scales.insert(*BYTE_UNIT, ScaleType::Binary);
                    }
                    Some(Value::Atom(v)) if &*v == "metric" => {
                        self.scales.insert(*BYTE_UNIT, ScaleType::Metric);
                    }
                    Some(v) => {
                        return Err(RunnerError::InvalidCommandValue {
                            val: self.display_value(&v),
                            location,
                            src,
                        })
                    }
                };
                Ok(())
            }
            "default_scale" => {
                match value {
                    None => return Err(RunnerError::MissingCommandValue { location, src }),
                    Some(Value::Atom(v)) if &*v == "binary" => {
                        self.default_scale = ScaleType::Binary
                    }
                    Some(Value::Atom(v)) if &*v == "metric" => {
                        self.default_scale = ScaleType::Metric
                    }
                    Some(v) => {
                        return Err(RunnerError::InvalidCommandValue {
                            val: self.display_value(&v),
                            location,
                            src,
                        })
                    }
                };
                Ok(())
            }
            "help" => {
                match value {
                    None => {
                        println!("Commands:");
                        println!("  round = :none | number (default is 2)");
                        println!("  byte_scale = :binary | :metric (default is :binary)");
                        println!("  default_scale = :binary | :metric (default is :metric)");
                    }
                    Some(_) => todo!(),
                };

                Ok(())
            }
            _ => Err(RunnerError::UnknownCommand { location, src }),
        }
    }

    pub fn eval_input_statement(
        &mut self,
        expr: SpannedValue<ast::InputStatement>,
    ) -> Result<Option<Value>, miette::Report> {
        let span = expr.span();
        let value = match expr.value {
            ast::InputStatement::Command(name, val) => {
                let v = val.as_ref().map(|e| self.eval_expr(e)).transpose()?;
                self.handle_command(name, v)?;
                return Ok(self.last.clone().map(|v| v.value));
            }
            ast::InputStatement::Expr(e) => self.eval_expr(&e)?,
            ast::InputStatement::LastRedirect(func) => match &self.last {
                None => Err(RunnerError::NoStoredValue)?,
                Some(last) => {
                    let f = self.resolve_function(func)?;
                    f.invoke(vec![last.clone()])?
                }
            },
        };
        self.last = Some(value.clone().spanned(&span));
        Ok(Some(value))
    }
}
