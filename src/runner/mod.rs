use std::{collections::HashMap, sync::Arc};

use either::Either;
use itertools::Itertools;
use miette::{Context, Diagnostic, SourceSpan};

use crate::{
    ast::{self, Variable},
    span::{MaybeNamed, Span, SpannedValue, SpanningExt},
};

use functions::ValueFn;
use value::{
    BYTE_UNIT, KNOWN_UNITS, MASS_UNIT, ScaleRender, ScaleStep, ScaleType, TIME_UNIT, Unit,
    ValueMagnitude,
};

use self::value::{NumericValue, Value};

mod commands;
mod functions;
mod value;

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
                Value::Numeric(n) => n.magnitude.ty(),
                v => v.ty(),
            },
            location: (val.start..val.end).into(),
            src: val.source,
        }
    }
}

#[derive(thiserror::Error, Debug, Diagnostic)]
#[error("Parsing failure")]
pub struct RunnerParseError {
    #[source]
    #[diagnostic_source]
    error: crate::ParseError,
    #[label("this input could not be parsed")]
    location: SourceSpan,
    #[source_code]
    src: MaybeNamed,
}

pub struct BoxedDiagnostic<T>(Box<T>);

impl<T: std::fmt::Debug> std::fmt::Debug for BoxedDiagnostic<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for BoxedDiagnostic<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: std::error::Error> std::error::Error for BoxedDiagnostic<T> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.source()
    }
}

impl<T: Diagnostic> Diagnostic for BoxedDiagnostic<T> {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.0.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.url()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        self.0.source_code()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.0.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.0.related()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.0.diagnostic_source()
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
    #[error("Unit could be interpreted in multiple ways")]
    AmbiguousUnit {
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
    #[error("Value is not an unsigned integer")]
    UintConversion {
        #[label("This number is not an unsigned integer")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Operation returned a NaN value")]
    NaN {
        #[label("This calculation returned a NaN value")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Unit mismatches")]
    UnitMismatch {
        #[label("this value is of unit {lhs_unit}")]
        lhs: SourceSpan,
        lhs_unit: String,
        #[label("this value is of unit {rhs_unit}")]
        rhs: SourceSpan,
        rhs_unit: String,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Type mismatch")]
    IncompatibleTypes {
        lhs_ty: &'static str,
        #[label("this value is of type {lhs_ty}")]
        lhs: SourceSpan,
        rhs_ty: &'static str,
        #[label("this value is of type {rhs_ty}")]
        rhs: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("function called with an invalid number of arguments, provided {provided} arguments")]
    FunctionArity {
        provided: usize,
        arity: usize,
        #[label("function requires {arity} arguments")]
        f: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Value overflow")]
    Overflow {
        #[label("this operation overflowed an integer value")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Unit overflow")]
    UnitOverflow {
        #[label("this operation overflowed the units")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[diagnostic(transparent)]
    #[error(transparent)]
    ParseError(BoxedDiagnostic<RunnerParseError>),
    #[error("Could not cast value")]
    #[diagnostic(transparent)]
    Cast(#[from] CastError),
}

pub struct Runner {
    last: Option<SpannedValue<Value>>,
    values: HashMap<Variable, Value>,
    scales: HashMap<Unit, ScaleType>,
    default_scale: NamedValue<ScaleType>,
    display_config: DisplayConfig,
}

struct NamedValue<T> {
    value: T,
    name: Variable,
}

pub struct DisplayConfig {
    round: NamedValue<Option<usize>>,
    large_threshold: NamedValue<Option<usize>>,
    neg_exponent: NamedValue<Option<i64>>,
}

fn atom_or<V, F>(v: Option<V>, f: F) -> Value
where
    F: Fn(V) -> Value,
{
    v.map(f).unwrap_or_else(|| Value::Atom(Arc::from("none")))
}

fn atom_int_or(v: Option<i128>) -> Value {
    atom_or(v, |n| {
        Value::Numeric(NumericValue {
            magnitude: n.into(),
            unit: Unit::dimensionless(),
        })
    })
}

enum ResolvedUnit<'a> {
    Prefix(Vec<(&'a str, Unit)>),
    Magnitude(ValueMagnitude, Unit),
}

fn render_with_unit(
    value: NumericValue,
    unit: &str,
    scale_prefixes: &[ScaleStep],
    display_config: &DisplayConfig,
) -> String {
    let mut magnitude = value.magnitude;
    let mut render = scale_prefixes[0].render;

    let last_unit = scale_prefixes.iter().last().unwrap();

    if magnitude.is_zero() {
        render = ScaleRender::AsIs;
    } else if magnitude.ge_abs(&scale_prefixes[0].order) {
        magnitude = ValueMagnitude::div_ok(magnitude, scale_prefixes[0].order.clone());
    } else if magnitude.lt_abs(&last_unit.order) {
        /* Smaller than the smallest unit */
        magnitude = ValueMagnitude::div_ok(magnitude, last_unit.order.clone());
        render = last_unit.render;
    } else {
        for (large, small) in scale_prefixes.iter().zip(scale_prefixes.iter().skip(1)) {
            if magnitude.lt_abs(&large.order) && magnitude.ge_abs(&small.order) {
                magnitude = ValueMagnitude::div_ok(magnitude, small.order.clone());
                render = small.render;
                break;
            }
        }
    };

    let unit_part = match render {
        ScaleRender::Override(o) => Either::Left(o),
        ScaleRender::Prefix(p) | ScaleRender::EitherPrefix { main: p, .. } => {
            Either::Right(format!("{p}{unit}"))
        }
        ScaleRender::AsIs => Either::Left(unit),
    };

    let scaled_magnitude = magnitude.to_string(display_config);
    format!("{scaled_magnitude} {unit_part}")
}

fn eval_literal(lit: &ast::Literal) -> Value {
    match lit {
        &ast::Literal::Number(v) => Value::Numeric(NumericValue {
            magnitude: v.into(),
            unit: Unit::dimensionless(),
        }),
        &ast::Literal::Decimal(literal) => Value::Numeric(NumericValue {
            magnitude: ValueMagnitude::new_decimal(literal),
            unit: Unit::dimensionless(),
        }),
        ast::Literal::Atom(a) => Value::Atom(a.clone()),
        &ast::Literal::Bool(b) => Value::Bool(b),
        ast::Literal::String(s) => Value::Str(s.to_string()),
    }
}

enum VarSet {
    Base,
    Config,
    Functions,
    Units,
}

impl Runner {
    pub fn new() -> Self {
        let mut values = HashMap::new();
        values.insert(vec!["config"].into(), Value::Config);
        values.insert(vec!["functions"].into(), Value::Functions);
        values.insert(vec!["units"].into(), Value::Units);
        for (name, &func) in functions::FUNCTIONS.iter() {
            values.insert(name.clone(), Value::Func(func));
        }

        let mut scales = HashMap::new();
        scales.insert(*TIME_UNIT, ScaleType::TimeMetric);
        scales.insert(*BYTE_UNIT, ScaleType::Binary);
        scales.insert(*MASS_UNIT, ScaleType::ShiftedMetric);

        Self {
            last: None,
            values,
            scales,
            default_scale: NamedValue {
                value: ScaleType::Metric,
                name: vec!["default", "scale"].into(),
            },
            display_config: DisplayConfig {
                round: NamedValue {
                    value: Some(2),
                    name: vec!["round"].into(),
                },
                large_threshold: NamedValue {
                    value: Some(1_000_000_000),
                    name: vec!["large", "threshold"].into(),
                },
                neg_exponent: NamedValue {
                    value: Some(-6),
                    name: vec!["negative", "exponent"].into(),
                },
            },
        }
    }

    fn varset_children(&self, varset: &VarSet) -> Vec<Variable> {
        match varset {
            VarSet::Base => todo!(),
            VarSet::Config => vec![
                self.default_scale.name.clone(),
                self.display_config.round.name.clone(),
                self.display_config.large_threshold.name.clone(),
                self.display_config.neg_exponent.name.clone(),
            ],
            VarSet::Functions => functions::FUNCTIONS.keys().cloned().collect(),
            VarSet::Units => KNOWN_UNITS.values().map(|&n| vec![n].into()).collect(),
        }
    }

    fn raw_resolve_varset(&self, set: &VarSet, name: &Variable) -> Option<Value> {
        match set {
            VarSet::Base => self.values.get(name).cloned(),
            VarSet::Config => {
                if &self.default_scale.name == name {
                    Some(self.default_scale.value.atom())
                } else if &self.display_config.round.name == name {
                    Some(atom_int_or(
                        self.display_config.round.value.map(|n| n as i128),
                    ))
                } else if &self.display_config.large_threshold.name == name {
                    Some(atom_int_or(
                        self.display_config.large_threshold.value.map(|n| n as i128),
                    ))
                } else if &self.display_config.neg_exponent.name == name {
                    Some(atom_int_or(
                        self.display_config.neg_exponent.value.map(|n| n as i128),
                    ))
                } else {
                    None
                }
            }
            VarSet::Functions => functions::FUNCTIONS.get(name).map(|&a| Value::Func(a)),
            VarSet::Units => {
                if name.0.len() != 1 {
                    return None;
                }

                for (dimension, unit) in KNOWN_UNITS.iter() {
                    if **unit == *name.0[0] {
                        return Some(Value::Numeric(NumericValue {
                            magnitude: 1.into(),
                            unit: *dimension,
                        }));
                    }
                }

                None
            }
        }
    }

    pub fn display_value(&self, value: Value, render_units: bool) -> String {
        match value {
            Value::Numeric(n) => self.display_numeric_value(n, render_units),
            Value::Str(s) => s,
            Value::Atom(a) => format!(":{a}"),
            Value::Bool(v) => v.to_string(),
            Value::Func(f) => {
                let mut output = String::new();
                for arg in f.arguments() {
                    if !output.is_empty() {
                        output.push(',');
                    }
                    output.push_str(arg);
                }
                format!("{output} :-> value")
            }
            v @ (Value::Config | Value::Functions | Value::Units) => {
                let mut value = String::new();

                let varset = v.to_varset();
                for name in self.varset_children(&varset) {
                    if !value.is_empty() {
                        value.push('\n');
                    }

                    let child = self.raw_resolve_varset(&varset, &name).unwrap();

                    value += &format!(
                        "{}: {}",
                        name,
                        self.display_value(child, !matches!(v, Value::Units))
                    )
                }

                value
            }
        }
    }

    fn resolve_function(
        &self,
        func: &ast::Function,
    ) -> Result<&(dyn ValueFn + Send + Sync), RunnerError> {
        match func {
            ast::Function::Ref(name) => self.resolve_path(name)?.spanned(&name.span()).cast(),
        }
    }

    fn display_numeric_value(&self, value: NumericValue, render_units: bool) -> String {
        if value.unit.is_dimensionless() {
            value.magnitude.to_string(&self.display_config)
        } else {
            match KNOWN_UNITS.get(&value.unit) {
                Some(known) if render_units => {
                    let unit = value.unit;
                    render_with_unit(
                        value,
                        known,
                        self.scales
                            .get(&unit)
                            .unwrap_or(&self.default_scale.value)
                            .steps(),
                        &self.display_config,
                    )
                }
                _ => {
                    let raw_magnitude = value.magnitude.to_string(&self.display_config);

                    let (num_unit, denum_unit) = value
                        .unit
                        .dimensions
                        .iter()
                        .filter(|&(_, &scale)| scale != 0)
                        .partition::<Unit, _>(|&(_, &scale)| scale > 0);

                    let join_units = |unit: Unit| {
                        unit.dimensions
                            .into_iter()
                            .flat_map(|(dim, scale)| match scale.abs() {
                                0 => None,
                                1 => Some(dim.to_string()),
                                s => Some(format!("{dim}{s}")),
                            })
                            .join(".")
                    };

                    let (known_numerator, numerator) = if num_unit.is_dimensionless() {
                        (false, "1".into())
                    } else {
                        match KNOWN_UNITS.get(&num_unit) {
                            Some(u) => (true, u.to_string()),
                            None => (false, join_units(num_unit)),
                        }
                    };

                    let unit = if denum_unit.is_dimensionless() {
                        numerator
                    } else {
                        numerator
                            + "/"
                            + &match KNOWN_UNITS.get(&(Unit::dimensionless() / denum_unit)) {
                                Some(u) => u.to_string(),
                                None => join_units(denum_unit),
                            }
                    };

                    if known_numerator {
                        render_with_unit(
                            value,
                            &unit,
                            self.scales
                                .get(&num_unit)
                                .unwrap_or(&self.default_scale.value)
                                .steps(),
                            &self.display_config,
                        )
                    } else {
                        format!("{raw_magnitude} {unit}")
                    }
                }
            }
        }
    }

    fn resolve_unit<'a>(&self, unit: &'a str, span: Span) -> Result<ResolvedUnit<'a>, RunnerError> {
        for (scale_unit, scale) in self.scales.iter() {
            for step in scale.steps() {
                if let ScaleRender::Override(u) = step.render
                    && u == unit
                {
                    return Ok(ResolvedUnit::Magnitude(step.order.clone(), *scale_unit));
                }
            }
        }

        let possible = KNOWN_UNITS
            .iter()
            .filter_map(|(&u, n)| unit.strip_suffix(n).map(|p| (p, u)))
            .collect_vec();

        if possible.is_empty() {
            return Err(RunnerError::InvalidUnit {
                unit: unit.to_string(),
                location: (span.start..span.end).into(),
                src: span.source.clone(),
            });
        }

        Ok(ResolvedUnit::Prefix(possible))
    }

    fn resolve_units(
        &self,
        units: &[SpannedValue<(Arc<str>, i64)>],
    ) -> Result<(ValueMagnitude, Unit), RunnerError> {
        let mut multiplier = ValueMagnitude::new(1);
        let mut unit_acc = Unit::dimensionless();

        for span in units {
            let (unit, scale) = &span.value;
            if *scale == 0 {
                continue;
            };

            let (mut unit_mult, unit) = match self.resolve_unit(unit, span.span())? {
                ResolvedUnit::Prefix(items) => {
                    let possible = items
                        .into_iter()
                        .filter_map(|(prefix, unit)| {
                            if prefix.is_empty() {
                                Some((ValueMagnitude::new(1), unit))
                            } else {
                                ScaleType::all_prefix()
                                    .find(|&(p, _)| p == prefix)
                                    .map(|(_, mult)| (mult.clone(), unit))
                            }
                        })
                        .collect_vec();

                    match possible.len() {
                        0 => {
                            return Err(RunnerError::InvalidUnit {
                                unit: unit.to_string(),
                                location: (span.start..span.end).into(),
                                src: span.source.clone(),
                            });
                        }
                        1 => possible.into_iter().next().unwrap(),
                        _ => {
                            return Err(RunnerError::AmbiguousUnit {
                                unit: unit.to_string(),
                                location: (span.start..span.end).into(),
                                src: span.source.clone(),
                            });
                        }
                    }
                }
                ResolvedUnit::Magnitude(value_magnitude, unit) => (value_magnitude, unit),
            };

            if unit == *MASS_UNIT {
                unit_mult = ValueMagnitude::div_ok(unit_mult, 1000.into());
            };

            unit_mult = unit_mult.clone().pow(
                span.span(),
                ValueMagnitude::new((scale.abs()).into()).spanned(span),
            )?;

            if *scale > 0 {
                multiplier = ValueMagnitude::mul_ok(multiplier, unit_mult);
            } else {
                multiplier = ValueMagnitude::div_ok(multiplier, unit_mult);
            }

            for (dim, dim_scale) in unit.dimensions {
                unit_acc.dimensions[dim] += scale * dim_scale;
            }
        }

        Ok((multiplier, unit_acc))
    }

    fn resolve_varset(
        &self,
        set: &VarSet,
        name: &SpannedValue<Variable>,
    ) -> Result<Value, RunnerError> {
        self.raw_resolve_varset(set, name)
            .ok_or_else(|| RunnerError::UndefinedIdentifier {
                name: (**name).clone(),
                location: (name.start..name.end).into(),
                src: name.source.clone(),
            })
    }

    fn resolve_path(&self, path: &[SpannedValue<Variable>]) -> Result<Value, RunnerError> {
        let (last, path) = path.split_last().unwrap();

        let mut varset = VarSet::Base;
        for p in path {
            let span = p.span();
            let next = self.resolve_varset(&varset, p)?;
            varset = next.spanned(&span).cast()?;
        }

        self.resolve_varset(&varset, last)
    }

    fn eval_expr(&mut self, expr: &ast::Expr) -> Result<Value, miette::Report> {
        match expr {
            ast::Expr::Literal(l) => Ok(eval_literal(l)),
            ast::Expr::Dimensioned(d) => {
                let value: NumericValue =
                    self.eval_expr(&d.expr)?.spanned(&d.expr.span()).cast()?;
                if !value.unit.is_dimensionless() {
                    return Err(CastError::from_val(
                        Value::Numeric(value).spanned(&d.expr.span()),
                        "dimensionless number",
                    )
                    .into());
                }
                let (multiplied, unit) = self.resolve_units(&d.unit)?;
                Ok(NumericValue {
                    magnitude: ValueMagnitude::mul(
                        d.span(),
                        value.magnitude.spanned(&d.expr.span()),
                        multiplied.spanned(&d.span()),
                    )?,
                    unit,
                }
                .into())
            }
            ast::Expr::Variable(vars) => Ok(self.resolve_path(vars)?),
            ast::Expr::Assign(v, e) => {
                let expr = self.eval_expr(e)?;
                self.values.insert((**v).clone(), expr.clone());
                Ok(expr)
            }
            ast::Expr::BinOp(b) => self.eval_bin_op(b),
            ast::Expr::UnaryOp(u) => self.eval_unary_op(u),
            ast::Expr::Call(c) => {
                let args = c
                    .args
                    .iter()
                    .map(|e| {
                        let span = e.span();
                        self.eval_expr(e).map(|v| v.spanned(&span))
                    })
                    .collect::<Result<_, _>>()?;
                let f = self.resolve_function(&c.fun)?;
                f.invoke(self, c.fun.span(), c.span(), args)
                    .map_err(Into::into)
            }
        }
    }

    fn eval_unary_op(&mut self, u: &SpannedValue<ast::UnaryOp>) -> Result<Value, miette::Report> {
        let value_span = u.operand.span();
        let value = self.eval_expr(&u.operand)?;
        match u.kind {
            ast::UnaryOpKind::Minus => Value::neg(u.span(), value.spanned(&value_span))
                .wrap_err("could not negate operand"),
            ast::UnaryOpKind::Plus => Ok(value),
        }
    }

    fn eval_bin_op(&mut self, b: &SpannedValue<ast::BinOp>) -> Result<Value, miette::Report> {
        let lhs = self.eval_expr(&b.lhs)?.spanned(&b.lhs.span());
        let rhs = self.eval_expr(&b.rhs)?.spanned(&b.rhs.span());
        match b.kind {
            ast::BinOpKind::Times => {
                Value::mul(b.span(), lhs, rhs).wrap_err("could not multiply operands")
            }
            ast::BinOpKind::Modulo => {
                Value::rem(b.span(), lhs, rhs).wrap_err("could not take modulus of operands")
            }
            ast::BinOpKind::Divide => {
                Value::div(b.span(), lhs, rhs).with_context(|| "could not divide operands")
            }
            ast::BinOpKind::Sum => {
                Value::add(b.span(), lhs, rhs).wrap_err("could not add operands")
            }
            ast::BinOpKind::Diff => {
                Value::sub(b.span(), lhs, rhs).wrap_err("could not substract operands")
            }
            ast::BinOpKind::LeftShift => {
                Value::shl(b.span(), lhs, rhs).wrap_err("could not shift operands")
            }
            ast::BinOpKind::RightShift => {
                Value::shr(b.span(), lhs, rhs).wrap_err("could not shift operands")
            }
            ast::BinOpKind::BinaryOr => {
                Value::bit_or(b.span(), lhs, rhs).wrap_err("could not or operands")
            }
            ast::BinOpKind::BinaryAnd => {
                Value::bit_and(b.span(), lhs, rhs).wrap_err("could not and operands")
            }
            ast::BinOpKind::BinaryXor => {
                Value::bit_xor(b.span(), lhs, rhs).wrap_err("could not xor operands")
            }
            ast::BinOpKind::Greater => Ok(Value::Bool(
                Value::cmp(b.span(), lhs, rhs)
                    .wrap_err("could not compare values")?
                    .is_gt(),
            )),
            ast::BinOpKind::GreaterOrEqual => Ok(Value::Bool(
                Value::cmp(b.span(), lhs, rhs)
                    .wrap_err("could not compare values")?
                    .is_ge(),
            )),
            ast::BinOpKind::Lesser => Ok(Value::Bool(
                Value::cmp(b.span(), lhs, rhs)
                    .wrap_err("could not compare values")?
                    .is_lt(),
            )),
            ast::BinOpKind::LesserOrEqual => Ok(Value::Bool(
                Value::cmp(b.span(), lhs, rhs)
                    .wrap_err("could not compare values")?
                    .is_le(),
            )),
            ast::BinOpKind::LogicalEquals => Ok(Value::Bool(
                Value::eq(b.span(), lhs, rhs).wrap_err("could not check equality")?,
            )),
            ast::BinOpKind::Different => Ok(Value::Bool(
                !Value::eq(b.span(), lhs, rhs).wrap_err("could not check equality")?,
            )),
            ast::BinOpKind::LogicalOr => {
                let lhs = lhs.cast()?;
                let rhs = rhs.cast()?;

                Ok(Value::Bool(lhs || rhs))
            }
            ast::BinOpKind::LogicalAnd => {
                let lhs = lhs.cast()?;
                let rhs = rhs.cast()?;

                Ok(Value::Bool(lhs && rhs))
            }
            ast::BinOpKind::Power => {
                Value::pow(b.span(), lhs, rhs).wrap_err("could not exponentiate operands")
            }
        }
    }

    pub fn handle_command(
        &mut self,
        name: SpannedValue<Arc<str>>,
        value: Option<Value>,
    ) -> Result<(), RunnerError> {
        let location = (name.start..name.end).into();
        let src = name.source;

        match commands::COMMANDS.get(&*name.value) {
            Some(cmd) => cmd.run(self, value, location, src),
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
                return Ok(None);
            }
            ast::InputStatement::Expr(e) => self.eval_expr(&e)?,
            ast::InputStatement::LastRedirect(func) => match &self.last {
                None => Err(RunnerError::NoStoredValue)?,
                Some(last) => {
                    let f = self.resolve_function(&func)?;
                    f.invoke(self, func.span(), span.clone(), vec![last.clone()])?
                }
            },
        };
        self.last = Some(value.clone().spanned(&span));
        Ok(Some(value))
    }
}

impl Default for Runner {
    fn default() -> Self {
        Self::new()
    }
}
