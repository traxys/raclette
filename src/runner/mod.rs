use std::{collections::HashMap, sync::Arc};

use either::Either;
use itertools::Itertools;
use miette::{Context, Diagnostic, SourceSpan};

use crate::{
    ast::{self, Variable},
    span::{MaybeNamed, SpannedValue, SpanningExt},
};

use functions::ValueFn;
use value::{
    Dimension, ScaleRender, ScaleType, Unit, ValueMagnitude, BYTE_UNIT, KNOWN_UNITS, MASS_UNIT,
    TIME_UNIT,
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
                Value::Numeric(n) => match n.magnitude {
                    ValueMagnitude::Int(_) => "int",
                    ValueMagnitude::Float(_) => "float",
                },
                v => v.ty(),
            },
            location: (val.start..val.end).into(),
            src: val.source,
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
    #[error("Value is not an unsigned integer")]
    UintConversion {
        #[label("This number is not an unsigned integer")]
        location: SourceSpan,
        #[source_code]
        src: MaybeNamed,
    },
    #[error("Operation on NaN")]
    NaN {
        #[label("This value is NaN")]
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
    #[error("Could not cast value")]
    #[diagnostic(transparent)]
    Cast(#[from] CastError),
}

pub struct Runner {
    last: Option<SpannedValue<Value>>,
    values: HashMap<Variable, Value>,
    scales: HashMap<Unit, ScaleType>,
    default_scale: ScaleType,
    round: Option<usize>,
}

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
        scales.insert(*TIME_UNIT, ScaleType::TimeMetric);
        scales.insert(*BYTE_UNIT, ScaleType::Binary);
        scales.insert(*MASS_UNIT, ScaleType::ShiftedMetric);

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
            Value::Bool(v) => v.to_string(),
        }
    }

    fn resolve_function(
        &self,
        func: &ast::Function,
    ) -> Result<&(dyn ValueFn + Send + Sync), RunnerError> {
        match func {
            ast::Function::Ref(name) => functions::FUNCTIONS.get(name).copied().ok_or_else(|| {
                RunnerError::UndefinedIdentifier {
                    name: name.value.clone(),
                    location: (name.start..name.end).into(),
                    src: name.source.clone(),
                }
            }),
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
                                Dimension::Mass => "kg",
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

                    let mut magnitude = value.magnitude.clone().into_float();
                    let mut abs_magnitude = magnitude.clone().abs();
                    let mut render = scale_prefixes[0].render;

                    let last_unit = scale_prefixes.iter().last().unwrap();

                    if magnitude == 0. {
                        render = ScaleRender::AsIs;
                    } else if abs_magnitude >= scale_prefixes[0].order {
                        magnitude /= scale_prefixes[0].order;
                        abs_magnitude /= scale_prefixes[0].order;
                    } else if abs_magnitude < last_unit.order {
                        /* Smaller than the smallest unit */
                        magnitude /= last_unit.order;
                        abs_magnitude /= last_unit.order;
                        render = last_unit.render;
                    } else {
                        for (&large, &small) in
                            scale_prefixes.iter().zip(scale_prefixes.iter().skip(1))
                        {
                            if abs_magnitude < large.order && abs_magnitude >= small.order {
                                abs_magnitude /= small.order;
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

                    let magnitude = match abs_magnitude == (abs_magnitude as i64) as f64 {
                        true => (magnitude as i64).to_string(),
                        false => match self.round {
                            Some(r) if abs_magnitude >= 0.1f64.powi(r as i32) * 0.98 => {
                                format!("{:.*}", r, magnitude)
                            }
                            _ => magnitude.to_string(),
                        },
                    };
                    format!("{magnitude} {unit_part}")
                }
            }
        }
    }

    fn resolve_units(
        &self,
        units: &[SpannedValue<(Arc<str>, i16)>],
    ) -> Result<(ValueMagnitude, Unit), RunnerError> {
        let mut multiplier = 1.;
        let mut unit_acc = Unit::dimensionless();

        for span in units {
            let (unit, scale) = &span.value;
            if *scale == 0 {
                continue;
            };

            let (prefix, &real_unit) = match KNOWN_UNITS.iter().find(|(_, n)| unit.ends_with(**n)) {
                Some((u, n)) => (unit.strip_suffix(n).unwrap(), u),
                None => {
                    return Err(RunnerError::InvalidUnit {
                        unit: unit.to_string(),
                        location: (span.start..span.end).into(),
                        src: span.source.clone(),
                    })
                }
            };

            if real_unit == *MASS_UNIT {
                multiplier /= 1000.;
            };

            if !prefix.is_empty() {
                match ScaleType::all_prefix().find(|&(p, _)| p == prefix) {
                    None => {
                        return Err(RunnerError::InvalidUnit {
                            unit: unit.to_string(),
                            location: (span.start..span.end).into(),
                            src: span.source.clone(),
                        });
                    }
                    Some((_, mult)) => {
                        if *scale > 0 {
                            multiplier *= mult;
                        } else {
                            multiplier /= mult;
                        }
                    }
                }
            };

            for (dim, dim_scale) in real_unit.dimensions {
                unit_acc.dimensions[dim] += scale * dim_scale;
            }
        }

        Ok((
            if multiplier < 1. {
                ValueMagnitude::Float(multiplier)
            } else {
                ValueMagnitude::Int(multiplier as i64)
            },
            unit_acc,
        ))
    }

    fn eval_expr(&mut self, expr: &ast::Expr) -> Result<Value, miette::Report> {
        match expr {
            ast::Expr::Literal(l) => Ok(self.eval_literal(l)),
            ast::Expr::Dimensioned(l, u) => {
                let value: NumericValue = self.eval_expr(l)?.spanned(&l.span()).try_into()?;
                if !value.unit.is_dimensionless() {
                    return Err(CastError::from_val(
                        Value::Numeric(value).spanned(&l.span()),
                        "dimensionless number",
                    )
                    .into());
                }
                let (multiplied, unit) = self.resolve_units(u)?;
                Ok(NumericValue {
                    magnitude: value.magnitude * multiplied,
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
                f.invoke(args)
            }
        }
    }

    fn eval_unary_op(&mut self, u: &ast::UnaryOp) -> Result<Value, miette::Report> {
        let value_span = u.operand.span();
        let value = self.eval_expr(&u.operand)?;
        match u.kind {
            ast::UnaryOpKind::Minus => {
                (-value.spanned(&value_span)).wrap_err("could not negate operand")
            }
            ast::UnaryOpKind::Plus => Ok(value),
        }
    }

    fn eval_bin_op(&mut self, b: &ast::BinOp) -> Result<Value, miette::Report> {
        let lhs_span = b.lhs.span();
        let lhs = self.eval_expr(&b.lhs)?;
        let rhs_span = b.rhs.span();
        let rhs = self.eval_expr(&b.rhs)?;
        match b.kind {
            ast::BinOpKind::Times => (lhs.spanned(&lhs_span) * rhs.spanned(&rhs_span))
                .wrap_err("could not multiply operands"),
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
            ast::BinOpKind::Sum => {
                (lhs.spanned(&lhs_span) + rhs.spanned(&rhs_span)).wrap_err("could not add operands")
            }
            ast::BinOpKind::Diff => (lhs.spanned(&lhs_span) - rhs.spanned(&rhs_span))
                .wrap_err("could not substract operands"),
            ast::BinOpKind::LeftShift => (lhs.spanned(&lhs_span) << rhs.spanned(&rhs_span))
                .wrap_err("could not shift operands"),
            ast::BinOpKind::RightShift => (lhs.spanned(&lhs_span) >> rhs.spanned(&rhs_span))
                .wrap_err("could not shift operands"),
            ast::BinOpKind::LogicalOr => {
                (lhs.spanned(&lhs_span) | rhs.spanned(&rhs_span)).wrap_err("could not or operands")
            }
            ast::BinOpKind::LogicalAnd => {
                (lhs.spanned(&lhs_span) & rhs.spanned(&rhs_span)).wrap_err("could not and operands")
            }
            ast::BinOpKind::LogicalXor => {
                (lhs.spanned(&lhs_span) ^ rhs.spanned(&rhs_span)).wrap_err("could not xor operands")
            }
            ast::BinOpKind::Greater => Ok(Value::Bool(
                lhs.spanned(&lhs_span)
                    .cmp(rhs.spanned(&rhs_span))
                    .wrap_err("could not compare values")?
                    .is_gt(),
            )),
            ast::BinOpKind::GreaterOrEqual => Ok(Value::Bool(
                lhs.spanned(&lhs_span)
                    .cmp(rhs.spanned(&rhs_span))
                    .wrap_err("could not compare values")?
                    .is_ge(),
            )),
            ast::BinOpKind::Lesser => Ok(Value::Bool(
                lhs.spanned(&lhs_span)
                    .cmp(rhs.spanned(&rhs_span))
                    .wrap_err("could not compare values")?
                    .is_lt(),
            )),
            ast::BinOpKind::LesserOrEqual => Ok(Value::Bool(
                lhs.spanned(&lhs_span)
                    .cmp(rhs.spanned(&rhs_span))
                    .wrap_err("could not compare values")?
                    .is_le(),
            )),
            ast::BinOpKind::LogicalEquals => Ok(Value::Bool(
                lhs.spanned(&lhs_span)
                    .eq(rhs.spanned(&rhs_span))
                    .wrap_err("could not check equality")?,
            )),
            ast::BinOpKind::Different => Ok(Value::Bool(
                !lhs.spanned(&lhs_span)
                    .eq(rhs.spanned(&rhs_span))
                    .wrap_err("could not check equality")?,
            )),
        }
    }

    fn eval_literal(&mut self, lit: &ast::Literal) -> Value {
        match lit {
            ast::Literal::Number(v) => Value::Numeric(NumericValue {
                magnitude: ValueMagnitude::Int(v.clone()),
                unit: Unit::dimensionless(),
            }),
            ast::Literal::Float(v) => Value::Numeric(NumericValue {
                magnitude: ValueMagnitude::Float(v.clone()),
                unit: Unit::dimensionless(),
            }),
            ast::Literal::Atom(a) => Value::Atom(a.clone()),
            &ast::Literal::Bool(b) => Value::Bool(b),
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
                return Ok(self.last.clone().map(|v| v.value));
            }
            ast::InputStatement::Expr(e) => self.eval_expr(&e)?,
            ast::InputStatement::LastRedirect(func) => match &self.last {
                None => Err(RunnerError::NoStoredValue)?,
                Some(last) => {
                    let f = self.resolve_function(&func)?;
                    f.invoke(vec![last.clone()])?
                }
            },
        };
        self.last = Some(value.clone().spanned(&span));
        Ok(Some(value))
    }
}
