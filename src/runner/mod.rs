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
                Value::Numeric(n) => n.magnitude.ty(),
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
    #[error("Could not cast value")]
    #[diagnostic(transparent)]
    Cast(#[from] CastError),
}

pub struct Runner {
    last: Option<SpannedValue<Value>>,
    values: HashMap<Variable, Value>,
    scales: HashMap<Unit, ScaleType>,
    default_scale: ScaleType,
    display_config: DisplayConfig,
}

pub struct DisplayConfig {
    round: Option<usize>,
    large_threshold: Option<usize>,
}

impl Runner {
    pub fn new() -> Self {
        let values = HashMap::new();

        let mut scales = HashMap::new();
        scales.insert(*TIME_UNIT, ScaleType::TimeMetric);
        scales.insert(*BYTE_UNIT, ScaleType::Binary);
        scales.insert(*MASS_UNIT, ScaleType::ShiftedMetric);

        Self {
            last: None,
            values,
            scales,
            default_scale: ScaleType::Metric,
            display_config: DisplayConfig {
                round: Some(2),
                large_threshold: Some(1_000_000_000),
            },
        }
    }

    pub fn display_value(&self, value: Value) -> String {
        match value {
            Value::Numeric(n) => self.display_numeric_value(n),
            Value::Str(s) => s,
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

    fn display_numeric_value(&self, value: NumericValue) -> String {
        if value.unit.is_dimensionless() {
            value.magnitude.to_string(&self.display_config)
        } else {
            match KNOWN_UNITS.get(&value.unit) {
                None => {
                    let raw_magnitude = value.magnitude.to_string(&self.display_config);
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

                    let mut magnitude = value.magnitude;
                    let mut render = scale_prefixes[0].render;

                    let last_unit = scale_prefixes.iter().last().unwrap();

                    if magnitude.is_zero() {
                        render = ScaleRender::AsIs;
                    } else if magnitude.ge_abs(&scale_prefixes[0].order) {
                        magnitude =
                            ValueMagnitude::div_ok(magnitude, scale_prefixes[0].order.clone());
                    } else if magnitude.lt_abs(&last_unit.order) {
                        /* Smaller than the smallest unit */
                        magnitude = ValueMagnitude::div_ok(magnitude, last_unit.order.clone());
                        render = last_unit.render;
                    } else {
                        for (large, small) in
                            scale_prefixes.iter().zip(scale_prefixes.iter().skip(1))
                        {
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
                            Either::Right(format!("{p}{u}"))
                        }
                        ScaleRender::AsIs => Either::Left(*u),
                    };

                    let scaled_magnitude = magnitude.to_string(&self.display_config);
                    format!("{scaled_magnitude} {unit_part}")
                }
            }
        }
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
                multiplier = ValueMagnitude::div_ok(multiplier, 1000.into());
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
                            multiplier = ValueMagnitude::mul_ok(multiplier, mult.clone());
                        } else {
                            multiplier = ValueMagnitude::div_ok(multiplier, mult.clone());
                        }
                    }
                }
            };

            for (dim, dim_scale) in real_unit.dimensions {
                unit_acc.dimensions[dim] += scale * dim_scale;
            }
        }

        Ok((multiplier, unit_acc))
    }

    fn eval_expr(&mut self, expr: &ast::Expr) -> Result<Value, miette::Report> {
        match expr {
            ast::Expr::Literal(l) => Ok(self.eval_literal(l)),
            ast::Expr::Dimensioned(d) => {
                let value: NumericValue = self
                    .eval_expr(&d.expr)?
                    .spanned(&d.expr.span())
                    .try_into()?;
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
                f.invoke(c.fun.span(), c.span(), args).map_err(Into::into)
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
                let lhs = lhs.try_into()?;
                let rhs = rhs.try_into()?;

                Ok(Value::Bool(lhs || rhs))
            }
            ast::BinOpKind::LogicalAnd => {
                let lhs = lhs.try_into()?;
                let rhs = rhs.try_into()?;

                Ok(Value::Bool(lhs && rhs))
            }
            ast::BinOpKind::Power => {
                Value::pow(b.span(), lhs, rhs).wrap_err("could not exponentiate operands")
            }
        }
    }

    fn eval_literal(&mut self, lit: &ast::Literal) -> Value {
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
                    f.invoke(func.span(), span.clone(), vec![last.clone()])?
                }
            },
        };
        self.last = Some(value.clone().spanned(&span));
        Ok(Some(value))
    }
}
