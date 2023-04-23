use std::collections::HashMap;

use miette::SourceSpan;
use once_cell::sync::Lazy;

use crate::span::MaybeNamed;

use super::{NumericValue, RunnerError, ScaleType, Value, ValueMagnitude, BYTE_UNIT};

struct Round;
impl ParamRunnerCommand for Round {
    fn name(&self) -> &'static str {
        "round"
    }

    fn help(&self) -> &'static str {
        "Number of decimals to print. Values: :none | <number>, default: 2"
    }

    fn run(
        &self,
        state: &mut super::Runner,
        value: Value,
        location: SourceSpan,
        src: MaybeNamed,
    ) -> Result<(), RunnerError> {
        match value {
            Value::Atom(v) if &*v == "none" => state.round = None,
            Value::Numeric(NumericValue {
                magnitude: ValueMagnitude::Int(n),
                unit,
            }) if unit.is_dimensionless() => state.round = Some(n as _),
            v => {
                return Err(RunnerError::InvalidCommandValue {
                    val: state.display_value(&v),
                    location,
                    src,
                })
            }
        };
        Ok(())
    }
}

struct ByteScale;
impl ParamRunnerCommand for ByteScale {
    fn name(&self) -> &'static str {
        "byte_scale"
    }

    fn help(&self) -> &'static str {
        "Set the scale to use for binary numbers. Values: :binary | :metric, default: :binary"
    }

    fn run(
        &self,
        state: &mut super::Runner,
        value: Value,
        location: SourceSpan,
        src: MaybeNamed,
    ) -> Result<(), RunnerError> {
        match value {
            Value::Atom(v) if &*v == "binary" => {
                state.scales.insert(*BYTE_UNIT, ScaleType::Binary);
            }
            Value::Atom(v) if &*v == "metric" => {
                state.scales.insert(*BYTE_UNIT, ScaleType::Metric);
            }
            v => {
                return Err(RunnerError::InvalidCommandValue {
                    val: state.display_value(&v),
                    location,
                    src,
                })
            }
        }
        Ok(())
    }
}

struct DefaultScale;
impl ParamRunnerCommand for DefaultScale {
    fn name(&self) -> &'static str {
        "default_scale"
    }

    fn help(&self) -> &'static str {
        "Set the scale used by default. Values: :binary | :metric, default: :binary"
    }

    fn run(
        &self,
        state: &mut super::Runner,
        value: Value,
        location: SourceSpan,
        src: MaybeNamed,
    ) -> Result<(), RunnerError> {
        match value {
            Value::Atom(v) if &*v == "binary" => state.default_scale = ScaleType::Binary,
            Value::Atom(v) if &*v == "metric" => state.default_scale = ScaleType::Metric,
            v => {
                return Err(RunnerError::InvalidCommandValue {
                    val: state.display_value(&v),
                    location,
                    src,
                })
            }
        }
        Ok(())
    }
}

struct Help;
impl RunnerCommand for Help {
    fn name(&self) -> &'static str {
        "help"
    }

    fn help(&self) -> &'static str {
        "Display the help for all commands. Optional value: :<command-name>"
    }

    fn run(
        &self,
        state: &mut super::Runner,
        value: Option<Value>,
        location: SourceSpan,
        src: MaybeNamed,
    ) -> Result<(), RunnerError> {
        match value {
            None => {
                println!("Commands:");
                for (name, command) in &*COMMANDS {
                    println!("  - {name}: {}", command.help());
                }
            }
            Some(Value::Atom(v)) if COMMANDS.contains_key(&*v) => {
                println!("{}", COMMANDS[&*v].help());
            }
            Some(v) => {
                return Err(RunnerError::InvalidCommandValue {
                    val: state.display_value(&v),
                    location,
                    src,
                })
            }
        }
        Ok(())
    }
}

pub(super) static COMMANDS: Lazy<HashMap<&'static str, Box<dyn RunnerCommand + Send + Sync>>> =
    Lazy::new(|| {
        let cmds: Vec<Box<dyn RunnerCommand + Send + Sync>> = vec![
            Box::new(Round),
            Box::new(ByteScale),
            Box::new(DefaultScale),
            Box::new(Help),
        ];
        let mut commands = HashMap::new();

        for command in cmds {
            commands.insert(command.name(), command);
        }

        commands
    });

pub(super) trait RunnerCommand {
    fn name(&self) -> &'static str;
    fn help(&self) -> &'static str;

    fn run(
        &self,
        state: &mut super::Runner,
        value: Option<Value>,
        location: SourceSpan,
        src: MaybeNamed,
    ) -> Result<(), RunnerError>;
}

trait ParamRunnerCommand {
    fn name(&self) -> &'static str;
    fn help(&self) -> &'static str;

    fn run(
        &self,
        state: &mut super::Runner,
        value: Value,
        location: SourceSpan,
        src: MaybeNamed,
    ) -> Result<(), RunnerError>;
}

impl<T: ParamRunnerCommand> RunnerCommand for T {
    fn name(&self) -> &'static str {
        self.name()
    }

    fn help(&self) -> &'static str {
        self.help()
    }

    fn run(
        &self,
        state: &mut super::Runner,
        value: Option<Value>,
        location: SourceSpan,
        src: MaybeNamed,
    ) -> Result<(), RunnerError> {
        match value {
            Some(v) => self.run(state, v, location, src),
            None => Err(RunnerError::MissingCommandValue { location, src }),
        }
    }
}
