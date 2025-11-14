use std::collections::HashMap;

use miette::SourceSpan;
use once_cell::sync::Lazy;

use crate::span::MaybeNamed;

use super::{value::BYTE_UNIT, NumericValue, RunnerError, ScaleType, Value};

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
            Value::Atom(v) if &*v == "none" => state.display_config.round = None,
            Value::Numeric(NumericValue { magnitude: n, unit })
                if unit.is_dimensionless() && n.is_usize() =>
            {
                state.display_config.round = Some(n.as_usize())
            }
            v => {
                return Err(RunnerError::InvalidCommandValue {
                    val: state.display_value(v),
                    location,
                    src,
                })
            }
        };
        Ok(())
    }
}

struct LargeThreshold;
impl ParamRunnerCommand for LargeThreshold {
    fn name(&self) -> &'static str {
        "large_threshold"
    }

    fn help(&self) -> &'static str {
        "Values above which scientific notation is used. Values: :none | <number>, default: 1_000_000_000"
    }

    fn run(
        &self,
        state: &mut super::Runner,
        value: Value,
        location: SourceSpan,
        src: MaybeNamed,
    ) -> Result<(), RunnerError> {
        match value {
            Value::Atom(v) if &*v == "none" => state.display_config.large_threshold = None,
            Value::Numeric(NumericValue { magnitude: n, unit })
                if unit.is_dimensionless() && n.is_usize() =>
            {
                state.display_config.large_threshold = Some(n.as_usize())
            }
            v => {
                return Err(RunnerError::InvalidCommandValue {
                    val: state.display_value(v),
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
                    val: state.display_value(v),
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
                    val: state.display_value(v),
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
                    val: state.display_value(v),
                    location,
                    src,
                })
            }
        }
        Ok(())
    }
}

struct Functions;
impl NoParamCommand for Functions {
    fn name(&self) -> &'static str {
        "functions"
    }

    fn help(&self) -> &'static str {
        "Display a list of available functions"
    }

    fn run(&self, _: &mut super::Runner) -> Result<(), RunnerError> {
        println!("Functions:");
        for name in super::functions::FUNCTIONS.keys() {
            println!("  - {}", name.0.join(" "))
        }

        Ok(())
    }
}

pub(super) static COMMANDS: Lazy<HashMap<&'static str, Box<dyn RunnerCommand + Send + Sync>>> =
    Lazy::new(|| {
        let cmds: Vec<Box<dyn RunnerCommand + Send + Sync>> = vec![
            Box::new(PR(Round)),
            Box::new(PR(LargeThreshold)),
            Box::new(PR(ByteScale)),
            Box::new(PR(DefaultScale)),
            Box::new(Help),
            Box::new(NP(Functions)),
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

struct PR<T>(T);

impl<T: ParamRunnerCommand> RunnerCommand for PR<T> {
    fn name(&self) -> &'static str {
        self.0.name()
    }

    fn help(&self) -> &'static str {
        self.0.help()
    }

    fn run(
        &self,
        state: &mut super::Runner,
        value: Option<Value>,
        location: SourceSpan,
        src: MaybeNamed,
    ) -> Result<(), RunnerError> {
        match value {
            Some(v) => self.0.run(state, v, location, src),
            None => Err(RunnerError::MissingCommandValue { location, src }),
        }
    }
}

trait NoParamCommand {
    fn name(&self) -> &'static str;
    fn help(&self) -> &'static str;

    fn run(&self, state: &mut super::Runner) -> Result<(), RunnerError>;
}

struct NP<T>(T);
impl<T: NoParamCommand> RunnerCommand for NP<T> {
    fn name(&self) -> &'static str {
        self.0.name()
    }

    fn help(&self) -> &'static str {
        self.0.help()
    }

    fn run(
        &self,
        state: &mut super::Runner,
        value: Option<Value>,
        location: SourceSpan,
        src: MaybeNamed,
    ) -> Result<(), RunnerError> {
        match value {
            Some(v) => Err(RunnerError::InvalidCommandValue {
                val: state.display_value(v),
                location,
                src,
            }),
            None => self.0.run(state),
        }
    }
}
