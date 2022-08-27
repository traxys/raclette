use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod interpreter;
pub mod span;

pub use interpreter::value::{Val, Value};

lalrpop_mod!(
    #[allow(
        clippy::just_underscores_and_digits,
        clippy::clone_on_copy,
        clippy::too_many_arguments,
        clippy::type_complexity,
    )]
    pub raclette
);
