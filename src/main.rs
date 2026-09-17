use std::cell::RefCell;

use clap::Parser;
use itertools::Itertools;
use lalrpop_util::lalrpop_mod;
use miette::{Context, Diagnostic, IntoDiagnostic, Result, SourceCode, SourceSpan};
use rustyline::{
    Editor, Helper, completion::Completer, config::Configurer, error::ReadlineError,
    highlight::Highlighter, hint::Hinter, history::FileHistory, validate::Validator,
};

use crate::{
    ast::{InputStatement, Variable},
    span::SpannedValue,
};

#[derive(Parser, Debug)]
struct Args {
    expr: Vec<String>,
}

lalrpop_mod!(
    #[allow(
        clippy::just_underscores_and_digits,
        clippy::clone_on_copy,
        clippy::too_many_arguments,
        clippy::type_complexity,
    )]
    pub calc
);

mod ast;
mod runner;
mod span;

#[derive(thiserror::Error, Debug, Diagnostic)]
pub enum ParseError {
    #[diagnostic(code(calc::parsing::invalid_token))]
    #[error("Token is invalid")]
    Invalid {
        #[label]
        position: SourceSpan,
    },
    #[diagnostic(code(calc::parsing::unrecognized_token))]
    #[error("Unrecognized token: {token}.\nExpected one of: {}", expected.join(", "))]
    Unrecognized {
        token: ast::Token,
        expected: Vec<String>,
        #[label("this token is not valid in this context")]
        location: SourceSpan,
    },
    #[diagnostic(code(calc::parsing::unknown_token))]
    #[error("token error for {token}: {error}")]
    TokenError {
        token: String,
        error: ast::TokenError,
        #[label("this is not a valid token")]
        location: SourceSpan,
    },
    #[diagnostic(code(calc::parsing::eof))]
    #[error("Unexpected end of file.\nExpected one of: {}", expected.join(", "))]
    Eof {
        expected: Vec<String>,
        #[label("end of file here")]
        location: SourceSpan,
    },
    #[diagnostic(code(calc::parsing::out_of_range))]
    #[error("Literal out of range\nExpected a value in {ty}")]
    OutOfRange {
        ty: &'static str,
        num: i128,
        #[label("this literal")]
        location: SourceSpan,
    },
}

type LalrpopParseError = lalrpop_util::ParseError<usize, ast::Token, ast::UserParseError>;

trait ParseDiagnosticExt<T> {
    fn into_parse_diagnostic(self) -> Result<T, ParseError>;
    fn into_report<S>(self, source: S) -> miette::Result<T>
    where
        Self: Sized,
        S: SourceCode + 'static,
    {
        let report: miette::Result<T> = self.into_parse_diagnostic().map_err(Into::into);
        report.map_err(|e| e.with_source_code(source))
    }
}

impl<T> ParseDiagnosticExt<T> for Result<T, LalrpopParseError> {
    fn into_parse_diagnostic(self) -> Result<T, ParseError> {
        match self {
            Ok(v) => Ok(v),
            Err(LalrpopParseError::InvalidToken { location }) => Err(ParseError::Invalid {
                position: location.into(),
            }),
            Err(LalrpopParseError::UnrecognizedToken { token, expected }) => {
                let (start, token, end) = token;
                Err(ParseError::Unrecognized {
                    token,
                    expected,
                    location: (start..end).into(),
                })
            }
            Err(LalrpopParseError::User { error }) => match error {
                ast::UserParseError::TokenError(u) => Err(ParseError::TokenError {
                    token: u.token,
                    error: u.error,
                    location: u.span.into(),
                }),
                ast::UserParseError::OutOfRange { ty, num, span } => Err(ParseError::OutOfRange {
                    ty,
                    num,
                    location: span.into(),
                }),
            },
            Err(LalrpopParseError::UnrecognizedEof { location, expected }) => {
                Err(ParseError::Eof {
                    expected,
                    location: location.into(),
                })
            }
            Err(LalrpopParseError::ExtraToken { token: _ }) => todo!(),
        }
    }
}

struct RacletteHelper {
    parser: calc::InputStatementParser,
    runner: RefCell<runner::Runner>,
}

impl Hinter for &RacletteHelper {
    type Hint = String;
}

impl Validator for &RacletteHelper {}

impl Highlighter for &RacletteHelper {}

impl Completer for &RacletteHelper {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let Ok(parsed) = self.parser.parse(&line.into(), ast::lexer(line)) else {
            return Ok((pos, vec![]));
        };

        if !(parsed.start..=parsed.end).contains(&pos) {
            return Ok((pos, vec![]));
        }

        fn get_chain(
            pos: usize,
            v: &[SpannedValue<Variable>],
        ) -> Option<&[SpannedValue<Variable>]> {
            let (index, _) = v
                .iter()
                .enumerate()
                .find(|(_, v)| (v.start..=v.end).contains(&pos))?;

            Some(&v[0..=index])
        }

        fn get_chain_in_expr(pos: usize, expr: &ast::Expr) -> Option<&[SpannedValue<Variable>]> {
            match expr {
                ast::Expr::Literal(_) => None,
                ast::Expr::Dimensioned(d) => get_chain_in_expr(pos, &d.expr),
                ast::Expr::Variable(v) => get_chain(pos, v),
                ast::Expr::Assign(v, e) => get_chain(pos, v).or_else(|| get_chain_in_expr(pos, e)),
                ast::Expr::BinOp(b) => {
                    get_chain_in_expr(pos, &b.lhs).or_else(|| get_chain_in_expr(pos, &b.rhs))
                }
                ast::Expr::Call(c) => get_chain_in_expr(pos, &c.fun.value).or_else(|| {
                    for arg in &c.args {
                        match get_chain_in_expr(pos, arg) {
                            Some(v) => return Some(v),
                            None => continue,
                        }
                    }

                    None
                }),
                ast::Expr::UnaryOp(u) => get_chain_in_expr(pos, &u.operand),
            }
        }

        let Some(variable) = (match &parsed.value {
            InputStatement::Expr(expr) => get_chain_in_expr(pos, expr),
            InputStatement::LastRedirect(v) => get_chain_in_expr(pos, &v.value),
        }) else {
            return Ok((pos, vec![]));
        };

        let (last, path) = variable.split_last().unwrap();
        let Ok(children) = self.runner.borrow().path_variables(path) else {
            return Ok((pos, vec![]));
        };

        let last_len = last.end - last.start;

        let mut completions = Vec::new();
        for child in children {
            if child.starts_with(last) {
                let child = child.0.join(" ");
                completions.push(child[last_len..].to_string());
            }
        }

        Ok((last.end, completions))
    }
}

impl Helper for &RacletteHelper {}

fn main() -> Result<()> {
    let args = Args::parse();

    let state = RacletteHelper {
        parser: calc::InputStatementParser::new(),
        runner: RefCell::new(runner::Runner::new()),
    };

    match args.expr.is_empty() {
        false => {
            let expr = args.expr.iter().join(" ");
            let parsed = state
                .parser
                .parse(&expr.as_str().into(), ast::lexer(&expr))
                .into_report(expr.clone())?;

            let mut runner = state.runner.borrow_mut();
            if let Some(value) = runner.eval_input_statement(parsed)? {
                println!("{}", runner.display_value(value, true, 0))
            }
        }
        true => {
            let data_dir = dirs_next::data_dir().map(|mut p| {
                p.push("raclette");
                p
            });

            if let Some(d) = &data_dir {
                std::fs::create_dir_all(d)
                    .into_diagnostic()
                    .wrap_err("Could not create data directory")?;
            };

            let path = data_dir
                .map(|mut p| {
                    p.push("history");
                    p
                })
                .unwrap_or_else(|| "raclette-history".into());

            let mut rl = Editor::<_, FileHistory>::new().into_diagnostic()?;
            rl.set_max_history_size(1024).into_diagnostic()?;
            rl.set_helper(Some(&state));

            if let Err(e) = rl.load_history(&path)
                && path.exists()
            {
                println!("Error loading history: {:?}", e)
            };

            loop {
                match rl.readline(">> ") {
                    Ok(line) => {
                        if let Err(e) = rl.add_history_entry(&line) {
                            println!("History error: {e:?}");
                        };

                        let parsed = match state
                            .parser
                            .parse(&line.as_str().into(), ast::lexer(&line))
                            .into_report(line.clone())
                        {
                            Ok(p) => p,
                            Err(e) => {
                                println!("Parsing error: {:?}", e);
                                continue;
                            }
                        };

                        let mut runner = state.runner.borrow_mut();
                        match runner.eval_input_statement(parsed) {
                            Err(e) => {
                                println!("Runtime error:\n{e:?}");
                                continue;
                            }
                            Ok(Some(v)) => println!("{}", runner.display_value(v, true, 0)),
                            Ok(None) => (),
                        };
                    }
                    Err(ReadlineError::Interrupted) => {
                        println!("Interrupted");
                        continue;
                    }
                    Err(ReadlineError::Eof) => {
                        break;
                    }
                    Err(e) => {
                        println!("Error: {e}");
                        break;
                    }
                }

                rl.append_history(&path).into_diagnostic()?;
            }
        }
    }

    Ok(())
}
