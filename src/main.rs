use std::path::PathBuf;

use clap::Parser;
use lalrpop_util::lalrpop_mod;
use miette::{Context, Diagnostic, IntoDiagnostic, Result, SourceCode, SourceSpan};
use rustyline::{error::ReadlineError, Editor};

#[derive(Parser, Debug)]
struct Args {
    #[clap(short, long)]
    file: Option<PathBuf>,
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
enum ParseError {
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
    #[error("Unknown token: {token}")]
    Unknown {
        token: String,
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
}

type LalrpopParseError = lalrpop_util::ParseError<usize, ast::Token, ast::UnknownToken>;

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
            Err(LalrpopParseError::User {
                error: ast::UnknownToken { token, span },
            }) => Err(ParseError::Unknown {
                token,
                location: span.into(),
            }),
            Err(LalrpopParseError::UnrecognizedEOF { location, expected }) => {
                Err(ParseError::Eof {
                    expected,
                    location: location.into(),
                })
            }
            Err(LalrpopParseError::ExtraToken { token: _ }) => todo!(),
        }
    }
}

fn main() -> Result<()> {
    let _args = Args::parse();

    let data_dir = dirs_next::data_dir().map(|mut p| {
        p.push("rcalc");
        p
    });

    if let Some(d) = &data_dir {
        std::fs::create_dir_all(d)
            .into_diagnostic()
            .wrap_err("Could not create data directory")?;
    };

    let parser = calc::InputStatementParser::new();
    let mut rl = Editor::<()>::new().into_diagnostic()?;

    let path = data_dir
        .map(|mut p| {
            p.push("history");
            p
        })
        .unwrap_or_else(|| "rcalc-history".into());

    if let Err(e) = rl.load_history(&path) {
        if path.exists() {
            println!("Error loading history: {:?}", e)
        }
    };

    let mut runner = runner::Runner::new();

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                let parsed = match parser
                    .parse(&line.as_str().into(), ast::lexer(&line))
                    .into_report(line.clone())
                {
                    Ok(p) => p,
                    Err(e) => {
                        println!("Parsing error: {:?}", e);
                        continue;
                    }
                };
                match runner.eval_input_statement(parsed) {
                    Err(e) => {
                        println!("Runtime error: {e:?}");
                        continue;
                    }
                    Ok(Some(v)) => println!("{}", runner.display_value(&v)),
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
    }

    Ok(())
}
