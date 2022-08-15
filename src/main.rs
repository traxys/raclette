use std::{ffi::OsStr, path::PathBuf};

use clap::Parser;
use lalrpop_util::lalrpop_mod;
use miette::{Context, Diagnostic, IntoDiagnostic, NamedSource, Result, SourceCode, SourceSpan};
use rustyline::{error::ReadlineError, Editor};

pub mod ast;
pub mod builtins;
pub mod interpeter;
pub mod span;

#[derive(Parser, Debug)]
struct Args {
    #[clap(short, long)]
    file: Option<PathBuf>,
}

lalrpop_mod!(pub raclette);

#[derive(thiserror::Error, Debug, Diagnostic)]
enum ParseError {
    #[diagnostic(code(raclette::parsing::invalid_token))]
    #[error("Token is invalid")]
    Invalid {
        #[label]
        position: SourceSpan,
    },
    #[diagnostic(code(raclette::parsing::unrecognized_token))]
    #[error("Unrecognized token: {token}.\nExpected one of: {}", expected.join(", "))]
    Unrecognized {
        token: ast::Token,
        expected: Vec<String>,
        #[label("this token is not valid in this context")]
        location: SourceSpan,
    },
    #[diagnostic(code(raclette::parsing::unknown_token))]
    #[error("Unknown token: {token}")]
    Unknown {
        token: String,
        #[label("this is not a valid token")]
        location: SourceSpan,
    },
    #[diagnostic(code(raclette::parsing::eof))]
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
    let args = Args::parse();

    dbg!(&args);

    let data_dir = dirs_next::data_dir().map(|mut p| {
        p.push("raclette");
        p
    });

    if let Some(d) = &data_dir {
        std::fs::create_dir_all(d)
            .into_diagnostic()
            .wrap_err("Could not create data directory")?;
    };

    match args.file {
        None => {
            let parser = raclette::StatementParser::new();
            let mut rl = Editor::<()>::new();

            let mut interpreter = interpeter::Interpreter::new();

            let path = data_dir
                .map(|mut p| {
                    p.push("history");
                    p
                })
                .unwrap_or_else(|| "raclette-history".into());

            if let Err(e) = rl.load_history(&path) {
                if path.exists() {
                    println!("Error loading history: {:?}", e)
                }
            };

            loop {
                match rl.readline(">> ") {
                    Ok(line) => {
                        rl.add_history_entry(&line);
                        let parsed = match parser.parse(ast::lexer(&line)).into_report(line.clone())
                        {
                            Ok(p) => p,
                            Err(e) => {
                                println!("Parsing error: {:?}", e);
                                continue;
                            }
                        };
                        match parsed {
                            ast::Statement::Expr(exp) => {
                                match interpreter.run_expr(exp).map(|v| v.borrow().to_string()) {
                                    Ok(v) => {
                                        println!("{}", v);
                                    }
                                    Err(e) => {
                                        let report: miette::Report = e.into();
                                        println!("Error: {:?}", report.with_source_code(line));
                                        continue;
                                    }
                                };
                            }
                            _ => {
                                if let Err(e) = interpreter.run_statement(parsed) {
                                    println!("Error: {:?}", e);
                                    continue;
                                }
                            }
                        }
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

            rl.save_history(&path)
                .into_diagnostic()
                .wrap_err("Could not save history")?;
        }
        Some(p) => {
            let input = std::fs::read_to_string(&p)
                .into_diagnostic()
                .wrap_err("Could not open input file")?;
            let source = NamedSource::new(
                p.file_name()
                    .map(OsStr::to_string_lossy)
                    .unwrap_or_else(|| "<source-file>".into()),
                input.clone(),
            );
            let parsed = raclette::FileParser::new()
                .parse(ast::lexer(&input))
                .into_report(source)?;
            dbg!(parsed);
        }
    }

    Ok(())
}
