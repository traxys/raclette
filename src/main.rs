use std::path::PathBuf;

use clap::Parser;
use lalrpop_util::lalrpop_mod;
use rustyline::{error::ReadlineError, Editor};

pub mod ast;
pub mod builtins;
pub mod interpeter;

#[derive(Parser, Debug)]
struct Args {
    #[clap(short, long)]
    file: Option<PathBuf>,
}

lalrpop_mod!(pub raclette);

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let args = Args::parse();

    dbg!(&args);

    let data_dir = dirs_next::data_dir().map(|mut p| {
        p.push("raclette");
        p
    });

    if let Some(d) = &data_dir {
        std::fs::create_dir_all(d)?;
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
                        let parsed = match parser.parse(ast::lexer(&line)) {
                            Ok(p) => p,
                            Err(e) => {
                                println!("Parsing error: {:?}", e);
                                continue;
                            }
                        };
                        match parsed {
                            ast::Statement::Expr(exp) => {
                                match interpreter.run_expr(exp).map(|v| interpreter.display(v)) {
                                    Ok(Ok(v)) => {
                                        println!("{}", v);
                                    }
                                    Err(e) | Ok(Err(e)) => {
                                        println!("Error: {:?}", e);
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

            rl.save_history(&path)?;
        }
        Some(p) => {
            let input = std::fs::read_to_string(p)?;
            let parsed = raclette::FileParser::new().parse(ast::lexer(&input))?;
            dbg!(parsed);
        }
    }

    Ok(())
}
