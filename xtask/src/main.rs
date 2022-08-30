use std::{fs, path::Path};

use clap::{CommandFactory, Parser};
use duct::cmd;
use yansi::Paint;

#[derive(Parser, Debug)]
struct Args {
    #[clap(subcommand)]
    cmd: Option<Commands>,
}

#[derive(Parser, Debug)]
enum Commands {
    Test,
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let args = Args::from_args();

    let xtask_root = Path::new(env!("CARGO_MANIFEST_DIR"));

    let workspace_root = xtask_root
        .ancestors()
        .nth(1)
        .expect("could not find workspace root");

    match args.cmd {
        None => Args::into_app().print_help()?,
        Some(Commands::Test) => {
            let expr_dir = workspace_root.join("tests/expr");

            for entry in fs::read_dir(expr_dir)? {
                let entry = entry?;
                let file = std::fs::read_to_string(entry.path())?;
                let newline = file
                    .find('\n')
                    .unwrap_or_else(|| panic!("no newline in test {:?}", entry.file_name()));
                let expr = file[..newline].trim();
                let result = file[newline + 1..].trim();
                print!(
                    "  {} ... ",
                    Paint::new(entry.file_name().to_string_lossy()).bold()
                );
                let test_result = cmd!(env!("CARGO"), "run", "--", "-c", expr)
                    .stdout_capture()
                    .stderr_null()
                    .unchecked()
                    .run()?;
                if !test_result.status.success() {
                    println!(
                        "{} (exit code = {})",
                        Paint::red("not ok"),
                        test_result.status
                    );
                } else {
                    let test_result = String::from_utf8(test_result.stdout)?;
                    let test_result = test_result.trim();
                    if test_result != result {
                        println!(
                            "{} (expected = `{}`, got = `{}`)",
                            Paint::red("not ok"),
                            result,
                            test_result,
                        );
                    } else {
                        println!("{}", Paint::green("ok"),);
                    }
                }
            }
        }
    }

    Ok(())
}
