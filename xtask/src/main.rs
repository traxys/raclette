use std::path::Path;

use clap::{CommandFactory, Parser};
use duct::cmd;

#[derive(Parser, Debug)]
struct Args {
    #[clap(subcommand)]
    cmd: Option<Commands>,
}

#[derive(Parser, Debug)]
enum Commands {
    Benches,
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
        Some(Commands::Benches) => {
            cmd!(env!("CARGO"), "criterion").run()?;
            cmd!(env!("CARGO"), "build", "--release").run()?;
            cmd!(workspace_root.join("benches/cmp_python/cmp.sh"))
                .dir(workspace_root.join("benches/cmp_python"))
                .run()?;
        }
    }

    Ok(())
}
