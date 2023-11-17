use std::path::Path;

use clap::Parser;
use env_logger::Env;
use miniagda_core::diagnostics::error::Error;
use miniagda_core::diagnostics::Result;
use miniagda_core::{elaboration::elab, parsing::parse, syntax::surface_to_core};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
  path: std::path::PathBuf,
}

fn run<P: AsRef<Path>>(path: P) -> Result<()> {
  let prog = parse(path)?;
  println!("-- surface -------------------------------------------------------\n\n{}\n", &prog);
  let prog = surface_to_core(prog)?;
  println!("-- core ----------------------------------------------------------\n\n{}\n", &prog);
  elab(prog)
}

fn main() {
  let args = Args::parse();
  env_logger::Builder::from_env(Env::default().default_filter_or("info")).format_timestamp(None).init();
  match run(args.path) {
    Ok(()) => log::info!(target: "miniagda", "type check successful"),
    Err(e) => match e {
      Error::SurfaceToCore(e) => log::error!(target: "translation to core language", "{}", e),
      Error::Parse(e) => log::error!(target: "parsing", "{}", e),
      Error::Lex(e) => log::error!(target: "lexing", "{}", e),
      Error::Elab(e) => log::error!(target: "elaboration", "{}", e),
    },
  }
}
