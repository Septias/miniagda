use std::env;

use env_logger::Env;
use miniagda_core::diagnostics::error::Error;
use miniagda_core::diagnostics::Result;
use miniagda_core::{elaboration::elab, parsing::parse, syntax::surface_to_core};

fn run(path: String) -> Result<()> {
  let prog = parse(path)?;
  println!("-- surface -------------------------------------------------------\n\n{}\n", &prog);
  let prog = surface_to_core(prog)?;
  println!("-- core ----------------------------------------------------------\n\n{}\n", &prog);
  elab(prog)
}

fn main() {
  env_logger::Builder::from_env(Env::default().default_filter_or("info")).format_timestamp(None).init();
  match run(env::args().collect::<Vec<_>>()[1].clone()) {
    Ok(()) => log::info!(target: "miniagda", "type check successful"),
    Err(e) => match e {
      Error::SurfaceToCore(e) => log::error!(target: "translation to core language", "{}", e),
      Error::Parse(e) => log::error!(target: "parsing", "{}", e),
      Error::Lex(e) => log::error!(target: "lexing", "{}", e),
      Error::Elab(e) => log::error!(target: "elaboration", "{}", e),
    },
  }
}
