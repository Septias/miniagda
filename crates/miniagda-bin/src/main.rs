use std::env;

use env_logger::Env;
use miniagda_core::diagnostics::Result;
use miniagda_core::{elaboration::elaborate::elab, parsing::parse, syntax::surface_to_core};
fn main() -> Result<()> {
  env_logger::Builder::from_env(Env::default().default_filter_or("warn"))
    .format_timestamp(None)
    .init();

  let path: String = env::args().collect::<Vec<_>>()[1].clone();
  let prog = parse(path)?;
  println!("-- surface -------------------------------------------------------\n\n{}\n", &prog);
  let prog = surface_to_core(prog)?;
  println!("-- core ----------------------------------------------------------\n\n{}\n", &prog);
  elab(prog)
}
