use std::env;

use miniagda_core::{syntax::surface_to_core, parsing::parse};

fn main() {
  let file_path: String = env::args().collect::<Vec<_>>()[1].clone();
  let surface = parse(file_path).unwrap();
  println!("-- surface -------------------------------------------------------\n\n{}\n", &surface);
  let core = surface_to_core(&surface).unwrap();
  println!("-- core ----------------------------------------------------------\n\n{}\n", &core);
}
