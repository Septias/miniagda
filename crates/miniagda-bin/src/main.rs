use std::env;

use miniagda_core::{ast::surface_to_core, parser::parse};

fn main() {
  let file_path: String = env::args().collect::<Vec<_>>()[1].clone();
  let parsed = parse(file_path).unwrap();
  let core = surface_to_core(&parsed);
  println!("{:#?}", core);
}
