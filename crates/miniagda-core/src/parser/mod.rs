pub mod lex;
pub mod parse;

use crate::{
  ast::surface::Prog,
  diagnostic::{Diag, ParseErr},
  parser::lex::Token,
  Result,
};
use std::{fs::read_to_string, path::Path};

pub fn parse<P: AsRef<Path>>(path: P) -> Result<Prog> {
  let file_path = path.as_ref().to_string_lossy().to_string();
  let src = read_to_string(path).map_err(|_| {
    Diag::from(ParseErr::FileNotFound {
      path: file_path.clone(),
    })
  })?;
  let stoks = lex::lex(src.as_str()).map_err(Diag::from)?;
  let stoks = lex::process_indent(stoks, |t| *t == Token::Where, |t| *t == Token::NewLine);
  let parsed = parse::parser::prog(&stoks, &file_path).map_err(|e| ParseErr::UnexpectedToken {
    span: e.location,
    expected: e.expected.tokens().into_iter().fold("".to_string(), |l, r| l + r),
  })?;
  Ok(parsed)
}
