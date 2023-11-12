use crate::syntax::core::Tm;

use super::span::Span;

#[derive(Clone, Debug)]
pub enum Error {
  SurfaceToCore(SurfaceToCoreErr),
  Parse(ParseErr),
  Lex(LexErr),
  Elab(ElabErr),
}

#[derive(Clone, Debug)]
pub enum SurfaceToCoreErr {
  UnboundName { name: String, span: Span },
  GlobalExists { name: String, span: Span },
  Internal { message: String },
}

#[derive(Clone, Debug)]
pub enum ParseErr {
  FileNotFound { path: String },
  UnexpectedToken { span: usize, expected: String },
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum LexErr {
  Level,
  #[default]
  UnknownCharacter,
}

#[derive(Clone, Debug)]
pub enum ElabErr {
  ExpectedSet { span: Span, got: Tm },
}

macro_rules! impl_from_diag_enum {
  ($ident:path; $variant:ident) => {
    impl From<$ident> for Error {
      fn from(value: $ident) -> Self {
        Error::$variant(value)
      }
    }
  };
}

impl_from_diag_enum!(SurfaceToCoreErr; SurfaceToCore);
impl_from_diag_enum!(LexErr; Lex);
impl_from_diag_enum!(ParseErr; Parse);
