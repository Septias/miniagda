use crate::syntax::{core, surface};

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
  pub file: String,
  pub start: usize,
  pub end: usize,
}

impl Span {
  pub fn dummy() -> Self {
    Span {
      file: "".to_owned(),
      start: 0,
      end: 0,
    }
  }
}

pub trait Spanned {
  fn span(&self) -> Span;
}

#[derive(Clone, Debug)]
pub enum Diag {
  SurfaceToCore(SurfaceToCoreErr),
  Parse(ParseErr),
  Lex(LexErr),
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

macro_rules! impl_from_diag_enum {
  ($ident:path; $variant:ident) => {
    impl From<$ident> for Diag {
      fn from(value: $ident) -> Self {
        Diag::$variant(value)
      }
    }
  };
}

macro_rules! impl_spanned_struct {
  ($name:path) => {
    impl Spanned for $name {
      fn span(&self) -> Span {
        self.span.clone()
      }
    }
  };
}

macro_rules! impl_spanned_enum {
  ($name:path; $($variant:ident),*) => {
      impl Spanned for $name {
        fn span(&self) -> Span {
          match self {
            $(
              Self::$variant(x) => x.span()
            ),*
          }
        }
      }
  };
}

impl_from_diag_enum!(SurfaceToCoreErr; SurfaceToCore);
impl_from_diag_enum!(LexErr; Lex);
impl_from_diag_enum!(ParseErr; Parse);

impl_spanned_struct!(surface::Ident);
impl_spanned_struct!(surface::TmApp);
impl_spanned_struct!(surface::TmAbs);
impl_spanned_struct!(surface::TmAll);
impl_spanned_struct!(surface::TmSet);
impl_spanned_enum!(surface::Tm; Var, App, Abs, All, Set, Brc);
impl_spanned_struct!(surface::Ctx);
impl_spanned_struct!(surface::Cstr);
impl_spanned_struct!(surface::Data);
impl_spanned_enum!(surface::Decl; Data);
impl_spanned_struct!(surface::Prog);

impl_spanned_struct!(core::Ident);
impl_spanned_struct!(core::TmVar);
impl_spanned_struct!(core::TmApp);
impl_spanned_struct!(core::TmAbs);
impl_spanned_struct!(core::TmAll);
impl_spanned_struct!(core::TmSet);
impl_spanned_enum!(core::Tm; Var, Glo, App, Abs, All, Set);
impl_spanned_struct!(core::Ctx);
impl_spanned_struct!(core::Tel);
impl_spanned_struct!(core::Cstr);
impl_spanned_struct!(core::Data);
impl_spanned_enum!(core::Decl; Data);
impl_spanned_struct!(core::Prog);
