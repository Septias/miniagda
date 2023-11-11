use std::path::PathBuf;

use crate::surface;

#[derive(Clone, Debug)]

pub struct Span {
  file: PathBuf,
  start: usize,
  end: usize,
}

pub trait Spanned {
  fn span(&self) -> Span;
}

#[derive(Clone, Debug)]
pub enum Diag {
  SurfaceToCore(SurfaceToCoreDiag),
}

#[derive(Clone, Debug)]
pub enum SurfaceToCoreDiag {
  UnboundName { string: String, span: Span },
  Internal { message: String },
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

impl_from_diag_enum!(SurfaceToCoreDiag; SurfaceToCore);

impl_spanned_struct!(surface::Ident);
impl_spanned_struct!(surface::TmApp);
impl_spanned_struct!(surface::TmAbs);
impl_spanned_struct!(surface::TmAll);
impl_spanned_struct!(surface::TmSet);
impl_spanned_enum!(surface::Tm; Var, App, Abs, All, Set);
impl_spanned_struct!(surface::Ctx);
impl_spanned_struct!(surface::Cstr);
impl_spanned_struct!(surface::Data);
impl_spanned_enum!(surface::Decl; Data);
impl_spanned_struct!(surface::Program);
