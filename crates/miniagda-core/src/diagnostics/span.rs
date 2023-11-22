use crate::syntax::{core, surface, Ident};

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Span {
  pub file: String,
  pub start: usize,
  pub end: usize,
}

pub trait Spanned {
  fn span(&self) -> Span;
}

impl Spanned for Span {
  fn span(&self) -> Span {
    self.clone()
  }
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

impl_spanned_struct!(Ident);

impl_spanned_struct!(surface::TmApp);
impl_spanned_struct!(surface::TmAbs);
impl_spanned_struct!(surface::TmAll);
impl_spanned_struct!(surface::TmSet);
impl_spanned_enum!(surface::Tm; Var, App, Abs, All, Set, Brc);
impl_spanned_struct!(surface::Ctx);
impl_spanned_struct!(surface::Cstr);
impl_spanned_struct!(surface::Data);
impl_spanned_struct!(surface::PatCst);
impl_spanned_struct!(surface::PatDot);
impl_spanned_enum!(surface::Pat; Var, Cst, Dot, Abs, Brc);
impl_spanned_struct!(surface::ClsClause);
impl_spanned_struct!(surface::ClsAbsurd);
impl_spanned_enum!(surface::Cls; Cls, Abs);
impl_spanned_struct!(surface::Func);
impl_spanned_enum!(surface::Decl; Data, Func);
impl_spanned_struct!(surface::Prog);

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

impl_spanned_struct!(core::ValVar);
impl_spanned_struct!(core::ValApp);
impl_spanned_struct!(core::ValAbs);
impl_spanned_struct!(core::ValAll);
impl_spanned_enum!(core::Val; Var, Glo, App, Abs, All, Set);
