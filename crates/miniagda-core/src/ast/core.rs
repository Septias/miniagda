use crate::diagnostic::Span;

#[derive(Clone, Copy, Debug)]
pub struct Idx(pub usize);

#[derive(Clone, Debug)]
pub struct Ident {
  pub name: String,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TmVar {
  pub name: String,
  pub idx: Idx,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TmGlo {
  pub name: String,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TmApp {
  pub left: Box<Tm>,
  pub right: Box<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TmAbs {
  pub ident: Ident,
  pub ty: Box<Tm>,
  pub body: Box<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TmAll {
  pub ident: Ident,
  pub dom: Box<Tm>,
  pub codom: Box<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TmSet {
  pub level: usize,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Tm {
  Var(TmVar),
  Glo(TmGlo),
  App(TmApp),
  Abs(TmAbs),
  All(TmAll),
  Set(TmSet),
}

#[derive(Clone, Debug)]
pub struct Ctx {
  pub tms: Vec<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Tel {
  pub tms: Vec<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Cstr {
  pub ident: Ident,
  pub args: Tel,
  pub params: Vec<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Data {
  pub ident: Ident,
  pub params: Ctx,
  pub indices: Tel,
  pub level: usize,
  pub cstrs: Vec<Cstr>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Decl {
  Data(Data),
}

#[derive(Clone, Debug)]
pub struct Prog {
  pub decls: Vec<Decl>,
  pub tm: Tm,
  pub ty: Tm,
  pub span: Span,
}

impl From<usize> for Idx {
  fn from(value: usize) -> Self {
    Idx(value)
  }
}
