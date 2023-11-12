use crate::diagnostic::Span;

#[derive(Clone, Debug)]
pub struct Ident {
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
  Var(Ident),
  App(TmApp),
  Abs(TmAbs),
  All(TmAll),
  Set(TmSet),
}

#[derive(Clone, Debug)]
pub struct Ctx {
  pub ctx: Vec<(Ident, Tm)>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Cstr {
  pub ident: Ident,
  pub args: Ctx,
  pub params: Vec<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Data {
  pub ident: Ident,
  pub params: Ctx,
  pub indices: Ctx,
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

impl PartialEq for Ident {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
  }
}
