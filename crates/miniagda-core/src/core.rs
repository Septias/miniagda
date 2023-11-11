use std::rc::Rc;

use crate::{context, diagnostic::Span};

#[derive(Clone, Copy, Debug)]
pub struct Idx(pub usize);

#[derive(Clone, Debug)]
pub struct Ident {
  pub name: String,
  pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub struct TmVar {
  pub name: String,
  pub idx: Idx,
  pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub struct TmGlo {
  pub name: String,
  pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub struct TmApp {
  pub left: Rc<Tm>,
  pub right: Rc<Tm>,
  pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub struct TmAbs {
  pub name: String,
  pub body: Rc<Tm>,
  pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub struct TmAll {
  pub name: String,
  pub dom: Rc<Tm>,
  pub codom: Rc<Tm>,
  pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub struct TmSet {
  pub level: usize,
  pub(crate) span: Span,
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
  pub ctx: context::Ctx<Tm>,
  pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub struct Cstr {
  pub name: Ident,
  pub args: Ctx,
  pub params: Vec<Tm>,
  pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub struct Data {
  pub name: Ident,
  pub params: Ctx,
  pub indices: Ctx,
  pub level: usize,
  pub cstrs: Vec<Cstr>,
  pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub enum Decl {
  Data(Data),
}

#[derive(Clone, Debug)]
pub struct Prog {
  pub decls: Vec<Decl>,
  pub tm: Tm,
  pub(crate) span: Span,
}

impl From<usize> for Idx {
  fn from(value: usize) -> Self {
    Idx(value)
  }
}
