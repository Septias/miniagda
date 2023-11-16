use std::{
  fmt::Display,
  ops::{Add, AddAssign},
};

use crate::diagnostics::span::Span;

use super::Ident;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Idx(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub struct Lvl(pub usize);

#[derive(Clone, Debug)]
pub struct TmVar {
  pub name: String,
  pub idx: Idx,
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
  Glo(Ident),
  App(TmApp),
  Abs(TmAbs),
  All(TmAll),
  Set(TmSet),
}

#[derive(Clone, Debug)]
pub struct Ctx {
  pub binds: Vec<Ident>,
  pub tms: Vec<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Tel {
  pub binds: Vec<Ident>,
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

#[derive(Clone, Debug)]
pub struct ValVar {
  pub name: String,
  pub lvl: Lvl,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ValApp {
  pub left: Box<Val>,
  pub right: Box<Val>,
  pub span: Span,
}

#[derive(Clone, Debug, Default)]
pub struct Env(pub Vec<Val>);

#[derive(Clone, Debug)]
pub struct ValAbs {
  pub env: Env,
  pub ident: Ident,
  pub ty: Box<Val>,
  pub body: Tm,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ValAll {
  pub env: Env,
  pub ident: Ident,
  pub dom: Box<Val>,
  pub codom: Tm,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Val {
  Var(ValVar),
  Glo(Ident),
  App(ValApp),
  Abs(ValAbs),
  All(ValAll),
  Set(TmSet),
}

// ------------------------------------------------------------------------------------------------
// trait impls

impl From<usize> for Idx {
  fn from(value: usize) -> Self {
    Idx(value)
  }
}

impl From<usize> for Lvl {
  fn from(value: usize) -> Self {
    Lvl(value)
  }
}

impl Add<usize> for Lvl {
  type Output = Lvl;

  fn add(self, rhs: usize) -> Self::Output {
    Lvl(self.0 + rhs)
  }
}

impl AddAssign<usize> for Lvl {
  fn add_assign(&mut self, rhs: usize) {
    self.0 += rhs;
  }
}

impl Display for Val {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Val::Var(ValVar { lvl, name, .. }) => write!(f, "{}{{{{{}}}}}", name, lvl.0),
      Val::Glo(x) => write!(f, "{}", x.name),
      Val::App(ValApp { left, right, .. }) => write!(f, "({left} {right})"),
      Val::Abs(ValAbs { env, ident, ty, body, .. }) => write!(
        f,
        "(λ[{}]({} : {}) → {})",
        ident,
        env.0.iter().map(|val| format!("{val}")).collect::<Vec<String>>().join(", "),
        ty,
        body
      ),
      Val::All(ValAll { env, ident, dom, codom, .. }) => write!(
        f,
        "(∀[{}]({} : {}) → {})",
        env.0.iter().map(|val| format!("{val}")).collect::<Vec<String>>().join(", "),
        ident,
        dom,
        codom
      ),
      Val::Set(TmSet { level, .. }) => write!(f, "Set{}", if *level == 0 { String::new() } else { level.to_string() }),
    }
  }
}

impl Display for TmVar {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}{{{}}}", self.name, self.idx.0)
  }
}

impl Display for Tm {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Tm::Var(x) => write!(f, "{x}"),
      Tm::Glo(x) => write!(f, "{}", x.name),
      Tm::App(TmApp { left, right, .. }) => write!(f, "({left} {right})"),
      Tm::Abs(TmAbs { ident, ty, body, .. }) => write!(f, "(λ ({ident} : {ty}) → {body})"),
      Tm::All(TmAll { ident, dom, codom, .. }) => write!(f, "(∀ ({ident} : {dom}) → {codom})"),
      Tm::Set(TmSet { level, .. }) => {
        write!(f, "Set{}", if *level == 0 { String::new() } else { level.to_string() })
      }
    }
  }
}

impl Display for Ctx {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      self
        .tms
        .iter()
        .enumerate()
        .map(|(i, tm)| format!("({} : {})", self.binds[i].name, tm))
        .collect::<Vec<String>>()
        .join(" ")
    )
  }
}

impl Display for Tel {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      self
        .tms
        .iter()
        .enumerate()
        .map(|(i, tm)| format!("({} : {})", self.binds[i].name, tm))
        .collect::<Vec<String>>()
        .join(" ")
    )
  }
}

impl Display for Cstr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{} : {}{}{}",
      self.ident,
      self.args,
      if self.args.tms.is_empty() { "" } else { " → " },
      self.params.iter().map(|tm| format!("{tm}")).collect::<Vec<String>>().join(" ")
    )
  }
}

impl Display for Data {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "data {}{}{} : {}{}Set{} where\n{}",
      self.ident,
      if self.params.tms.is_empty() { "" } else { " " },
      self.params,
      self.indices,
      if self.indices.tms.is_empty() { "" } else { " → " },
      if self.level == 0 { self.level.to_string() } else { String::new() },
      self.cstrs.iter().map(|cstr| format!("  {cstr}")).collect::<Vec<String>>().join("\n")
    )
  }
}

impl Display for Decl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Decl::Data(data) => write!(f, "{data}"),
    }
  }
}

impl Display for Prog {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}\n\n_ : {}\n_ = {}",
      self.decls.iter().map(|decl| format!("{decl}")).collect::<Vec<String>>().join("\n\n"),
      self.ty,
      self.tm
    )
  }
}
