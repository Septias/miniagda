use std::fmt::Display;

use crate::diagnostics::span::Span;

use super::Ident;

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
  Brc(Box<Tm>),
}

#[derive(Clone, Debug)]
pub struct Ctx {
  pub binds: Vec<(Ident, Tm)>,
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
pub struct Func {
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

impl Display for Tm {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Tm::Var(x) => write!(f, "{}", x.name),
      Tm::App(TmApp { left, right, .. }) => write!(f, "{left} {right}"),
      Tm::Abs(TmAbs { ident, ty, body, .. }) => write!(f, "λ ({ident} : {ty}) → {body}"),
      Tm::All(TmAll { ident, dom, codom, .. }) => write!(f, "∀ ({ident} : {dom}) → {codom}"),
      Tm::Set(TmSet { level, .. }) => {
        write!(f, "Set{}", if *level == 0 { String::new() } else { level.to_string() })
      }
      Tm::Brc(tm) => write!(f, "({tm})"),
    }
  }
}

impl Display for Ctx {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      self.binds.iter().map(|(x, tm)| format!("({} : {})", x.name, tm)).collect::<Vec<String>>().join(" ")
    )
  }
}

impl Display for Cstr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{} : {}{} {}",
      self.ident,
      self.args,
      if self.args.binds.is_empty() { "" } else { " → " },
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
      if self.params.binds.is_empty() { "" } else { " " },
      self.params,
      self.indices,
      if self.indices.binds.is_empty() { "" } else { " → " },
      if self.level == 0 { String::new() } else { self.level.to_string() },
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
