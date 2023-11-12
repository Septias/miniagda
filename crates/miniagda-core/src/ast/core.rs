use std::fmt::Display;

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
  pub data: Ident,
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

impl Display for Tm {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Tm::Var(x) => write!(f, "{}{{{}}}", x.name, x.idx.0),
      Tm::Glo(x) => write!(f, "{}", x.name),
      Tm::App(TmApp { left, right, .. }) => write!(f, "({} {})", left, right),
      Tm::Abs(TmAbs {
        ident, ty, body, ..
      }) => write!(f, "(λ ({} : {}) → {})", ident.name, ty, body),
      Tm::All(TmAll {
        ident, dom, codom, ..
      }) => write!(f, "(∀ ({} : {}) → {})", ident.name, dom, codom),
      Tm::Set(TmSet { level, .. }) => {
        write!(
          f,
          "Set{}",
          if *level != 0 {
            level.to_string()
          } else {
            String::new()
          }
        )
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
      "{} : {}{}{} {}",
      self.ident.name,
      self.args,
      if self.args.tms.is_empty() {
        ""
      } else {
        " → "
      },
      self.data.name,
      self
        .params
        .iter()
        .map(|tm| format!("{}", tm))
        .collect::<Vec<String>>()
        .join(" ")
    )
  }
}

impl Display for Data {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "data {}{}{} : {}{}Set{} where\n{}",
      self.ident.name,
      if self.params.tms.is_empty() { "" } else { " " },
      self.params,
      self.indices,
      if self.indices.tms.is_empty() {
        ""
      } else {
        " → "
      },
      if self.level != 0 {
        self.level.to_string()
      } else {
        String::new()
      },
      self
        .cstrs
        .iter()
        .map(|cstr| format!("  {}", cstr))
        .collect::<Vec<String>>()
        .join("\n")
    )
  }
}

impl Display for Decl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Decl::Data(data) => write!(f, "{}", data),
    }
  }
}

impl Display for Prog {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}\n\n_ : {}\n_ = {}",
      self
        .decls
        .iter()
        .map(|decl| format!("{}", decl))
        .collect::<Vec<String>>()
        .join("\n\n"),
      self.ty,
      self.tm
    )
  }
}
