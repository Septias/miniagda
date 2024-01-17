pub mod core;
pub mod surface;
use crate::{
  diagnostics::Result,
  diagnostics::{
    error::{Error, SurfaceToCoreErr},
    span::Span,
  },
  syntax::{core::PatCst, surface::PatId},
};
use std::{fmt::Display, hash::Hash};

use self::core::{ClsAbsurd, ClsClause};

#[derive(Clone, Debug)]
pub struct Ident {
  pub name: String,
  pub span: Span,
}

#[derive(Clone, Debug)]
enum Glo {
  Cstr,
  Data,
  Func,
}

#[derive(Clone, Debug, Default)]
struct Env {
  var: Vec<Ident>,
  glo: Vec<(Ident, Glo)>,
}

impl Env {
  fn resolve(&self, var: Ident) -> Result<core::Tm> {
    assert!(var.name != "_", "term inference not yet implemented");
    match self.var.iter().position(|n| n.name == var.name) {
      Some(idx) => Ok(core::Tm::Var(core::TmVar {
        name: var.name,
        idx: core::Idx(idx),
        span: var.span,
      })),
      None => {
        if let Some((_, glo)) = self.glo.iter().find(|(x, _)| x == &var) {
          Ok(match glo {
            Glo::Cstr => core::Tm::Cstr(var),
            Glo::Data => core::Tm::Data(var),
            Glo::Func => core::Tm::Func(var),
          })
        } else {
          Err(Error::from(SurfaceToCoreErr::UnboundName { name: var.name, span: var.span }))
        }
      }
    }
  }

  fn has_cstr(&self, var: &Ident) -> bool {
    if let Some((_, Glo::Cstr)) = self.glo.iter().find(|(x, _)| x == var) {
      true
    } else {
      false
    }
  }

  pub fn add_glo(&mut self, x: Ident, glo: Glo) -> Result<()> {
    if self.glo.iter().find(|(var, _)| &x == var).is_some() {
      return Err(Error::from(SurfaceToCoreErr::GlobalExists { name: x.name, span: x.span }));
    }
    self.glo.push((x, glo));
    Ok(())
  }

  pub fn add_var(&mut self, x: Ident) {
    self.var.insert(0, x);
  }

  pub fn forget<T>(&mut self, f: impl FnOnce(&mut Env) -> T) -> T {
    let len = self.var.len();
    let res = f(self);
    self.var.drain(0..(self.var.len() - len));
    res
  }
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Terms

fn surf_to_core_tm(tm: surface::Tm, env: &mut Env) -> Result<core::Tm> {
  Ok(match tm {
    surface::Tm::Var(x) => env.resolve(x)?,
    surface::Tm::App(surface::TmApp { left, right, span }) => core::Tm::App(core::TmApp {
      left: Box::new(surf_to_core_tm(*left, env)?),
      right: Box::new(surf_to_core_tm(*right, env)?),
      span: span.clone(),
    }),
    surface::Tm::Abs(surface::TmAbs { ident, ty, body, span }) => {
      let mut n_env: Env = env.clone();
      n_env.add_var(ident.clone());
      core::Tm::Abs(core::TmAbs {
        ident,
        ty: Box::new(surf_to_core_tm(*ty, env)?),
        body: Box::new(surf_to_core_tm(*body, &mut n_env)?),
        span: span.clone(),
      })
    }
    surface::Tm::All(surface::TmAll { ident, dom, codom, span }) => {
      let mut n_env = env.clone();
      n_env.add_var(ident.clone());
      core::Tm::All(core::TmAll {
        ident,
        dom: Box::new(surf_to_core_tm(*dom, env)?),
        codom: Box::new(surf_to_core_tm(*codom, &mut n_env)?),
        span: span.clone(),
      })
    }
    surface::Tm::Set(surface::TmSet { level, span }) => core::Tm::Set(core::Set { level, span }),
    surface::Tm::Brc(tm) => surf_to_core_tm(*tm, env)?,
  })
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Contexts

fn surf_to_core_tel(ctx: surface::Ctx, env: &mut Env) -> Result<core::Tel> {
  let (binds, tms) = surf_to_core_binds(ctx.binds, env)?;
  Ok(core::Tel { binds, tms, span: ctx.span })
}

fn surf_to_core_ctx(ctx: surface::Ctx, env: &mut Env) -> Result<core::Ctx> {
  let (binds, tms) = surf_to_core_binds(ctx.binds, env)?;
  Ok(core::Ctx { binds, tms, span: ctx.span })
}

fn surf_to_core_binds(binds: Vec<(Ident, surface::Tm)>, env: &mut Env) -> Result<(Vec<Ident>, Vec<core::Tm>)> {
  Ok(
    binds
      .into_iter()
      .map(|(x, tm)| {
        let tm = surf_to_core_tm(tm, env)?;
        env.add_var(x.clone());
        Ok((x, tm))
      })
      .collect::<Result<Vec<_>>>()?
      .into_iter()
      .unzip(),
  )
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Functions

fn surf_to_core_func(func: surface::Func, env: &mut Env) -> Result<core::Func> {
  let ty = surf_to_core_tm(func.ty, env)?;

  env.add_glo(func.ident.clone(), Glo::Func)?;

  let cls = func
    .cls
    .into_iter()
    .map(|cls| env.forget(|env| surface_to_core_cls(&func.ident, cls, env)))
    .collect::<Result<Vec<core::Cls>>>()?;

  Ok(core::Func {
    ident: func.ident,
    ty,
    cls,
    span: func.span,
  })
}

fn surface_to_core_cls(ident: &Ident, cls: surface::Cls, env: &mut Env) -> Result<core::Cls> {
  let (func, pats, span, rhs) = match cls {
    surface::Cls::Cls(surface::ClsClause { func, pats, rhs, span }) => (func, pats, span, Some(rhs)),
    surface::Cls::Abs(surface::ClsAbsurd { func, pats, span }) => (func, pats, span, None),
  };

  if ident != &func {
    return Err(Error::from(SurfaceToCoreErr::MisnamedCls {
      name: ident.clone(),
      cls: func.clone(),
      span: span.clone(),
    }));
  }

  let pats = surf_to_core_pats(pats, env)?;
  match rhs {
    Some(rhs) => Ok(core::Cls::Cls(ClsClause {
      func,
      pats,
      rhs: surf_to_core_tm(rhs, env)?,
      span,
    })),
    None => Ok(core::Cls::Abs(ClsAbsurd { func, pats, span })),
  }
}

fn surf_to_core_pats(pats: Vec<surface::Pat>, env: &mut Env) -> Result<Vec<core::Pat>> {
  enum Mixed {
    Core(core::Pat),
    Dot(surface::PatDot),
  }

  let pats = pats
    .into_iter()
    .map(|pat| match pat {
      surface::Pat::Id(PatId { ident, pats, span }) => Ok(Mixed::Core(if env.has_cstr(&ident) {
        core::Pat::Cst(PatCst {
          cstr: ident,
          pats: surf_to_core_pats(pats, env)?,
          span,
        })
      } else {
        if !pats.is_empty() {
          return Err(Error::from(SurfaceToCoreErr::UnresolvedCstr {
            name: ident.clone(),
            span: span.clone(),
          }));
        }
        env.add_var(ident.clone());
        core::Pat::Var(ident)
      })),
      surface::Pat::Dot(pat) => Ok(Mixed::Dot(pat)),
    })
    .collect::<Result<Vec<_>>>()?;

  pats
    .into_iter()
    .map(|mixed| match mixed {
      Mixed::Core(pat) => Ok(pat),
      Mixed::Dot(surface::PatDot { tm, span }) => Ok(core::Pat::Dot(core::PatDot {
        tm: surf_to_core_tm(tm, env)?,
        span,
      })),
    })
    .collect::<Result<Vec<_>>>()
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Data Types

fn surf_to_core_data(data: surface::Data, env: &mut Env) -> Result<core::Data> {
  let params = surf_to_core_ctx(data.params, env)?;

  let indices = env.forget(|env| surf_to_core_tel(data.indices, env))?;

  let set = surf_to_core_tm(data.set, &mut Env::default())?;

  env.add_glo(data.ident.clone(), Glo::Data)?;

  let cstrs = data.cstrs.into_iter().map(|cstr| surf_to_core_cstr(cstr, env)).collect::<Result<Vec<_>>>()?;

  Ok(core::Data {
    ident: data.ident,
    params,
    indices,
    set,
    cstrs,
    span: data.span,
  })
}

fn surf_to_core_cstr(cstr: surface::Cstr, env: &mut Env) -> Result<core::Cstr> {
  let (args, params) = env.forget(|env| {
    (
      surf_to_core_tel(cstr.args, env),
      cstr.params.into_iter().map(|tm| surf_to_core_tm(tm, env)).collect::<Result<Vec<_>>>(),
    )
  });

  env.add_glo(cstr.ident.clone(), Glo::Cstr)?;

  Ok(core::Cstr {
    ident: cstr.ident,
    args: args?,
    params: params?,
    span: cstr.span,
  })
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Programs

fn surf_to_core_decl(decl: surface::Decl, env: &mut Env) -> Result<core::Decl> {
  Ok(env.forget::<Result<_>>(|env| {
    Ok(match decl {
      surface::Decl::Data(data) => core::Decl::Data(surf_to_core_data(data, env)?),
      surface::Decl::Func(func) => core::Decl::Func(surf_to_core_func(func, env)?),
    })
  })?)
}

pub fn surface_to_core(prog: surface::Prog) -> Result<core::Prog> {
  let mut env = Env::default();
  let prog = core::Prog {
    decls: prog.decls.into_iter().map(|decl| surf_to_core_decl(decl, &mut env)).collect::<Result<Vec<_>>>()?,
    span: prog.span.clone(),
  };
  debug_assert!(env.var.is_empty());
  Ok(prog)
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Trait Impls

impl PartialEq for Ident {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
  }
}

impl Eq for Ident {}

impl Hash for Ident {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.name.hash(state);
  }
}

impl Display for Ident {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name)
  }
}
