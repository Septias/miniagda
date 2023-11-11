use crate::{
  core::{self, TmGlo, TmVar},
  diagnostic::{Diag, SurfaceToCoreDiag},
  surface, Result,
};

#[derive(Clone, Debug, Default)]
struct Env {
  var: Vec<String>,
  glo: Vec<String>,
}

impl Env {
  fn resolve(&self, var: &surface::Ident) -> Result<core::Tm> {
    match self.var.iter().position(|n| n == &var.name) {
      Some(idx) => Ok(core::Tm::Var(TmVar {
        name: var.name.clone(),
        idx: core::Idx(idx),
        span: var.span.clone(),
      })),
      None => {
        if self.glo.contains(&var.name) {
          Ok(core::Tm::Glo(TmGlo {
            name: var.name.clone(),
            span: var.span.clone(),
          }))
        } else {
          Err(Diag::from(SurfaceToCoreDiag::UnboundName {
            string: var.name.clone(),
            span: var.span.clone(),
          }))
        }
      }
    }
  }

  fn add_glo(&mut self, glo: String) {
    self.glo.push(glo);
  }

  fn add_var(&mut self, var: String) {
    self.glo.push(var);
  }

  fn clone_with_glo(&self) -> Self {
    Env {
      var: Vec::default(),
      glo: self.glo.clone(),
    }
  }
}

pub fn surface_to_core(prog: &surface::Program) -> Result<core::Prog> {
  let mut env = Env::default();
  Ok(core::Prog {
    decls: prog
      .decls
      .iter()
      .map(|decl| surf_to_core_decl(decl, &mut env))
      .collect::<Result<Vec<_>>>()?,
    tm: surf_to_core_tm(&prog.tm, &mut env)?,
    span: prog.span.clone(),
  })
}

fn surf_to_core_decl(decl: &surface::Decl, env: &mut Env) -> Result<core::Decl> {
  Ok(match decl {
    surface::Decl::Data(data) => core::Decl::Data(surf_to_core_data(data, env)?),
  })
}

fn surf_to_core_data(data: &surface::Data, env: &mut Env) -> Result<core::Data> {
  todo!()
}

fn surf_to_core_ctx(ctx: &surface::Ctx, env: &Env) -> Result<core::Ctx> {
  todo!()
}

fn surf_to_core_tm(tm: &surface::Tm, env: &Env) -> Result<core::Tm> {
  todo!()
}
