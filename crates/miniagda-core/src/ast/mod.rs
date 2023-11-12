pub mod core;
pub mod surface;

use crate::{
  diagnostic::{Diag, SurfaceToCoreErr},
  Result,
};

#[derive(Clone, Debug, Default)]
struct Env {
  var: Vec<surface::Ident>,
  glo: Vec<surface::Ident>,
}

impl Env {
  fn resolve(&self, var: &surface::Ident) -> Result<core::Tm> {
    assert!(var.name != "_", "inference not yet implemented");
    match self.var.iter().position(|n| n.name == var.name) {
      Some(idx) => Ok(core::Tm::Var(core::TmVar {
        name: var.name.clone(),
        idx: core::Idx(idx),
        span: var.span.clone(),
      })),
      None => {
        if self.glo.contains(var) {
          Ok(core::Tm::Glo(core::TmGlo {
            name: var.name.clone(),
            span: var.span.clone(),
          }))
        } else {
          Err(Diag::from(SurfaceToCoreErr::UnboundName {
            name: var.name.clone(),
            span: var.span.clone(),
          }))
        }
      }
    }
  }

  pub fn add_glo(&mut self, x: &surface::Ident) -> Result<()> {
    if self.glo.contains(x) {
      return Err(Diag::from(SurfaceToCoreErr::GlobalExists {
        name: x.name.clone(),
        span: x.span.clone(),
      }));
    }
    self.glo.push(x.clone());
    Ok(())
  }

  pub fn add_var(&mut self, x: &surface::Ident) {
    if x.name == "_" {
      return;
    }
    self.var.push(x.clone());
  }
}

pub fn surface_to_core(prog: &surface::Prog) -> Result<core::Prog> {
  let mut env = Env::default();
  let prog = core::Prog {
    decls: prog
      .decls
      .iter()
      .map(|decl| surf_to_core_decl(decl, &mut env))
      .collect::<Result<Vec<_>>>()?,
    ty: surf_to_core_tm(&prog.ty, &mut env)?,
    tm: surf_to_core_tm(&prog.tm, &mut env)?,
    span: prog.span.clone(),
  };
  debug_assert!(env.var.is_empty());
  Ok(prog)
}

fn surf_to_core_decl(decl: &surface::Decl, env: &mut Env) -> Result<core::Decl> {
  let decl = match decl {
    surface::Decl::Data(data) => core::Decl::Data(surf_to_core_data(data, env)?),
  };
  env.var.clear();
  Ok(decl)
}

fn surf_to_core_data(data: &surface::Data, env: &mut Env) -> Result<core::Data> {
  env.add_glo(&data.ident)?;

  let params = surf_to_core_ctx(&data.params, env)?;

  let len = env.var.len();
  let indices = surf_to_core_tel(&data.indices, env)?;
  env.var.truncate(len);

  let cstrs = data
    .cstrs
    .iter()
    .map(|cstr| surf_to_core_cstr(cstr, env))
    .collect::<Result<Vec<_>>>()?;

  Ok(core::Data {
    ident: core::Ident {
      name: data.ident.name.clone(),
      span: data.ident.span.clone(),
    },
    params,
    indices,
    level: data.level,
    cstrs,
    span: data.span.clone(),
  })
}

fn surf_to_core_ctx(ctx: &surface::Ctx, env: &mut Env) -> Result<core::Ctx> {
  let tms = ctx
    .ctx
    .iter()
    .map(|(_, tm)| surf_to_core_tm(tm, env))
    .collect::<Result<Vec<_>>>()?;
  ctx.ctx.iter().for_each(|(x, _)| env.add_var(x));
  Ok(core::Ctx {
    tms,
    span: ctx.span.clone(),
  })
}

fn surf_to_core_cstr(cstr: &surface::Cstr, env: &mut Env) -> Result<core::Cstr> {
  let len = env.var.len();

  let args = surf_to_core_tel(&cstr.args, env)?;
  let params = cstr
    .params
    .iter()
    .map(|tm| surf_to_core_tm(tm, env))
    .collect::<Result<Vec<_>>>()?;

  env.var.truncate(len);
  env.add_glo(&cstr.ident)?;

  Ok(core::Cstr {
    ident: core::Ident {
      name: cstr.ident.name.clone(),
      span: cstr.ident.span.clone(),
    },
    args,
    params,
    span: cstr.span.clone(),
  })
}

fn surf_to_core_tel(tel: &surface::Ctx, env: &mut Env) -> Result<core::Tel> {
  let tms = tel
    .ctx
    .iter()
    .map(|(x, tm)| {
      let tm = surf_to_core_tm(tm, env);
      env.add_var(x);
      tm
    })
    .collect::<Result<Vec<_>>>()?;

  Ok(core::Tel {
    tms,
    span: tel.span.clone(),
  })
}

fn surf_to_core_tm(tm: &surface::Tm, env: &mut Env) -> Result<core::Tm> {
  match tm {
    surface::Tm::Var(x) => env.resolve(x),
    surface::Tm::App(surface::TmApp { left, right, span }) => Ok(core::Tm::App(core::TmApp {
      left: Box::new(surf_to_core_tm(left.as_ref(), env)?),
      right: Box::new(surf_to_core_tm(right.as_ref(), env)?),
      span: span.clone(),
    })),
    surface::Tm::Abs(surface::TmAbs { ident, ty, body, span }) => {
      let mut nenv = env.clone();
      nenv.add_var(ident);
      Ok(core::Tm::Abs(core::TmAbs {
        ident: core::Ident {
          name: ident.name.clone(),
          span: ident.span.clone(),
        },
        ty: Box::new(surf_to_core_tm(ty, env)?),
        body: Box::new(surf_to_core_tm(body, &mut nenv)?),
        span: span.clone(),
      }))
    }
    surface::Tm::All(surface::TmAll {
      ident,
      dom,
      codom,
      span,
    }) => {
      let mut nenv = env.clone();
      nenv.add_var(ident);
      Ok(core::Tm::All(core::TmAll {
        ident: core::Ident {
          name: ident.name.clone(),
          span: ident.span.clone(),
        },
        dom: Box::new(surf_to_core_tm(dom, env)?),
        codom: Box::new(surf_to_core_tm(codom, &mut nenv)?),
        span: span.clone(),
      }))
    }
    surface::Tm::Set(surface::TmSet { level, span }) => Ok(core::Tm::Set(core::TmSet {
      level: *level,
      span: span.clone(),
    })),
  }
}
