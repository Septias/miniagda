pub mod core;
pub mod surface;

use crate::{
  diagnostics::error::{Err, SurfaceToCoreErr},
  diagnostics::Result,
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
          Ok(core::Tm::Glo(core::Ident {
            name: var.name.clone(),
            span: var.span.clone(),
          }))
        } else {
          Err(Err::from(SurfaceToCoreErr::UnboundName {
            name: var.name.clone(),
            span: var.span.clone(),
          }))
        }
      }
    }
  }

  pub fn add_glo(&mut self, x: &surface::Ident) -> Result<()> {
    if self.glo.contains(x) {
      return Err(Err::from(SurfaceToCoreErr::GlobalExists {
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
    self.var.insert(0, x.clone());
  }

  pub fn forget_vars<T>(&mut self, f: impl FnOnce(&mut Env) -> T) -> T {
    let len = self.var.len();
    let res = f(self);
    self.var.drain(0..(self.var.len() - len));
    res
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

  let indices = env.forget_vars(|env| surf_to_core_tel(&data.indices, env))?;

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
  let binds = ctx
    .ctx
    .iter()
    .map(|(surface::Ident { name, span }, _)| core::Ident {
      name: name.clone(),
      span: span.clone(),
    })
    .collect::<Vec<core::Ident>>();
  ctx.ctx.iter().for_each(|(x, _)| env.add_var(x));
  Ok(core::Ctx {
    binds,
    tms,
    span: ctx.span.clone(),
  })
}

fn surf_to_core_cstr(cstr: &surface::Cstr, env: &mut Env) -> Result<core::Cstr> {
  let (args, params) = env.forget_vars(|env| {
    (
      surf_to_core_tel(&cstr.args, env),
      cstr
        .params
        .iter()
        .map(|tm| surf_to_core_tm(tm, env))
        .collect::<Result<Vec<_>>>(),
    )
  });

  env.add_glo(&cstr.ident)?;

  Ok(core::Cstr {
    ident: core::Ident {
      name: cstr.ident.name.clone(),
      span: cstr.ident.span.clone(),
    },
    data: core::Ident {
      name: cstr.data.name.clone(),
      span: cstr.data.span.clone(),
    },
    args: args?,
    params: params?,
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
  let binds = tel
    .ctx
    .iter()
    .map(|(surface::Ident { name, span }, _)| core::Ident {
      name: name.clone(),
      span: span.clone(),
    })
    .collect::<Vec<core::Ident>>();
  Ok(core::Tel {
    binds,
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
    surface::Tm::Abs(surface::TmAbs {
      ident,
      ty,
      body,
      span,
    }) => {
      let mut nenv: Env = env.clone();
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
    surface::Tm::Brc(tm) => surf_to_core_tm(tm, env),
  }
}
