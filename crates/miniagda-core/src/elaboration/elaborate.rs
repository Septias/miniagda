use crate::diagnostics::error::{ElabErr, Error};
use crate::diagnostics::span::{Span, Spanned};
use crate::diagnostics::Result;
use crate::syntax::{core::*, Ident};
use std::collections::HashMap;

use super::normalize::{eval, nf};

#[derive(Clone, Debug, Default)]
pub struct Env {
  pub env: Vec<Val>,
  pub var_tys: Vec<Tm>,
  pub glo_tys: HashMap<Ident, Tm>,
  pub lvl: Lvl,
}

impl Env {
  fn add_global(&mut self, glo: Ident, ty: Tm) {
    assert!(!self.glo_tys.contains_key(&glo));
    self.glo_tys.insert(glo, ty);
  }

  pub fn forget<T>(&mut self, f: impl FnOnce(&mut Env) -> T) -> T {
    let len_tys = self.var_tys.len();
    let len_env = self.env.len();
    let res = f(self);
    self.var_tys.drain(0..(self.var_tys.len() - len_tys));
    self.env.drain(0..(self.env.len() - len_env));
    res
  }

  fn bind(&mut self, name: String, ty: Tm) {
    self.define(
      Val::Var(ValVar {
        name,
        lvl: self.lvl,
        span: Span::dummy(),
      }),
      ty,
    );
  }

  fn define(&mut self, val: Val, ty: Tm) {
    self.env.push(val);
    self.var_tys.push(ty);
    self.lvl += 1;
  }
}

pub fn elab(prog: Prog) -> Result<()> {
  elab_prog(prog, &mut Env::default())
}

pub fn elab_prog(prog: Prog, env: &mut Env) -> Result<()> {
  prog.decls.into_iter().map(|decl| elab_decl(decl, env)).collect::<Result<Vec<_>>>()?;
  assert!(env.var_tys.is_empty());
  elab_tm_chk(prog.tm, prog.ty, env)?;
  Ok(())
}

fn elab_decl(decl: Decl, env: &mut Env) -> Result<()> {
  match decl {
    Decl::Data(data) => elab_data(data, env),
  }
}

fn tms_to_fn(tms: &[Tm], binds: &[Ident], end: Tm) -> Tm {
  if tms.is_empty() {
    assert!(binds.is_empty());
    return end;
  }
  let dom = tms[0].clone();
  let ident = binds[0].clone();
  let codom = tms_to_fn(&tms[1..], &binds[1..], end);
  let dom_span = dom.span();
  let codom_span = codom.span();
  Tm::All(TmAll {
    ident,
    dom: Box::new(dom),
    codom: Box::new(codom),
    span: Span {
      file: dom_span.file,
      start: dom_span.start,
      end: codom_span.end,
    },
  })
}

fn ctx_to_fn(ctx: &Ctx, end: Tm) -> Tm {
  tms_to_fn(&mut ctx.tms.clone(), &mut ctx.binds.clone(), end)
}

fn tel_to_fn(tel: &Tel, end: Tm) -> Tm {
  tms_to_fn(&mut tel.tms.clone(), &mut tel.binds.clone(), end)
}

fn elab_data(data: Data, env: &mut Env) -> Result<()> {
  let as_fn = ctx_to_fn(&data.params, tel_to_fn(&data.indices, Tm::Set(TmSet { level: data.level, span: data.span() })));
  let cstr_clone = data.clone(); // TODO: optimize+

  elab_ctx(data.params, env)?;

  env.forget(|env| elab_tel(data.indices, env, None))?;

  env.add_global(data.ident, as_fn);

  data.cstrs.into_iter().map(|cstr| env.forget(|env| elab_cstr(cstr, &cstr_clone, env))).collect::<Result<Vec<_>>>()?;
  Ok(())
}

fn params_as_app(left: Tm, tms: &[Tm]) -> Tm {
  if tms.is_empty() {
    return left;
  }
  let left_span = left.span();
  params_as_app(
    Tm::App(TmApp {
      left: Box::new(left),
      right: Box::new(tms[0].clone()),
      span: Span {
        file: left_span.file,
        start: left_span.start,
        end: tms[0].span().end,
      },
    }),
    &tms[1..],
  )
}

fn elab_cstr(cstr: Cstr, data: &Data, env: &mut Env) -> Result<()> {
  let as_fn = ctx_to_fn(&data.params, tel_to_fn(&cstr.args, params_as_app(Tm::Glo(data.ident.clone()), &cstr.params)));

  elab_tel(cstr.args, env, Some(data.level))?;

  for i in 0..data.params.binds.len() {
    if let Tm::Var(TmVar { idx, name: _, .. }) = &cstr.params[i] {
      if idx.0 == i {
        continue;
      }
    }
    return Err(Error::from(ElabErr::ExpectedParam {
      span: cstr.params[i].span(),
      got: cstr.params[i].clone(),
    }));
  }

  let indices_params = &cstr.params[data.params.binds.len()..];
  elab_tms_chk(indices_params, &data.indices.tms, &data.indices.binds, env)?;

  env.add_global(cstr.ident, as_fn);

  // TODO: Termination checking.

  Ok(())
}

fn expect_set(ty: Tm, max_lvl: Option<usize>) -> Result<()> {
  if let Tm::Set(TmSet { level, .. }) = ty {
    if let Some(max_lvl) = max_lvl {
      if level > max_lvl {
        return Err(Error::from(ElabErr::CstrLevelTooHigh { span: ty.span(), max: max_lvl }));
      }
    }
    Ok(())
  } else {
    Err(Error::Elab(ElabErr::ExpectedSet { span: ty.span(), got: ty.clone() }))
  }
}

fn elab_ctx(ctx: Ctx, env: &mut Env) -> Result<()> {
  let tys_lvls = ctx
    .tms
    .into_iter()
    .map(|tm| {
      expect_set(elab_tm_inf(tm.clone(), env)?, None)?;
      Ok(tm)
    })
    .collect::<Result<Vec<_>>>()?;

  tys_lvls.into_iter().zip(ctx.binds).for_each(|(tm, Ident { name, .. })| env.bind(name, tm));
  Ok(())
}

fn elab_tel(tel: Tel, env: &mut Env, max_lvl: Option<usize>) -> Result<()> {
  tel
    .tms
    .into_iter()
    .zip(tel.binds)
    .map(|(tm, Ident { name, .. })| {
      let ty = elab_tm_inf(tm.clone(), env)?;
      expect_set(ty, max_lvl)?;
      env.bind(name, tm);
      Ok(())
    })
    .collect::<Result<Vec<_>>>()?;
  Ok(())
}

fn elab_tms_chk(tms: &[Tm], tys: &[Tm], binds: &[Ident], env: &mut Env) -> Result<()> {
  assert!(tms.len() == binds.len() && binds.len() == tms.len());
  if tms.is_empty() {
    return Ok(());
  }
  elab_tm_chk(tms[0].clone(), tys[0].clone(), env)?;
  if binds.len() > 1 {
    return elab_tms_chk(tms, &tys[1..], &binds[1..], env);
  }
  Ok(())
}

fn elab_tm_chk(tm: Tm, ty: Tm, env: &mut Env) -> Result<()> {
  match (tm, ty) {
    (Tm::Abs(TmAbs { ident, ty: _, body, .. }), Tm::All(TmAll { dom, codom, .. })) => {
      let mut nenv = env.clone();
      let mut tenv = env.env.clone();
      nenv.bind(ident.name, *dom);
      tenv.push(Val::Var(ValVar::from_lvl(env.lvl)));
      // if !eq(&ty, &dom) {
      //   return Err(Error::Elab(ElabErr::TypeMismatch { ty1: ty, ty2: *dom }));
      // }
      elab_tm_chk(*body, nf(*codom, &tenv), env)?;
    }
    (tm, ty) => {
      let ity = elab_tm_inf(tm, env)?;
      if !eq(&ity, &ty) {
        return Err(Error::Elab(ElabErr::TypeMismatch { ty1: ity, ty2: ty }));
      }
    }
  };
  Ok(())
}

fn elab_tm_inf(tm: Tm, env: &mut Env) -> Result<Tm> {
  Ok(match tm {
    Tm::Var(TmVar { idx, name, .. }) => env.var_tys.get(idx.0).unwrap_or_else(|| panic!("could not resolve type of variable {}", name)).clone(), // panic means impl error
    Tm::Glo(x) => env.glo_tys.get(&x).unwrap_or_else(|| panic!("could not resolve type of global {}", x.name)).clone(),                          // panic means impl error
    Tm::App(TmApp { left, right, .. }) => {
      let left_span = left.span();
      let lty = elab_tm_inf(*left, env)?;
      match lty {
        Tm::All(TmAll { dom, codom, .. }) => {
          let mut tenv = env.env.clone();
          tenv.push(eval(*right.clone(), &env.env));
          elab_tm_chk(*right, *dom, env)?;
          nf(*codom, &tenv)
        }
        ty => return Err(Error::Elab(ElabErr::FunctionTypeExpected { span: left_span, got: ty })),
      }
    }
    Tm::Abs(_) => todo!(),
    Tm::All(TmAll { dom, codom, span, .. }) => match (elab_tm_inf(*dom, env)?, elab_tm_inf(*codom, env)?) {
      (Tm::Set(TmSet { level: level1, .. }), Tm::Set(TmSet { level: level2, .. })) => Tm::Set(TmSet { level: level1.max(level2), span }),
      (Tm::Set(_), tm) | (tm, Tm::Set(_)) | (tm, _) => return Err(Error::Elab(ElabErr::ExpectedSet { span: tm.span(), got: tm })),
    },
    Tm::Set(TmSet { level, span }) => Tm::Set(TmSet { level: level + 1, span }),
  })
}

fn eq(_val1: &Tm, _val2: &Tm) -> bool {
  true
}
