use crate::diagnostics::error::{ElabErr, Error};
use crate::diagnostics::span::{Span, Spanned};
use crate::diagnostics::Result;
use crate::elaboration::normalize::nf;
use crate::syntax::{core::*, Ident};
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct Env {
  pub env: Vec<Option<Val>>,
  pub var_tys: Vec<Tm>,
  pub glo_tys: HashMap<Ident, Tm>,
  pub lvl: Lvl,
}

impl Env {
  fn add_var_ty(&mut self, ty: Tm) {
    self.var_tys.push(ty)
  }
  fn add_glo_ty(&mut self, glo: Ident, ty: Tm) {
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
  fn add_val_empty(&mut self) {
    self.env.push(None)
  }
  fn add_val(&mut self, val: Val) {
    self.env.push(Some(val));
  }
}

pub fn elab(prog: Prog) -> Result<()> {
  elab_prog(prog, &mut Env::default())
}

pub fn elab_prog(prog: Prog, env: &mut Env) -> Result<()> {
  prog
    .decls
    .into_iter()
    .map(|decl| elab_decl(decl, env))
    .collect::<Result<Vec<_>>>()?;
  assert!(env.var_tys.is_empty());
  elab_tm_chk(prog.tm, prog.ty, env)
}

fn elab_decl(decl: Decl, env: &mut Env) -> Result<()> {
  match decl {
    Decl::Data(data) => elab_data(data, env),
  }
}

fn tms_to_fn(tms: &mut [Tm], binds: &mut [Ident], end: Tm) -> Tm {
  if tms.is_empty() {
    assert!(binds.is_empty());
    return end;
  }
  let dom = tms[0].clone();
  let ident = binds[0].clone();
  let codom = tms_to_fn(&mut tms[1..], &mut binds[1..], end);
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

fn data_fn(data: &Data) -> Tm {
  ctx_to_fn(
    &data.params,
    tel_to_fn(
      &data.indices,
      Tm::Set(TmSet {
        level: data.level,
        span: data.span(),
      }),
    ),
  )
}

fn elab_data(data: Data, env: &mut Env) -> Result<()> {
  let as_fn = data_fn(&data);
  let cstr_clone = data.clone();
  elab_ctx(data.params, env)?;
  env.forget(|env| elab_tel(data.indices, env, None))?;
  env.add_glo_ty(data.ident, as_fn);
  data
    .cstrs
    .into_iter()
    .map(|cstr| env.forget(|env| elab_cstr(cstr, &cstr_clone, env)))
    .collect::<Result<Vec<_>>>()?;
  Ok(())
}

fn elab_cstr(cstr: Cstr, data: &Data, env: &mut Env) -> Result<()> {
  elab_tel(cstr.args, env, Some(data.level))?;
  for i in 0..data.params.binds.len() {
    // params don't eval
    env.add_val_empty();

    if let Tm::Var(TmVar { idx, .. }) = &cstr.params[i] && idx.0 == i {
        continue;
    }
    return Err(Error::from(ElabErr::ExpectedParam {
      span: cstr.params[i].span(),
      got: cstr.params[i].clone(),
    }));
  }
  let params = &cstr.params[data.params.binds.len()..];

  elab_tms_chk(params, &data.indices, env)?;

  //TODO: termination checking

  Ok(())
}

fn expect_set(ty: Tm, max_lvl: Option<usize>) -> Result<()> {
  if let Tm::Set(TmSet { level, .. }) = ty {
    if let Some(max_lvl) = max_lvl && level > max_lvl {
      return Err(Error::from(ElabErr::CstrLevelTooHigh { span: ty.span(), max: max_lvl }))
    }
    Ok(())
  } else {
    Err(Error::Elab(ElabErr::ExpectedSet {
      span: ty.span(),
      got: ty.clone(),
    }))
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
  tys_lvls.into_iter().for_each(|tm| env.add_var_ty(tm)); // todo performance
  Ok(())
}

fn elab_tel(tel: Tel, env: &mut Env, max_lvl: Option<usize>) -> Result<()> {
  tel
    .tms
    .into_iter()
    .map(|tm| {
      let ty = elab_tm_inf(tm.clone(), env)?;
      expect_set(ty, max_lvl)?;
      env.add_var_ty(tm);
      Ok(())
    })
    .collect::<Result<Vec<_>>>()?;
  Ok(())
}

fn elab_tms_chk(tms: &[Tm], tel: &Tel, env: &mut Env) -> Result<()> {
  assert!(tms.len() == tel.binds.len() && tel.binds.len() == tel.tms.len());
  if tms.is_empty() {
    return Ok(());
  }
  let tm = tms[0].clone();
  let ty = tel.tms[0].clone();
  elab_tm_chk(tm, ty, env)?;
  if tel.binds.len() > 1 {
    let tel = Tel {
      binds: tel.binds[1..].to_vec(),
      tms: tel.tms[1..].to_vec(),
      span: Span {
        file: tel.span.file.clone(),
        start: tel.tms[1].span().start,
        end: tel.span().end,
      },
    };
    return elab_tms_chk(tms, &tel, env);
  }
  Ok(())
}

fn elab_tm_chk(tm: Tm, ty: Tm, env: &mut Env) -> Result<()> {
  match (nf(tm, &env.env), nf(ty, &env.env)) {
    (
      Tm::Abs(TmAbs {
        ident: ident1,
        ty,
        body,
        span: span1,
      }),
      Tm::All(TmAll {
        ident: ident2,
        dom,
        codom,
        span: span2,
      }),
    ) => {
      let x = ();
    }
    _ => {
      todo!()
    }
  }
  Ok(())
}

fn elab_tm_inf(_tm: Tm, _env: &mut Env) -> Result<Tm> {
  todo!()
}

fn eq(_val1: &Val, _val2: &Val) -> bool {
  true
}
