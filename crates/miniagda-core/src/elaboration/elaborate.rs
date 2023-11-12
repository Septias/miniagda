use std::collections::HashMap;

use crate::diagnostics::error::{ElabErr, Error};
use crate::diagnostics::span::Spanned;
use crate::diagnostics::Result;
use crate::syntax::{core::*, Ident};

pub struct Env {
  pub env: Vec<Val>,
  pub var_tys: Vec<Tm>,
  pub glo_tys: HashMap<Ident, Tm>,
  pub lvl: Lvl,
}

impl Env {
  fn add_var(&mut self, ty: &Tm) {
    self.var_tys.push(ty.clone())
  }
  fn add_glo(&mut self, glo: &Ident, ty: &Tm) {
    assert!(!self.glo_tys.contains_key(glo));
    self.glo_tys.insert(glo.clone(), ty.clone()).unwrap();
  }
}

pub fn elab_prog(prog: &Prog, env: &mut Env) -> Result<()> {
  prog
    .decls
    .iter()
    .map(|decl| elab_decl(decl, env))
    .collect::<Result<Vec<_>>>()?;

  elab_tm_chk(&prog.tm, &prog.ty, env)
}

fn elab_decl(decl: &Decl, env: &mut Env) -> Result<()> {
  match decl {
    Decl::Data(data) => elab_data(data, env),
  }
}

fn elab_data(data: &Data, env: &mut Env) -> Result<()> {
  elab_ctx(&data.params, env)?;
  elab_tel(&data.indices, env)?;

  Ok(())
}

fn elab_cstr(_cstr: &Cstr, lvl: usize, _env: &mut Env) -> Result<()> {
  todo!()
}

fn expect_ty(ty: &Tm) -> Result<usize> {
  if let Tm::Set(TmSet { level, .. }) = ty {
    Ok(*level)
  } else {
    Err(Error::Elab(ElabErr::ExpectedSet {
      span: ty.span(),
      got: ty.clone(),
    }))
  }
}

fn elab_ctx(ctx: &Ctx, env: &mut Env) -> Result<usize> {
  let lvls = ctx
    .tms
    .iter()
    .map(|tm| {
      let ty = elab_tm_inf(tm, env)?;
      let lvl = expect_ty(&ty)?;
      env.add_var(&ty);
      Ok(lvl)
    })
    .collect::<Result<Vec<_>>>()?;

  Ok(lvls.into_iter().max().unwrap_or(0))
}

fn elab_tel(_tel: &Tel, _env: &mut Env) -> Result<usize> {
  todo!()
}

fn elab_tm_chk(_tm: &Tm, _ty: &Tm, _env: &mut Env) -> Result<()> {
  todo!()
}

fn elab_tm_inf(_tm: &Tm, _env: &mut Env) -> Result<Tm> {
  todo!()
}

fn eq(val1: &Val, _val2: &Val) -> bool {
  true
}
