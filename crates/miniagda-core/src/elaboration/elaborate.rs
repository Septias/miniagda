use crate::syntax::core::*;
use crate::diagnostics::Result;

struct Env {
  env: Vec<Val>,
  var_tys: Vec<Tm>,
  glo_tys: Vec<Tm>,
  lvl: Lvl,
}

fn elab_prog(prog: Prog, env: &mut Env) -> Result<()> {
  todo!()
}

fn elab_decl(_decl: Decl, _env: &mut Env) -> Result<()> {
  todo!()
}

fn elab_data(_data: Data, _env: &mut Env) -> Result<()> {
  todo!()
}

fn elab_cstr(_cstr: Cstr, _env: &mut Env) -> Result<()> {
  todo!()
}

fn elab_ctx(_ctx: Ctx, _env: &mut Env) -> Result<()> {
  todo!()
}

fn elab_tel(_tel: Tel, _env: &mut Env) -> Result<()> {
  todo!()
}

fn elab_tm_chk(_tm: Tm, _ty: Tm, _env: &mut Env) -> Result<()> {
  todo!()
}

fn elab_tm_inf(_tm: Tm, _env: &mut Env) -> Result<()> {
  todo!()
}

fn tm_eq(_tm1: Tm, _tm2: Tm) -> Result<()> {
  Ok(())
}
