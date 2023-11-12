use crate::syntax::core::*;

use crate::Result;

struct Env {
  
}

fn type_prog(_prog: Prog, _env: &mut Env) -> Result<()> {
  todo!()
}

fn type_decl(_decl: Decl, _env: &mut Env) -> Result<()> {
  todo!()
}

fn type_data(_data: Data, _env: &mut Env) -> Result<()> {
  todo!()
}

fn type_cstr(_cstr: Cstr, _env: &mut Env) -> Result<()> {
  todo!()
}

fn type_ctx(_ctx: Ctx, _env: &mut Env) -> Result<()> {
  todo!()
}

fn type_tel(_tel: Tel, _env: &mut Env) -> Result<()> {
  todo!()
}

fn type_tm_chk(_tm: Tm, _ty: Tm, _env: &mut Env) -> Result<()> {
  todo!()
}

fn type_tm_inf(_tm: Tm, _env: &mut Env) -> Result<()> {
  todo!()
}

fn tm_eq(_tm1: Tm, _tm2: Tm) -> Result<()> {
  Ok(())
}
