use super::normalize::eval;
use crate::diagnostics::error::{ElabErr, Error};
use crate::diagnostics::span::{Span, Spanned};
use crate::diagnostics::Result;
use crate::syntax::{core::*, Ident};
use crate::{debug, info, trace};
use core::panic;
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct State {
  pub env: Vec<Val>,
  pub var_tys: Vec<Val>,
  pub glo_tys: HashMap<Ident, Val>,
  pub lvl: Lvl,
}

impl State {
  fn add_global(&mut self, glo: Ident, ty: Val) {
    debug!("add_global", "add global `{}` with type `{}`", glo, ty);
    assert!(!self.glo_tys.contains_key(&glo));
    self.glo_tys.insert(glo, ty);
  }

  pub fn forget<T>(&mut self, f: impl FnOnce(&mut State) -> T) -> T {
    let len_tys = self.var_tys.len();
    let len_env = self.env.len();
    let res = f(self);
    self.var_tys.drain(0..(self.var_tys.len() - len_tys));
    self.env.drain(0..(self.env.len() - len_env));
    res
  }

  fn bind(&mut self, name: String, ty: Val) {
    debug!("bind", "bound variable `{}` with type `{}`", name, ty);
    self.define(
      Val::Var(ValVar {
        name,
        lvl: self.lvl,
        span: Span::dummy(),
      }),
      ty,
    );
  }

  fn define(&mut self, val: Val, ty: Val) {
    debug!("define", "defined `{}` with type `{}`", val, ty);
    self.env.push(val);
    self.var_tys.push(ty);
    self.lvl += 1;
  }
}

pub fn elab(prog: Prog) -> Result<()> {
  let mut env = State::default();
  elab_prog(prog, &mut env)
}

pub fn elab_prog(prog: Prog, state: &mut State) -> Result<()> {
  prog.decls.into_iter().map(|decl| elab_decl(decl, state)).collect::<Result<Vec<_>>>()?;
  // assert!(env.var_tys.is_empty());
  elab_tm_chk(prog.tm, eval(prog.ty, &state.env), state)?;
  Ok(())
}

fn elab_decl(decl: Decl, env: &mut State) -> Result<()> {
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
  tms_to_fn(&ctx.tms, &ctx.binds, end)
}

fn tel_to_fn(tel: &Tel, end: Tm) -> Tm {
  tms_to_fn(&tel.tms, &tel.binds, end)
}

fn elab_data(data: Data, state: &mut State) -> Result<()> {
  let name = data.ident.clone();
  info!("elab_data", "elaborating data type `{}`", name);
  let as_fn = eval(
    ctx_to_fn(
      &data.params,
      tel_to_fn(
        &data.indices,
        Tm::Set(TmSet {
          level: data.level,
          span: data.span(),
        }),
      ),
    ),
    &state.env,
  );
  let cstr_clone = data.clone(); // TODO: optimize

  info!("elab_data", "elaborating paramters `{}` of type `{}`", data.params, name);
  elab_ctx(data.params, state)?;

  info!("elab_data", "elaborating indices `{}` of type `{}`", data.indices, name);
  state.forget(|env| elab_tel(data.indices, env, None))?;

  state.add_global(data.ident, as_fn);

  data
    .cstrs
    .into_iter()
    .map(|cstr| state.forget(|env| elab_cstr(cstr, &cstr_clone, env)))
    .collect::<Result<Vec<_>>>()?;
  info!("elab_data", "elaborated data type `{}`", name);
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

fn elab_cstr(cstr: Cstr, data: &Data, state: &mut State) -> Result<()> {
  let name = cstr.ident.clone();
  info!("elab_cstr", "elaborating constructor `{}`", name);
  let as_fn = eval(
    ctx_to_fn(&data.params, tel_to_fn(&cstr.args, params_as_app(Tm::Glo(data.ident.clone()), &cstr.params))),
    &state.env,
  );
  let args_len = cstr.args.binds.len();

  info!("elab_cstr", "elaborating constructor arguments `{}`", cstr.args);
  elab_tel(cstr.args, state, Some(data.level))?;

  for i in 0..data.params.binds.len() {
    if let Tm::Var(TmVar { idx, name: _, .. }) = &cstr.params[i] {
      if idx.0 == i + args_len {
        continue;
      }
    }
    return Err(Error::from(ElabErr::ExpectedParam {
      span: cstr.params[i].span(),
      got: cstr.params[i].clone(),
    }));
  }

  let indices_params = &cstr.params[data.params.binds.len()..];
  info!(
    "elab_cstr",
    "checking constructor indices `[{}]` match expected types `[{}]`",
    indices_params.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(", "),
    data.indices.tms.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(", ")
  );
  elab_tms_chk(indices_params, &data.indices.tms, &data.indices.binds, state)?;

  state.add_global(cstr.ident, as_fn);

  // TODO: Termination checking.
  info!("elab_cstr", "elaborated constructor `{}`", name);
  Ok(())
}

fn max_set_lvl(ty: Val, max_lvl: Option<usize>) -> Result<()> {
  if let Val::Set(TmSet { level, .. }) = ty {
    if let Some(max_lvl) = max_lvl {
      if level > max_lvl {
        return Err(Error::from(ElabErr::CstrLevelTooHigh { span: ty.span(), max: max_lvl }));
      }
    }
  }
  Ok(())
}

fn elab_ctx(ctx: Ctx, state: &mut State) -> Result<()> {
  let tys_lvls = ctx
    .tms
    .into_iter()
    .map(|tm| {
      max_set_lvl(elab_tm_inf(tm.clone(), state)?, None)?;
      Ok(tm)
    })
    .collect::<Result<Vec<_>>>()?;

  tys_lvls
    .into_iter()
    .zip(ctx.binds)
    .for_each(|(tm, Ident { name, .. })| state.bind(name, eval(tm, &state.env)));
  Ok(())
}

fn elab_tel(tel: Tel, state: &mut State, max_lvl: Option<usize>) -> Result<()> {
  tel
    .tms
    .into_iter()
    .zip(tel.binds)
    .map(|(tm, Ident { name, .. })| {
      let ty = elab_tm_inf(tm.clone(), state)?;
      max_set_lvl(ty, max_lvl)?;
      state.bind(name, eval(tm, &state.env));
      Ok(())
    })
    .collect::<Result<Vec<_>>>()?;
  Ok(())
}

fn elab_tms_chk(tms: &[Tm], tys: &[Tm], binds: &[Ident], state: &State) -> Result<()> {
  assert!(tms.len() == binds.len() && binds.len() == tms.len());
  if tms.is_empty() {
    return Ok(());
  }
  elab_tm_chk(tms[0].clone(), eval(tys[0].clone(), &state.env), state)?;
  if binds.len() > 1 {
    return elab_tms_chk(tms, &tys[1..], &binds[1..], state);
  }
  Ok(())
}

fn elab_tm_chk(tm: Tm, ty: Val, state: &State) -> Result<()> {
  trace!("elab_tm_chk", "check that term `{}` has type `{}` (up to β-η reduction)", tm, ty);
  let tm_fmt = format!("{}", tm);
  let ty_fmt = format!("{}", ty);

  match (tm, ty) {
    (Tm::Abs(TmAbs { ident, ty: _, body, .. }), Val::All(ValAll { dom, codom, env, .. })) => {
      let mut n_state = state.clone();
      n_state.bind(ident.name, *dom);
      // if !eq(&ty, &dom) {
      //   return Err(Error::Elab(ElabErr::TypeMismatch { ty1: ty, ty2: *dom }));
      // }
      let mut env = env.clone();
      env.push(Val::Var(ValVar::from_lvl(state.lvl)));
      elab_tm_chk(*body, eval(codom, &env), &n_state)?;
    }
    (tm, ty) => {
      let ity = elab_tm_inf(tm, state)?;
      if !eq(ity.clone(), ty.clone(), state.lvl) {
        // TODO: performance
        return Err(Error::Elab(ElabErr::TypeMismatch { ty1: ity, ty2: ty }));
      }
    }
  };
  trace!("elab_tm_chk", "checked that term `{}` has type `{}` (up to β-η reduction)", tm_fmt, ty_fmt);
  Ok(())
}

fn elab_tm_inf(tm: Tm, state: &State) -> Result<Val> {
  let tm_fmt = format!("{}", tm);
  let ty = match tm {
    Tm::Var(TmVar { idx, name, .. }) => state
      .var_tys
      .get(idx.0)
      .unwrap_or_else(|| panic!("could not resolve type of variable {}", name))
      .clone(),
    Tm::Glo(x) => state.glo_tys.get(&x).unwrap_or_else(|| panic!("could not resolve type of global {}", x.name)).clone(),
    Tm::App(TmApp { left, right, .. }) => {
      let left_span = left.span();
      let lty = elab_tm_inf(*left, state)?;
      match lty {
        Val::All(ValAll { dom, codom, env, .. }) => {
          let mut env = env.clone();
          env.push(eval(*right.clone(), &state.env));

          elab_tm_chk(*right, *dom, state)?;
          eval(codom, &env)
        }
        ty => return Err(Error::Elab(ElabErr::FunctionTypeExpected { span: left_span, got: ty })),
      }
    }
    Tm::Abs(_) => todo!(),
    Tm::All(TmAll { dom, codom, span, .. }) => match (elab_tm_inf(*dom, state)?, elab_tm_inf(*codom, state)?) {
      (Val::Set(TmSet { level: level1, .. }), Val::Set(TmSet { level: level2, .. })) => Val::Set(TmSet { level: level1.max(level2), span }),
      (Val::Set(_), s) | (s, Val::Set(_)) | (s, _) => return Err(Error::Elab(ElabErr::ExpectedSetAll { span: s.span(), got: s })),
    },
    Tm::Set(TmSet { level, span }) => Val::Set(TmSet { level: level + 1, span }),
  };
  trace!("elab_tm_inf", "inferred type of `{}` to be `{}`", tm_fmt, ty);
  Ok(ty)
}

fn eq(ty1: Val, ty2: Val, lvl: Lvl) -> bool {
  let ty1_fmt = format!("{}", ty1);
  let ty2_fmt = format!("{}", ty2);
  trace!("eq", "checking for type equality of `{}` and `{}`", ty1_fmt, ty2_fmt,);
  let eq = match (ty1, ty2) {
    (Val::Set(TmSet { level: level1, .. }), Val::Set(TmSet { level: level2, .. })) => level1 == level2,
    (
      Val::Abs(ValAbs {
        ty: ty1, body: body1, env: env1, ..
      }),
      Val::Abs(ValAbs {
        ty: ty2, body: body2, env: env2, ..
      }),
    ) => {
      let mut env1 = env1.clone();
      env1.push(Val::Var(ValVar::from_lvl(lvl)));
      let mut env2 = env2.clone();
      env2.push(Val::Var(ValVar::from_lvl(lvl)));
      eq(*ty1, *ty2, lvl) && eq(eval(body1, &env1), eval(body2, &env2), lvl + 1)
    }
    (
      Val::All(ValAll {
        dom: dom1,
        codom: codom1,
        env: env1,
        ..
      }),
      Val::All(ValAll {
        dom: dom2,
        codom: codom2,
        env: env2,
        ..
      }),
    ) => {
      let mut env1 = env1.clone();
      env1.push(Val::Var(ValVar::from_lvl(lvl)));
      let mut env2 = env2.clone();
      env2.push(Val::Var(ValVar::from_lvl(lvl)));
      eq(*dom1, *dom2, lvl) && eq(eval(codom1, &env1), eval(codom2, &env2), lvl + 1)
    }
    (Val::Abs(ValAbs { env, body, .. }), val) => {
      let mut env = env.clone();
      let var = Val::Var(ValVar::from_lvl(lvl));
      let span = val.span();
      env.push(var.clone());
      eq(
        eval(body, &env),
        Val::App(ValApp {
          left: Box::new(val),
          right: Box::new(var),
          span,
        }),
        lvl + 1,
      )
    }
    (val, Val::Abs(ValAbs { env, body, .. })) => {
      let mut env = env.clone();
      let var = Val::Var(ValVar::from_lvl(lvl));
      let span = val.span();
      env.push(var.clone());
      eq(
        Val::App(ValApp {
          left: Box::new(val),
          right: Box::new(var),
          span,
        }),
        eval(body, &env),
        lvl + 1,
      )
    }
    (Val::Var(ValVar { lvl: lvl1, .. }), Val::Var(ValVar { lvl: lvl2, .. })) => lvl1 == lvl2,
    (Val::Glo(ident1), Val::Glo(ident2)) => ident1 == ident2,
    (Val::App(ValApp { left: left1, right: right1, .. }), Val::App(ValApp { left: left2, right: right2, .. })) => {
      eq(*left1, *left2, lvl) && eq(*right1, *right2, lvl)
    }
    _ => false,
  };
  trace!(
    "eq",
    "checked for type equality of `{}` and `{}` ({})",
    ty1_fmt,
    ty2_fmt,
    if eq { "they are equal" } else { "they are not equal" }
  );
  eq
}
