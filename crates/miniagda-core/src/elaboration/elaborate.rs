use super::normalize::eval;
use crate::diagnostics::error::{ElabErr, Error};
use crate::diagnostics::span::{Span, Spanned};
use crate::diagnostics::Result;
use crate::syntax::core::Env;
use crate::syntax::{
  core::{Cstr, Ctx, Data, Decl, Lvl, Prog, Tel, Tm, TmAbs, TmAll, TmApp, TmSet, TmVar, Val, ValAbs, ValAll, ValApp, ValVar},
  Ident,
};
use crate::{debug, trace};
use core::panic;
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct State {
  pub env: Env,
  pub types: Vec<Val>,
  pub global_types: HashMap<Ident, Val>,
  pub lvl: Lvl,
}

impl State {
  fn resolve_global(&self, var: &Ident) -> Val {
    let ty = self
      .global_types
      .get(var)
      .unwrap_or_else(|| panic!("could not resolve type of variable {}", var))
      .clone();
    trace!(
      "resolve_global",
      "resolved global `{}` to be of type `{}` in `{{{}}}",
      var,
      ty,
      self.global_types.iter().map(|(k, v)| format!("{k} : {v}")).collect::<Vec<String>>().join(", ")
    );
    ty
  }

  fn resolve(&self, var: &TmVar) -> Val {
    let ty = self
      .types
      .get(var.idx.0)
      .unwrap_or_else(|| panic!("could not resolve type of variable {}", var.name))
      .clone();
    trace!(
      "resolve",
      "resolved `{}` to be of type `{}` in [`{}`]",
      var,
      ty,
      self.types.iter().map(|v| format!("{v}")).collect::<Vec<String>>().join(", ")
    );
    ty
  }

  pub fn forget<T>(&mut self, f: impl FnOnce(&mut State) -> T) -> T {
    let len_tys = self.types.len();
    let len_env = self.env.0.len();
    let res = f(self);
    self.types.drain(0..(self.types.len() - len_tys));
    self.env.0.drain(0..(self.env.0.len() - len_env));
    res
  }

  fn bind_global(&mut self, glo: Ident, ty: Val) {
    debug!("add_global", "add global `{}` with type `{}`", glo, ty);
    assert!(!self.global_types.contains_key(&glo));
    self.global_types.insert(glo, ty);
  }

  fn bind(&mut self, name: String, ty: Val) {
    self.define(
      Val::Var(ValVar {
        name,
        lvl: self.lvl,
        span: Span::default(),
      }),
      ty,
    );
  }

  fn define(&mut self, val: Val, ty: Val) {
    trace!("define", "defined `{}` with type `{}`", val, ty);
    self.env.0.insert(0, val);
    self.types.insert(0, ty);
    self.lvl += 1;
  }

  fn is_only_globals(&self) -> bool {
    self.env.0.is_empty() && self.types.is_empty()
  }
}

pub fn elab_prog(prog: Prog, state: &mut State) -> Result<()> {
  prog.decls.into_iter().map(|decl| elab_decl(decl, state)).collect::<Result<Vec<_>>>()?;
  assert!(state.is_only_globals());
  let ty = elab_tm_inf(prog.ty.clone(), state)?;
  expected_set(&ty, None)?;
  elab_tm_chk(prog.tm, eval(prog.ty, &state.env), state)?;
  Ok(())
}

fn elab_decl(decl: Decl, state: &mut State) -> Result<()> {
  state.forget(|state| match decl {
    Decl::Data(data) => elab_data(data, state),
  })
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
  debug!("elab_data", "elaborating data type `{}`", name);
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
    &Env::default(),
  );
  let cstr_clone = data.clone(); // TODO: optimize

  debug!("elab_data", "elaborating parameters `{}` of data type `{}`", data.params, name);
  elab_ctx(data.params, state)?;

  debug!("elab_data", "elaborating indices `{}` of data type `{}`", data.indices, name);
  state.forget(|state| elab_tel(data.indices, state, None))?;

  state.bind_global(data.ident, as_fn);

  data
    .cstrs
    .into_iter()
    .map(|cstr| state.forget(|env| elab_cstr(cstr, &cstr_clone, env)))
    .collect::<Result<Vec<_>>>()?;
  debug!("elab_data", "elaborated data type `{}`", name);
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
  debug!("elab_cstr", "elaborating constructor `{}`", name);

  assert!(!cstr.params.is_empty());

  match &cstr.params[0] {
    Tm::Glo(ident) if ident == &data.ident => (),
    tm => {
      return Err(Error::from(ElabErr::ExpectedData {
        expected: data.ident.clone(),
        got: tm.clone(),
      }))
    }
  }

  let as_fn = eval(
    ctx_to_fn(&data.params, tel_to_fn(&cstr.args, params_as_app(cstr.params[0].clone(), &cstr.params[1..]))),
    &Env::default(),
  );

  let params = &cstr.params[1..];

  let args_len = cstr.args.binds.len();

  debug!("elab_cstr", "elaborating constructor arguments `{}`", cstr.args);
  elab_tel(cstr.args, state, Some(data.level))?;

  for (i, param) in params.iter().enumerate().take(data.params.binds.len()) {
    if let Tm::Var(TmVar { idx, name: _, .. }) = param {
      if idx.0 == i + args_len {
        continue;
      }
    }
    return Err(Error::from(ElabErr::ExpectedParam {
      expected: data.params.binds[i + args_len].clone(),
      got: param.clone(),
    }));
  }

  let indices_params = &params[data.params.binds.len()..];
  debug!(
    "elab_cstr",
    "checking constructor indices `[{}]` match expected types `[{}]`",
    indices_params.iter().map(|x| format!("{x}")).collect::<Vec<String>>().join(", "),
    data.indices.tms.iter().map(|x| format!("{x}")).collect::<Vec<String>>().join(", ")
  );
  elab_tms_chk(indices_params, &data.indices.tms, &data.indices.binds, state)?;

  state.bind_global(cstr.ident, as_fn);

  // TODO: termination checking

  debug!("elab_cstr", "elaborated constructor `{}`", name);
  Ok(())
}

fn expected_set(ty: &Val, max_lvl: Option<usize>) -> Result<()> {
  if let Val::Set(TmSet { level, .. }) = ty {
    if let Some(max_lvl) = max_lvl {
      if *level > max_lvl {
        return Err(Error::from(ElabErr::LevelTooHigh { tm: ty.clone(), max: max_lvl }));
      }
    }
    return Ok(());
  }
  Err(Error::from(ElabErr::ExpectedSetCtx { got: ty.clone() }))
}

fn elab_ctx(ctx: Ctx, state: &mut State) -> Result<()> {
  let tys_lvls = ctx
    .tms
    .into_iter()
    .map(|tm| {
      let ty = elab_tm_inf(tm.clone(), state)?;
      expected_set(&ty, None)?;
      Ok(eval(tm, &state.env))
    })
    .collect::<Result<Vec<_>>>()?;

  tys_lvls.into_iter().zip(ctx.binds).for_each(|(ty, Ident { name, .. })| state.bind(name, ty));
  Ok(())
}

fn elab_tel(tel: Tel, state: &mut State, max_lvl: Option<usize>) -> Result<()> {
  tel
    .tms
    .into_iter()
    .zip(tel.binds)
    .map(|(tm, Ident { name, .. })| {
      let ty = elab_tm_inf(tm.clone(), state)?;
      expected_set(&ty, max_lvl)?;
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
  let tm_fmt = format!("{tm}");
  let ty_fmt = format!("{ty}");

  match (tm, ty) {
    (Tm::Abs(TmAbs { ident, ty: _, body, .. }), Val::All(ValAll { dom, codom, mut env, .. })) => {
      let mut n_state = state.clone();
      n_state.bind(ident.name, *dom);
      elab_tm_chk(*body, eval(codom, &env.ext_lvl(state.lvl)), &n_state)?;
    }
    (tm, ty) => {
      let ity = elab_tm_inf(tm, state)?;
      match eq(ity.clone(), ty.clone(), state.lvl) {
        Ok(()) => (),
        Err((v1, v2)) => return Err(Error::from(ElabErr::TypeMismatch { ty1: ity, ty2: ty, v1, v2 })),
      }
    }
  };
  trace!("elab_tm_chk", "checked that term `{}` has type `{}` (up to β-η reduction)", tm_fmt, ty_fmt);
  Ok(())
}

fn elab_tm_inf(tm: Tm, state: &State) -> Result<Val> {
  let tm_fmt = format!("{tm}");
  let ty = match tm {
    Tm::Var(x) => state.resolve(&x),
    Tm::Glo(x) => state.resolve_global(&x),
    Tm::App(TmApp { left, right, .. }) => {
      let left_clone = *left.clone(); // TODO: performance
      let lty = elab_tm_inf(*left, state)?;
      match lty {
        Val::All(ValAll { dom, codom, mut env, .. }) => {
          elab_tm_chk(*right.clone(), *dom, state)?;
          let right = eval(*right, &state.env);
          eval(codom, &env.ext(right))
        }
        ty => return Err(Error::from(ElabErr::FunctionTypeExpected { tm: left_clone, got: ty })),
      }
    }
    Tm::Abs(_) => todo!(),
    Tm::All(TmAll { dom, codom, span, .. }) => match (elab_tm_inf(*dom, state)?, elab_tm_inf(*codom, state)?) {
      (Val::Set(TmSet { level: level1, .. }), Val::Set(TmSet { level: level2, .. })) => Val::Set(TmSet { level: level1.max(level2), span }),
      (Val::Set(_), s) | (s, Val::Set(_) | _) => return Err(Error::from(ElabErr::ExpectedSetAll { got: s })),
    },
    Tm::Set(TmSet { level, span }) => Val::Set(TmSet { level: level + 1, span }),
  };
  trace!("elab_tm_inf", "inferred type of `{}` to be `{}`", tm_fmt, ty);
  Ok(ty)
}

fn eq(ty1: Val, ty2: Val, lvl: Lvl) -> std::result::Result<(), (Val, Val)> {
  let ty1_fmt = format!("{ty1}");
  let ty2_fmt = format!("{ty2}");
  trace!("eq", "test for type equality of `{}` and `{}`", ty1_fmt, ty2_fmt,);
  match (ty1, ty2) {
    (Val::Set(TmSet { level: level1, .. }), Val::Set(TmSet { level: level2, .. })) if level1 == level2 => Ok(()),
    (
      Val::Abs(ValAbs {
        ty: ty1,
        body: body1,
        env: mut env1,
        ..
      }),
      Val::Abs(ValAbs {
        ty: ty2,
        body: body2,
        env: mut env2,
        ..
      }),
    ) => {
      eq(*ty1, *ty2, lvl)?;
      eq(eval(body1, &env1.ext_lvl(lvl)), eval(body2, &env2.ext_lvl(lvl)), lvl + 1)
    }
    (
      Val::All(ValAll {
        dom: dom1,
        codom: codom1,
        env: mut env1,
        ..
      }),
      Val::All(ValAll {
        dom: dom2,
        codom: codom2,
        env: mut env2,
        ..
      }),
    ) => {
      eq(*dom1, *dom2, lvl)?;
      eq(eval(codom1, &env1.ext_lvl(lvl)), eval(codom2, &env2.ext_lvl(lvl)), lvl + 1)
    }
    (Val::Abs(ValAbs { mut env, body, .. }), val) => {
      let var = Val::Var(ValVar::from(lvl));
      let span = val.span();
      eq(
        eval(body, &env.ext_lvl(lvl)),
        Val::App(ValApp {
          left: Box::new(val),
          right: Box::new(var),
          span,
        }),
        lvl + 1,
      )
    }
    (val, Val::Abs(ValAbs { mut env, body, .. })) => {
      let var = Val::Var(ValVar::from(lvl));
      let span = val.span();
      eq(
        Val::App(ValApp {
          left: Box::new(val),
          right: Box::new(var),
          span,
        }),
        eval(body, &env.ext_lvl(lvl)),
        lvl + 1,
      )
    }
    (Val::Var(ValVar { lvl: lvl1, .. }), Val::Var(ValVar { lvl: lvl2, .. })) if lvl1 == lvl2 => Ok(()),
    (Val::Glo(ident1), Val::Glo(ident2)) if ident1 == ident2 => Ok(()),
    (Val::App(ValApp { left: left1, right: right1, .. }), Val::App(ValApp { left: left2, right: right2, .. })) => {
      eq(*left1, *left2, lvl)?;
      eq(*right1, *right2, lvl)
    }
    (ty1, ty2) => Err((ty1, ty2)),
  }
}
