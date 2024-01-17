use crate::{
  diagnostics::{
    error::{ElabErr, Error},
    span::Spanned,
  },
  elaboration::eval::eval,
  syntax::core::{Prog, Decl},
  trace,
};

use crate::syntax::core::{Lvl, Set, Tm, TmAbs, TmAll, TmApp, Val, ValAbs, ValAll, ValApp, ValVar};

use self::{state::State, data::elab_data};
use crate::diagnostics::Result;

mod data;
mod eval;
mod func;
mod state;

// -----------------------------------------------------------------------------------------------------------------------------------
// Terms


pub fn elab_tm_chk(tm: Tm, ty: Val, state: &State) -> Result<()> {
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

pub fn elab_tm_inf(tm: Tm, state: &State) -> Result<Val> {
  let tm_fmt = format!("{tm}");
  let ty = match tm {
    Tm::Var(x) => state.resolve(&x),
    Tm::Data(x) | Tm::Cstr(x) | Tm::Func(x) => state.resolve_global(&x),
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
    tm @ Tm::Abs(_) => return Err(Error::from(ElabErr::AttemptAbsInfer { tm })),
    Tm::All(TmAll { dom, codom, span, .. }) => match (elab_tm_inf(*dom, state)?, elab_tm_inf(*codom, state)?) {
      (Val::Set(Set { level: level1, .. }), Val::Set(Set { level: level2, .. })) => Val::Set(Set { level: level1.max(level2), span }),
      (Val::Set(_), s) | (s, Val::Set(_) | _) => return Err(Error::from(ElabErr::ExpectedSetAll { got: s })),
    },
    Tm::Set(Set { level, span }) => Val::Set(Set { level: level + 1, span }),
  };
  trace!("elab_tm_inf", "inferred type of `{}` to be `{}`", tm_fmt, ty);
  Ok(ty)
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Programs

pub fn elab(prog: Prog) -> Result<()> {
  let mut state = State::default();
  elab_prog(prog, &mut state)
}

pub fn elab_prog(prog: Prog, state: &mut State) -> Result<()> {
  prog.decls.into_iter().map(|decl| elab_decl(decl, state)).collect::<Result<Vec<_>>>()?;
  assert!(state.is_only_globals());
  // let ty = elab_tm_inf(prog.ty.clone(), state)?;
  // expected_set(&ty, None)?;
  // elab_tm_chk(prog.tm, eval(prog.ty, &state.env), state)?;
  Ok(())
}

pub fn elab_decl(decl: Decl, state: &mut State) -> Result<()> {
  state.forget(|state| match decl {
    Decl::Data(data) => elab_data(data, state),
    Decl::Func(_) => unimplemented!(),
  })
}

fn eq(ty1: Val, ty2: Val, lvl: Lvl) -> std::result::Result<(), (Val, Val)> {
  let ty1_fmt = format!("{ty1}");
  let ty2_fmt = format!("{ty2}");
  trace!("eq", "test for type equality of `{}` and `{}`", ty1_fmt, ty2_fmt,);
  match (ty1, ty2) {
    (Val::Set(Set { level: level1, .. }), Val::Set(Set { level: level2, .. })) if level1 == level2 => Ok(()),
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
    (Val::Data(ident1), Val::Data(ident2)) if ident1 == ident2 => Ok(()),
    (Val::Cstr(ident1), Val::Cstr(ident2)) if ident1 == ident2 => Ok(()),
    (Val::Func(ident1), Val::Func(ident2)) if ident1 == ident2 => Ok(()),
    (Val::App(ValApp { left: left1, right: right1, .. }), Val::App(ValApp { left: left2, right: right2, .. })) => {
      eq(*left1, *left2, lvl)?;
      eq(*right1, *right2, lvl)
    }
    (ty1, ty2) => Err((ty1, ty2)),
  }
}

pub fn expected_set(ty: &Val, max_lvl: Option<usize>) -> Result<()> {
  if let Val::Set(Set { level, .. }) = ty {
    if let Some(max_lvl) = max_lvl {
      if *level > max_lvl {
        return Err(Error::from(ElabErr::LevelTooHigh { tm: ty.clone(), max: max_lvl }));
      }
    }
    return Ok(());
  }
  Err(Error::from(ElabErr::ExpectedSetCtx { got: ty.clone() }))
}
