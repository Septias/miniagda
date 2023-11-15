use crate::diagnostics::span::Span;
use crate::syntax::core::{Lvl, Tm, TmAbs, TmAll, TmApp, TmVar, Val, ValAbs, ValAll, ValApp, ValVar};
use crate::trace;

impl ValVar {
  pub fn from_lvl(lvl: Lvl) -> Self {
    ValVar {
      name: "$γ".to_owned(),
      lvl,
      span: Span::dummy(),
    }
  }
}

// TODO: Env wrapper

pub fn env_ext_lvl(env: Vec<Val>, lvl: Lvl) -> Vec<Val> {
  env_ext(env, Val::Var(ValVar::from_lvl(lvl)))
}

pub fn env_ext(env: Vec<Val>, val: Val) -> Vec<Val> {
  let mut env = env.to_owned();
  env.insert(0, val);
  env
}

fn env_resolve(env: &[Val], x: TmVar) -> Val {
  // if this panics, implementation is wrong
  trace!(
    "env_reosolve",
    "resolving `{}` from env `[{}]`",
    x,
    env.iter().map(|v| format!("{v}")).collect::<Vec<String>>().join(", ")
  );
  match &env[x.idx.0] {
    // copy name and span from actual var
    Val::Var(ValVar { lvl, .. }) => Val::Var(ValVar {
      name: x.name,
      lvl: *lvl,
      span: x.span,
    }),
    v => v.clone(),
  }
}

pub fn eval(tm: Tm, env: &[Val]) -> Val {
  let tm_str = format!("{tm}");
  let val = match tm {
    Tm::Var(x) => env_resolve(env, x),
    Tm::Glo(x) => Val::Glo(x.clone()),
    Tm::App(TmApp { left, right, span }) => match eval(*left, env) {
      Val::Abs(ValAbs { env, body, .. }) => {
        let right = eval(*right, &env);
        eval(body, &env_ext(env, right))
      }
      v => Val::App(ValApp {
        left: Box::new(v),
        right: Box::new(eval(*right, env)),
        span,
      }),
    },
    Tm::Abs(TmAbs { ident, ty, body, span }) => Val::Abs(ValAbs {
      env: env.to_vec(),
      ident,
      ty: Box::new(eval(*ty, env)),
      body: *body,
      span,
    }),
    Tm::All(TmAll { ident, dom, codom, span }) => Val::All(ValAll {
      env: env.to_vec(),
      ident,
      dom: Box::new(eval(*dom, env)),
      codom: *codom,
      span,
    }),
    Tm::Set(set) => Val::Set(set),
  };
  trace!("eval", "evaluated `{}` to `{}`", tm_str, val);
  val
}
