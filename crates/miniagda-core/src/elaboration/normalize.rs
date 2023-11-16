use crate::diagnostics::span::Span;
use crate::syntax::core::{Env, Lvl, Tm, TmAbs, TmAll, TmApp, TmVar, Val, ValAbs, ValAll, ValApp, ValVar};
use crate::trace;

impl ValVar {
  pub fn from_lvl(lvl: Lvl) -> Self {
    ValVar {
      name: "$γ".to_owned(),
      lvl,
      span: Span::default(),
    }
  }
}

impl Env {
  pub fn ext_lvl(&mut self, lvl: Lvl) -> Self {
    self.ext(Val::Var(ValVar::from_lvl(lvl)))
  }
  pub fn ext(&mut self, val: Val) -> Self {
    let mut env = self.to_owned();
    env.0.insert(0, val);
    env
  }

  fn resolve(&self, x: TmVar) -> Val {
    // if this panics, implementation is wrong
    trace!(
      "env_reosolve",
      "resolving `{}` from env `[{}]`",
      x,
      self.0.iter().map(|v| format!("{v}")).collect::<Vec<String>>().join(", ")
    );
    match &self.0[x.idx.0] {
      // copy name and span from actual var
      Val::Var(ValVar { lvl, .. }) => Val::Var(ValVar {
        name: x.name,
        lvl: *lvl,
        span: x.span,
      }),
      v => v.clone(),
    }
  }
}

pub fn eval(tm: Tm, env: &Env) -> Val {
  let tm_str = format!("{tm}");
  let val = match tm {
    Tm::Var(x) => env.resolve(x),
    Tm::Glo(x) => Val::Glo(x.clone()),
    Tm::App(TmApp { left, right, span }) => match eval(*left, env) {
      Val::Abs(ValAbs { mut env, body, .. }) => {
        let right = eval(*right, &env);
        eval(body, &env.ext(right))
      }
      v => Val::App(ValApp {
        left: Box::new(v),
        right: Box::new(eval(*right, env)),
        span,
      }),
    },
    Tm::Abs(TmAbs { ident, ty, body, span }) => Val::Abs(ValAbs {
      env: env.clone() ,
      ident,
      ty: Box::new(eval(*ty, env)),
      body: *body,
      span,
    }),
    Tm::All(TmAll { ident, dom, codom, span }) => Val::All(ValAll {
      env: env.clone(),
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
