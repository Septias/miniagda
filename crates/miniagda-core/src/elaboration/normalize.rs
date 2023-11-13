use crate::{
  diagnostics::span::Span,
  syntax::core::{Idx, Lvl, Tm, TmAbs, TmAll, TmApp, TmVar, Val, ValAbs, ValAll, ValApp, ValVar},
};

impl Lvl {
  fn as_idx(&self, lvl: Lvl) -> Idx {
    Idx::from(lvl.0 - self.0 - 1)
  }
}

impl ValVar {
  pub fn from_lvl(lvl: Lvl) -> Self {
    ValVar {
      name: "".to_owned(),
      lvl,
      span: Span::dummy(),
    }
  }
}

pub fn nf(tm: Tm, env: &[Val]) -> Tm {
  quote(eval(tm, env), Lvl::from(env.len()))
}

fn quote(val: Val, lvl: Lvl) -> Tm {
  match val {
    Val::Var(ValVar { name, lvl: x, span }) => Tm::Var(TmVar { name, idx: x.as_idx(lvl), span }),
    Val::Glo(x) => Tm::Glo(x),
    Val::App(ValApp { left, right, span }) => Tm::App(TmApp {
      left: Box::new(quote(*left, lvl)),
      right: Box::new(quote(*right, lvl)),
      span,
    }),
    Val::Abs(ValAbs { env, ident, ty, body, span }) => {
      let mut nenv = env.clone();
      nenv.push(Val::Var(ValVar::from_lvl(lvl)));
      Tm::Abs(TmAbs {
        ident,
        ty: Box::new(quote(*ty, lvl)),
        body: Box::new(quote(eval(body, &nenv), lvl + 1)),
        span,
      })
    }
    Val::All(ValAll { env, ident, dom, codom, span }) => {
      let mut nenv = env.clone();
      nenv.push(Val::Var(ValVar::from_lvl(lvl)));
      Tm::All(TmAll {
        ident,
        dom: Box::new(quote(*dom, lvl)),
        codom: Box::new(quote(eval(codom, &nenv), lvl + 1)),
        span,
      })
    }
    Val::Set(set) => Tm::Set(set.clone()),
  }
}

fn env_resolve(env: &[Val], x: TmVar) -> Val {
  // if this panics, implementation is wrong, there are no runtime errors!
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
  match tm {
    Tm::Var(x) => env_resolve(env, x),
    Tm::Glo(x) => Val::Glo(x.clone()),
    Tm::App(TmApp { left, right, span }) => match eval(*left, env) {
      Val::Abs(ValAbs { env, body, .. }) => {
        let mut nenv = env.clone();
        nenv.push(eval(*right, &env));
        eval(body, &nenv)
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
  }
}
