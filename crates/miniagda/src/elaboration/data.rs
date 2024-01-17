use super::eval::eval;
use super::state::State;
use super::{elab_tm_inf, expected_set};
use crate::debug;
use crate::diagnostics::error::{ElabErr, Error};
use crate::diagnostics::span::{Span, Spanned};
use crate::diagnostics::Result;
use crate::elaboration::elab_tm_chk;
use crate::syntax::core::Env;
use crate::syntax::{
  core::{Cstr, Ctx, Data, Tel, Tm, TmAll, TmApp, Set, TmVar, Val},
  Ident,
};

// -----------------------------------------------------------------------------------------------------------------------------------
// Data Types


pub fn elab_data(data: Data, state: &mut State) -> Result<()> {
  let data_clone = data.clone();

  let level = match data.set {
    Tm::Set(Set { level, .. }) => level,
    tm => return Err(Error::from(ElabErr::ExpectedSetData { got: tm.clone() })),
  };

  let name = data.ident.clone();
  debug!("elab_data", "elaborating data type `{}`", name);
  let as_fn = eval(ctx_to_fn(&data.params, tel_to_fn(&data.indices, data.set)), &Env::default());


  debug!("elab_data", "elaborating parameters `{}` of data type `{}`", data.params, name);
  elab_binds(data.params.binds, data.params.tms, None, state)?;

  debug!("elab_data", "elaborating indices `{}` of data type `{}`", data.indices, name);
  let indices_types = state.forget(|state| elab_binds(data.indices.binds, data.indices.tms, None, state))?;

  state.bind_global(data.ident, as_fn);

  data
    .cstrs
    .into_iter()
    .map(|cstr| state.forget(|env| elab_cstr(cstr, &data_clone, level, &indices_types, env)))
    .collect::<Result<Vec<_>>>()?;
  debug!("elab_data", "elaborated data type `{}`", name);
  Ok(())
}

fn elab_cstr(cstr: Cstr, data: &Data, level: usize, indices_types: &[Val], state: &mut State) -> Result<()> {
  let name = cstr.ident.clone();
  debug!("elab_cstr", "elaborating constructor `{}`", name);

  assert!(!cstr.params.is_empty());

  match &cstr.params[0] {
    Tm::Data(ident) if ident == &data.ident => (),
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
  elab_binds(cstr.args.binds, cstr.args.tms, Some(level), state)?;

  let data_params_len = data.params.binds.len();
  for i in 0..data_params_len {
    if let Some(Tm::Var(TmVar { idx, name: _, .. })) = params.get(i) {
      if idx.0 == data_params_len + args_len - (i + 1) {
        continue;
      }
    }
    return Err(Error::from(ElabErr::ExpectedParam {
      expected: data.params.binds[i].clone(),
      got: params.get(i).cloned(),
    }));
  }

  let indices = &params[data_params_len..];

  let data_indices_len = data.indices.binds.len();
  for i in 0..data_indices_len {
    if indices.get(i).is_some() {
      continue;
    }

    return Err(Error::from(ElabErr::ExpectedIndex {
      expected: data.indices.binds[i].clone(),
      got: indices.get(i).cloned(),
    }));
  }
  if let Some(tm) = indices.get(data_indices_len) {
    return Err(Error::from(ElabErr::UnexpectedArg { got: tm.clone() }));
  }

  debug!(
    "elab_cstr",
    "checking constructor indices `[{}]` match expected types `[{}]`",
    indices.iter().map(|x| format!("{x}")).collect::<Vec<String>>().join(", "),
    data.indices.tms.iter().map(|x| format!("{x}")).collect::<Vec<String>>().join(", ")
  );
  elab_indices(indices, indices_types, &data.indices.binds, state)?;

  state.bind_global(cstr.ident, as_fn);

  // TODO: termination checking

  debug!("elab_cstr", "elaborated constructor `{}`", name);
  Ok(())
}

fn elab_binds(binds: Vec<Ident>, tms: Vec<Tm>, max_lvl: Option<usize>, state: &mut State) -> Result<Vec<Val>> {
  binds
    .into_iter()
    .zip(tms)
    .map(|(Ident { name, .. }, tm)| {
      let ty = elab_tm_inf(tm.clone(), state)?;
      expected_set(&ty, max_lvl)?;
      let tm = eval(tm, &state.env);
      state.bind(name, tm.clone());
      Ok(tm)
    })
    .collect::<Result<Vec<_>>>()
}

fn elab_indices(tms: &[Tm], tys: &[Val], binds: &[Ident], state: &State) -> Result<()> {
  assert!(tms.len() == binds.len() && binds.len() == tms.len());
  if tms.is_empty() {
    return Ok(());
  }
  elab_tm_chk(tms[0].clone(), tys[0].clone(), state)?;
  if binds.len() > 1 {
    return elab_indices(&tms[1..], &tys[1..], &binds[1..], state);
  }
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

fn ctx_to_fn(ctx: &Ctx, end: Tm) -> Tm {
  tms_to_fn(&ctx.tms, &ctx.binds, end)
}

fn tel_to_fn(tel: &Tel, end: Tm) -> Tm {
  tms_to_fn(&tel.tms, &tel.binds, end)
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
