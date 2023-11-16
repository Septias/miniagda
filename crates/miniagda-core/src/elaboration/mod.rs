use crate::syntax::core::Prog;

use self::elaborate::{elab_prog, State};
use crate::diagnostics::Result;

pub mod elaborate;
pub mod normalize;

pub fn elab(prog: Prog) -> Result<()> {
  let mut state = State::default();
  elab_prog(prog, &mut state)
}
