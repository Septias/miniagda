use std::{fmt::Debug, ops::Range};

use logos::{Lexer, Logos};
use peg::{Parse, ParseElem, ParseSlice, RuleResult};

use crate::diagnostic::LexErr;

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
#[logos(skip r"[ \t\f]+")]
#[logos(skip r"#[^\n]+")]
#[logos(error = LexErr)]
pub enum Token<'a> {
  #[token("data")]
  Data,
  #[token("where")]
  Where,
  #[token("(")]
  ParenL,
  #[token(")")]
  ParenR,
  #[token("{")]
  BraceL,
  #[token("}")]
  BraceR,
  #[token(":")]
  Colon,
  #[token("→")]
  Arrow,
  #[token("λ")]
  Lambda,
  #[token("∀")]
  All,
  #[token("=")]
  Equals,
  #[regex("\n|\r\n|\n\r")]
  NewLine,
  #[regex("[a-zA-Z_]+[a-zA-Z0-9_]*")]
  Id(&'a str),
}

// -----------------------------------------------------------------------------------
// thanks Hannes bb :*

pub fn lex(src: &str) -> Result<SpannedToks<'_, Token<'_>>, LexErr> {
  let lex: Lexer<Token> = Token::lexer(src);
  Ok(SpannedToks {
    src,
    toks: lex
      .spanned()
      .map(|(tok, span)| {
        tok.map(|tok| {
          Spanned::new(
            tok,
            Span {
              start: span.start,
              end: span.end,
            },
          )
        })
      })
      .collect::<Result<Vec<_>, _>>()?,
  })
}

type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
  pub val: Box<T>,
  pub span: Span,
}

impl<T> Spanned<T> {
  pub fn new(val: T, span: Span) -> Self {
    Self {
      val: Box::new(val),
      span,
    }
  }
}

pub struct SpannedToks<'a, T: 'a> {
  pub src: &'a str,
  pub toks: Vec<Spanned<T>>,
}

impl<'a, T: Debug> Debug for SpannedToks<'a, T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for t in &self.toks {
      writeln!(f, "{:?}\t  {:?}", t.span, t.val)?;
    }
    Ok(())
  }
}

impl<'a, T> Parse for SpannedToks<'a, T> {
  type PositionRepr = usize;

  fn start(&self) -> usize {
    0
  }

  fn is_eof(&self, pos: usize) -> bool {
    pos >= self.toks.len()
  }

  fn position_repr(&self, pos: usize) -> Self::PositionRepr {
    if pos < self.toks.len() {
      self.toks[pos].span.start
    } else {
      self.toks.last().unwrap().span.end
    }
  }
}

impl<'input, T: 'input + Copy> ParseElem<'input> for SpannedToks<'input, T> {
  type Element = T;

  fn parse_elem(&'input self, pos: usize) -> RuleResult<T> {
    match self.toks[pos..].first() {
      Some(c) => RuleResult::Matched(pos + 1, *c.val),
      None => RuleResult::Failed,
    }
  }
}

impl<'input, T: 'input> ParseSlice<'input> for SpannedToks<'input, T> {
  type Slice = &'input str;
  fn parse_slice(&'input self, p1: usize, p2: usize) -> &'input str {
    let p1 = self.position_repr(p1);
    let p2 = self.position_repr(p2);
    &self.src[p1..p2]
  }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Braced<T> {
  Token(T),
  Begin,
  End,
  Item,
}

pub fn process_indent<T>(
  stoks: SpannedToks<T>,
  mut is_block_start: impl FnMut(&T) -> bool,
  mut is_newline: impl FnMut(&T) -> bool,
) -> SpannedToks<Braced<T>> {
  let mut toks = vec![];
  let mut indent_stack = vec![0];
  let mut waiting = false;
  let mut last_newline = 0;

  toks.push(Spanned::new(Braced::Begin, Span { start: 0, end: 0 }));
  for stok in stoks.toks {
    let tok = *stok.val;
    let span = stok.span;
    let span_end = Span {
      start: span.end,
      end: span.end,
    };
    let span_col = span.start - last_newline;

    // Skip newlines but remember their positions.
    if is_newline(&tok) {
      last_newline = span.end;
      continue;
    }

    // Pop indents larger than the start of this token and add corresponding end-tokens.
    let mut drop = 0;
    for i in indent_stack.iter().cloned().rev() {
      if i <= span_col {
        break;
      }
      drop += 1
    }
    for _ in 0..drop {
      indent_stack.pop();
      toks.push(Spanned::new(Braced::End, span_end.clone()));
    }

    // If the current token starts a new item, push a separation token.
    let started_new_item = span_col == *indent_stack.last().unwrap();
    if started_new_item {
      toks.push(Spanned::new(Braced::Item, span_end.clone()));
    }

    // If the previous token started a block, push a new indent from the current token.
    if waiting {
      if drop > 0 || started_new_item {
        // Terminate an empty block.
        toks.push(Spanned::new(Braced::End, span_end.clone()));
      } else {
        toks.push(Spanned::new(Braced::Item, span_end.clone()));
        indent_stack.push(span_col);
      }
      waiting = false;
    }

    let is_start = is_block_start(&tok);

    // Push the current token into the output token stream.
    toks.push(Spanned::new(Braced::Token(tok), span.clone()));

    // If the current token is a start token, start a new block.
    if is_start {
      toks.push(Spanned::new(Braced::Begin, span_end));
      waiting = true;
    }
  }

  // Close all blocks which are still open at the end of the token stream.
  let span_end = Span {
    start: stoks.src.len(),
    end: stoks.src.len(),
  };
  for _ in indent_stack {
    toks.push(Spanned::new(Braced::End, span_end.clone()));
  }

  SpannedToks {
    src: stoks.src,
    toks,
  }
}
