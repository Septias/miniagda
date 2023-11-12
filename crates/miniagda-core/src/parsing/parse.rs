use super::lex::{Braced, SpannedToks, Token};
use crate::syntax::surface::{self, Cstr, Ctx, Decl, Ident, Prog, Tm, TmAbs, TmAll, TmApp, TmSet};
use crate::diagnostics::span::{Span, Spanned};

peg::parser! {
    pub grammar parser<'a>(file: &str) for SpannedToks<'a, Braced<Token<'a>>> {
        use Token::*;
        use Braced::{Begin, End, Item};
        use Braced::Token as Tok;

        rule id() -> Ident
            = start:position!() [Tok(Id(x))] end:position!() {
              Ident { name: x.to_owned(), span: Span { file: file.to_string(), start, end }
            }
        }

        rule tm() -> Tm = precedence!{
          [Tok(ParenL)] tm:tm() [Tok(ParenR)] { Tm::Brc(Box::new(tm)) }
          [Tok(BraceL)] _:id() [Tok(Equals)] tm:tm() [Tok(BraceR)] { Tm::Brc(Box::new(tm)) }
          --
          start:position!() [Tok(Lambda)] bind:bind() [Tok(Arrow)] body:tm() end:position!() {
              Tm::Abs(TmAbs { ident: bind.0, ty: Box::new(bind.1), body: Box::new(body), span: Span { file: file.to_string(), start, end }  })
          }
          start:position!()  [Tok(All)] bind:bind() [Tok(Arrow)] body:tm() end:position!() {
              Tm::All(TmAll { ident: bind.0, dom: Box::new(bind.1), codom: Box::new(body), span: Span { file: file.to_string(), start, end }  })
          }
          --
          left:(@) right:tm() end:position!() {
            let left_span = left.span();
            let right_span = right.span();
            roll_app(left, right)
          }
          ident:id() {
            if ident.name.starts_with("Set") {
              if let Ok(level) = ident.name[3..].parse::<usize>() {
                return Tm::Set(TmSet { level, span: ident.span })
              }
              if ident.name.len() == 3 {
                return Tm::Set(TmSet { level: 0, span: ident.span })
              }
            }
            Tm::Var(ident)
          }
        }

        rule bind() -> (Ident, Tm)
          = [Tok(ParenL)] ident:id() [Tok(Colon)] tm:tm() [Tok(ParenR)] { (ident, tm) }

        rule ctx() -> Ctx
          = start:position!() ctx:bind()* end:position!() {
            Ctx { ctx, span: Span { file: file.to_string(), start, end } }
          }

        rule ctx1() -> Ctx
          = start:position!() ctx:bind()+ end:position!() {
            Ctx { ctx, span: Span { file: file.to_string(), start, end } }
          }

        rule cstr_rhs(name: &str) -> (Ctx, Vec<Tm>)
          = ctx:ctx1() [Tok(Arrow)] ident:id() tm:tm()? {?
            if (name != ident.name) {
              return Err("type of data type constructors are expected to end in data type itself")
            }
            Ok((ctx, tm.map(unroll_app).unwrap_or_else(Vec::new)))
          }
          / pos:position!() ident:id() tm:tm()? {?
            if (name != ident.name) {
              return Err("type of data type constructors are expected to end in data type itself")
            }
            Ok((Ctx { ctx: vec![], span: Span{ file: file.to_string(), start:pos, end:pos } }, tm.map(unroll_app).unwrap_or_else(Vec::new)))
          }

      rule cstr(data : &Ident) -> Cstr
        = start:position!() [Item] ident:id() [Tok(Colon)] rhs:cstr_rhs(&data.name) end:position!() {
          Cstr { ident, data: data.clone(), args: rhs.0, params: rhs.1, span: Span { file: file.to_string(), start, end } }
        }

      rule indices_and_level() -> (Ctx, usize)
        = ctx:ctx1() [Tok(Arrow)] tm:tm() {?
          if let Tm::Set(TmSet { level, span }) = tm {
            return Ok((ctx, level))
          }
          Err("indices of data type definitions are expected to end in `Set`")
        }
        / pos:position!() tm:tm() {?
          if let Tm::Set(TmSet { level, span }) = tm {
            return Ok((Ctx { ctx: vec![], span: Span{ file: file.to_string(), start:pos, end:pos } }, level))
          }
          Err("indices of data type definitions are expected to end in `Set`")
        }

      rule data() -> surface::Data
        = start:position!() [Tok(Data)] ident:id() params:ctx() [Tok(Colon)] ial:indices_and_level() [Tok(Where)]
          [Begin] cstrs:cstr(&ident)* [End] end:position!() {
            surface::Data { ident, params, indices: ial.0, level: ial.1, cstrs, span: Span { file: file.to_string(), start, end } }
        }

      rule decl() -> Decl
        = [Item] data:data() { Decl::Data(data) }

      pub rule prog() -> Prog
       = start:position!() [Begin] decls:decl()*
          [Item] ident1:id() [Tok(Colon)] ty:tm() [Item] ident2:id() [Tok(Equals)] tm:tm() [End] end:position!() {
            assert!(ident1.name == ident2.name);
            Prog { decls, tm, ty, span: Span { file: file.to_string(), start, end } }
       }
    }
}

fn unroll_app(app: Tm) -> Vec<Tm> {
  match app {
    Tm::App(TmApp { left, right, .. }) => {
      let mut v = unroll_app(*left);
      v.push(*right);
      v
    }
    tm => vec![tm],
  }
}

fn roll_app(left: Tm, right: Tm) -> Tm {
  let left_span = left.span();
  let right_span = right.span();
  match right {
    Tm::App(TmApp {
      left: left1,
      right: right1,
      ..
    }) => Tm::App(TmApp {
      left: Box::new(roll_app(left, *left1)),
      right: right1,
      span: Span {
        file: left_span.file.to_string(),
        start: left_span.start,
        end: right_span.end,
      },
    }),
    tm => Tm::App(TmApp {
      left: Box::new(left),
      right: Box::new(tm),
      span: Span {
        file: left_span.file.clone(),
        start: left_span.start,
        end: right_span.end,
      },
    }),
  }
}
