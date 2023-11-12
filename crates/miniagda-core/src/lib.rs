#![allow(dead_code)]
#![feature(type_alias_impl_trait)]

pub mod ast;
pub mod diagnostic;
pub mod parser;

type Result<T> = std::result::Result<T, diagnostic::Diag>;
