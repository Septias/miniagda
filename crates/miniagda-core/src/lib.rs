#![allow(dead_code)]
#![feature(type_alias_impl_trait)]

pub mod syntax;
pub mod diagnostic;
pub mod normalize;
pub mod parsing;
pub mod elaborate;

type Result<T> = std::result::Result<T, diagnostic::Diag>;
