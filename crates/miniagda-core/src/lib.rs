#![allow(dead_code)]
#![feature(type_alias_impl_trait)]

mod context;
mod core;
mod diagnostic;
mod surface;
mod surface_to_core;

type Result<T> = std::result::Result<T, diagnostic::Diag>;
