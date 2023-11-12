use self::error::Err;

pub mod error;
pub mod span;

pub type Result<T> = std::result::Result<T, Err>;
