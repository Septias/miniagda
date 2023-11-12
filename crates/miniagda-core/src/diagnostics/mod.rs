use self::error::Error;

pub mod error;
pub mod span;

pub type Result<T> = std::result::Result<T, Error>;
