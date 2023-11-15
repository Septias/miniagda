use self::error::Error;

pub mod error;
pub mod span;

pub type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! warn {
  ($target:expr, $($arg:tt)+) => (log::warn!(target: format!("{}:{} {}", file!(), line!(), $target).as_str(), $($arg)+));
}

#[macro_export]
macro_rules! info {
  ($target:expr, $($arg:tt)+) => (log::info!(target: format!("{}:{} {}", file!(), line!(), $target).as_str(), $($arg)+));
}

#[macro_export]
macro_rules! debug {
  ($target:expr, $($arg:tt)+) => (log::debug!(target: format!("{}:{} {}", file!(), line!(), $target).as_str(), $($arg)+));
}

#[macro_export]
macro_rules! trace {
  ($target:expr, $($arg:tt)+) => (log::trace!(target: format!("{}:{} {}", file!(), line!(), $target).as_str(), $($arg)+));
}
