pub mod base_n;
pub mod iter;
mod macros;
pub mod pretty;
pub mod vec;

pub const IS_CI: bool = option_env!("CI").is_some();
