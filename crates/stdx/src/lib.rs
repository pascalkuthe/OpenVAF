mod macros;
pub mod pretty;
pub mod vec;
pub mod iter;


pub const IS_CI: bool = option_env!("CI").is_some();
