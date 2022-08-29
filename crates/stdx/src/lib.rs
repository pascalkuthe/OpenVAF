use std::path::PathBuf;

pub mod iter;
mod macros;
pub mod packed_option;
pub mod pretty;
pub mod vec;

pub const IS_CI: bool = option_env!("CI").is_some();
pub const SKIP_HOST_TESTS: bool = option_env!("CI").is_some() && cfg!(windows);

pub fn project_root() -> PathBuf {
    let dir = env!("CARGO_MANIFEST_DIR");
    let mut res = PathBuf::from(dir);
    while !res.join("README.md").exists() {
        res = res.parent().expect("reached fs root without finding project root").to_owned()
    }
    res
}
