use std::path::{Path, PathBuf};
use std::{env, fs};

mod ieee64;
pub mod iter;
mod macros;
pub mod packed_option;
pub mod pretty;
pub mod vec;
pub use crate::ieee64::Ieee64;

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

/// Returns `false` if slow tests should not run, otherwise returns `true` and
/// also creates a file at `./target/.slow_tests_cookie` which serves as a flag
/// that slow tests did run.
pub fn skip_slow_tests() -> bool {
    let should_skip = option_env!("CI").is_none() && env::var("RUN_SLOW_TESTS").is_err();
    if should_skip {
        std::eprintln!("ignoring slow test");
    } else {
        let path = project_root().join("./target/.slow_tests_cookie");
        fs::write(path, ".").unwrap();
    }
    should_skip
}

pub fn ignore_dev_tests<T: ?Sized>(_: &T) -> bool {
    env::var("RUN_DEV_TESTS").is_err()
}

pub fn ignore_slow_tests<T: ?Sized>(_: &T) -> bool {
    skip_slow_tests()
}

pub fn ignore_never<T: ?Sized>(_: &T) -> bool {
    false
}

pub fn openvaf_test_data(test: &str) -> PathBuf {
    project_root().join("openvaf").join("test_data").join(test)
}

pub fn is_va_file(path: &Path) -> bool {
    path.extension().and_then(|ext| ext.to_str()).map_or(false, |ext| ext == "va")
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}
