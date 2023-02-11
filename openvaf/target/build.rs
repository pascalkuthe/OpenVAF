use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt::Display;
use std::path::PathBuf;

use xshell::{cmd, Shell};

fn main() {
    println!("cargo:rustc-env=CFG_COMPILER_HOST_TRIPLE={}", std::env::var("TARGET").unwrap());
    // If we're just running `check`, there's no need to actually compute the stdlib just
    // popualte dummys
    let check = tracked_env_var_os("RUST_CHECK").is_some();
    let sh = Shell::new().unwrap();
    gen_msvcrt_importlib(&sh, "x64", "x86_64", check);
    gen_msvcrt_importlib(&sh, "arm64", "aarch64", check);
}

/// Reads an environment variable and adds it to dependencies.
/// Supposed to be used for all variables except those set for build scripts by cargo
/// <https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-build-scripts>
fn tracked_env_var_os<K: AsRef<OsStr> + Display>(key: K) -> Option<OsString> {
    println!("cargo:rerun-if-env-changed={}", key);
    env::var_os(key)
}

fn gen_msvcrt_importlib(sh: &Shell, arch: &str, target: &str, check: bool) {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let out_file = out_dir.join(format!("ucrt_{arch}.lib"));
    if check {
        sh.write_file(out_file, &[]).expect("failed to write dummy file");
        return;
    }
    let mut libs = Vec::new();
    let ucrt_src = stdx::project_root().join("openvaf").join("target").join("src").join("ucrt.c");
    println!("cargo:rerun-if-changed={}", ucrt_src.display());
    let ucrt_obj = out_dir.join(format!("ucrt_{arch}.obj"));
    cmd!(
        sh,
        "clang-cl /c /Zl /GS- /clang:--target={target}-pc-windows-msvc /clang:-o{ucrt_obj} {ucrt_src}"
    )
    .run()
    .expect("ucrt compilation succedes");
    libs.push(ucrt_obj);

    let libs_ref = &libs;
    cmd!(sh, "llvm-lib /machine:{arch} {libs_ref...} /OUT:{out_file}")
        .run()
        .expect("successfull linking");

    for lib in &libs {
        let _ = sh.remove_path(lib);
    }
}
