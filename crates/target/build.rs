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
    gen_msvcrt_importlib(&sh, "x64", "x86_64", "i386:x86-64", check);
    gen_msvcrt_importlib(&sh, "arm64", "aarch64", "arm64", check);
}

/// Reads an environment variable and adds it to dependencies.
/// Supposed to be used for all variables except those set for build scripts by cargo
/// <https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-build-scripts>
fn tracked_env_var_os<K: AsRef<OsStr> + Display>(key: K) -> Option<OsString> {
    println!("cargo:rerun-if-env-changed={}", key);
    env::var_os(key)
}

fn gen_msvcrt_importlib(sh: &Shell, arch: &str, target: &str, machine: &str, check: bool) {
    let def_dir =
        stdx::project_root().join("crates").join("target").join("ucrt").join("defs").join(arch);
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let out_file = out_dir.join(format!("ucrt_{arch}.lib"));
    if check {
        sh.write_file(out_file, &[]).expect("failed to write dummy file");
        return;
    }
    let mut libs = Vec::new();
    for src_file in std::fs::read_dir(def_dir).expect("access to crates/target/ucrt/defs") {
        let src_file = src_file.expect("access to files in crates/target/ucrts/defs").path();
        let dst_file = out_dir.join(src_file.file_name().unwrap()).with_extension("lib");

        println!("cargo:rerun-if-changed={}", src_file.display());
        cmd!(sh, "llvm-dlltool -m {machine} -d {src_file} -l {dst_file}")
            .run()
            .expect("importlib generation is successful");
        libs.push(dst_file);
    }

    let ucrt_src = stdx::project_root().join("crates").join("target").join("ucrt").join("ucrt.c");
    println!("cargo:rerun-if-changed={}", ucrt_src.display());
    let ucrt_obj = out_dir.join("ucrt.obj");
    cmd!(
        sh,
        "clang-cl /c /Zl /GS- /clang:--target={target}-pc-windows-msvc /clang:-o{ucrt_obj} {ucrt_src}"
    )
    .run()
    .expect("ucrt compilation succedes");
    libs.push(ucrt_obj);
    let libs_ref = &libs;
    cmd!(sh, "llvm-lib {libs_ref...} /OUT:{out_file}").run().expect("successfull linking");

    for lib in &libs {
        let _ = sh.remove_path(lib);
    }
}
