use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt::Display;
use std::path::Path;
use target::spec::get_targets;

use xshell::{cmd, Shell};

/// Reads an environment variable and adds it to dependencies.
/// Supposed to be used for all variables except those set for build scripts by cargo
/// <https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-build-scripts>
fn tracked_env_var_os<K: AsRef<OsStr> + Display>(key: K) -> Option<OsString> {
    println!("cargo:rerun-if-env-changed={}", key);
    env::var_os(key)
}

fn main() {
    // If we're just running `check`, there's no need to actually compute the stdlib just
    // popualte dummys
    let no_gen = tracked_env_var_os("RUST_CHECK").is_some();
    let sh = Shell::new().unwrap();
    let osdi_dir = stdx::project_root().join("openvaf").join("osdi");
    let src_file = osdi_dir.join("stdlib.c");

    sh.change_dir(osdi_dir);
    for file in sh.read_dir("header").unwrap() {
        if file.extension().map_or(true, |ext| ext != "h")
            || !file
                .file_stem()
                .and_then(|name| name.to_str())
                .map_or(false, |name| name.starts_with("osdi_"))
        {
            continue;
        }

        let name = file.file_stem().unwrap().to_str().unwrap();
        let def_name = name.to_uppercase();
        let version_str = name.strip_prefix("osdi_").unwrap();

        let out_dir = env::var_os("OUT_DIR").unwrap();
        for target in get_targets() {
            let target_name = &target.llvm_target;
            let out_file =
                Path::new(&out_dir).join(&format!("stdlib_{version_str}_{target_name}.bc"));
            if no_gen {
                sh.write_file(out_file, []).expect("failed to write dummy file");
            } else {
                println!("cargo:rerun-if-changed={}", file.display());

                let mut cmd = cmd!(sh, "clang -emit-llvm -O3 -D{def_name} -DNO_STD -o {out_file} -c {src_file} -target {target_name}");
                if !target.options.is_like_windows {
                    cmd = cmd.arg("-fPIC");
                }
                cmd.run().expect("failed to generate bitcode");
            }
        }
    }

    println!("cargo:rerun-if-changed={}", src_file.display());
}
