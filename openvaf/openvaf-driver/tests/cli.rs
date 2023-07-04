use std::env;
use std::path::PathBuf;

use mini_harness::{harness, Result};
use stdx::{ignore_never, project_root};

// Adapted from
// https://github.com/rust-lang/cargo/blob/485670b3983b52289a2f353d589c57fae2f60f82/tests/testsuite/support/mod.rs#L507
pub fn target_dir() -> PathBuf {
    env::current_exe()
        .ok()
        .map(|mut path| {
            path.pop();
            if path.ends_with("deps") {
                path.pop();
            }
            path
        })
        .unwrap()
}

/// Returns the path of a binary build by cargo
pub fn cargo_bin(name: &str) -> PathBuf {
    let env_var = format!("CARGO_BIN_EXE_{}", name);
    std::env::var_os(&env_var)
        .map(|p| p.into())
        .unwrap_or_else(|| target_dir().join(format!("{}{}", name, env::consts::EXE_SUFFIX)))
}

fn smoke_test(args: &str) -> Result {
    let args = args.split(' ');
    let sh = xshell::Shell::new().unwrap();
    sh.change_dir(project_root());
    let openvaf = cargo_bin("openvaf");
    xshell::cmd!(sh, "{openvaf} --dry-run {args...} integration_tests/DIODE/diode.va").run()?;
    Ok(())
}

fn link_diode() -> Result {
    let sh = xshell::Shell::new().unwrap();
    sh.change_dir(project_root());
    let openvaf = cargo_bin("openvaf");
    xshell::cmd!(sh, "{openvaf} -O 0 integration_tests/DIODE/diode.va").run()?;
    sh.remove_path("integration_tests/DIODE/diode.osdi").unwrap();
    Ok(())
}

harness! {
    Test::new("cli::link_diode", &link_diode),
    Test::from_list(
        "cli::smoke_test",
         &smoke_test,
         &ignore_never,
         &[
            "--target_cpu generic",
            "--target_cpu skylake",
             "--dump-json",
             "--supported-targets",
             "--batch",
             "--batch --cache-dir sourcegen",
             "-O 0",
             "-O 1",
             "-O 2",
             "-O 3",
             "-I sourcegen",
             "-D foo",
             "--print-expansion",
             "--supported-targets",
             "--lints",
             "-D all",
             "-D warnings",
             "-D macro_overwritten",
             "-W errors",
             "-W all",
             "-W macro_overwritten",
             "-A errors",
             "-A warnings",
             "-A all",
             "-A macro_overwritten",
        ]
    )
}
