use sourcegen::{add_preamble, ensure_file_contents, project_root, reformat};
use xshell::{cmd, Shell};

#[test]
fn gen_ffi() {
    // messes with caching
    if std::env::var("CI").is_ok() || std::env::var("RUN_SLOW_TESTS").is_err() {
        return;
    }
    let vae_dir = project_root().join("openvaf/verilogae");

    // rustc_bootstrap is used to allow macro expansion on stable. It ain't pretty but its fine here since
    // we don't actually use this to compile code and the generated code is all handchecked and commit
    // the vcs

    let sh = Shell::new().unwrap();
    let _env = sh.push_env("RUSTC_BOOTSTRAP", "1");

    let cpp_cfg = project_root().join("openvaf/verilogae_ffi/cppbindgen.toml");
    let cpp_header = project_root().join("include/verilogae.hpp");
    let cpp_header_content = cmd!(sh, "cbindgen {vae_dir} -c {cpp_cfg}").read().unwrap();
    ensure_file_contents(&cpp_header, &cpp_header_content);

    let c_cfg = project_root().join("openvaf/verilogae_ffi/cbindgen.toml");
    let c_header = project_root().join("include/verilogae.h");
    let c_header_content = cmd!(sh, "cbindgen {vae_dir} -c {c_cfg}").read().unwrap();
    ensure_file_contents(&c_header, &c_header_content);

    let res = cmd!(sh, "bindgen {cpp_header} --no-layout-tests --disable-name-namespacing --allowlist-function vae::verilogae_.*
--rustified-enum vae::OptLevel --blacklist-type=vae::NativePath --blacklist-type=vae::FatPtr --blacklist-type=vae::Meta  --allowlist-var=vae::PARAM_FLAGS.* --disable-header-comment").read().unwrap();
    let mut off = 0;
    for line in res.split_terminator('\n') {
        if line.contains("pub type") && (line.contains("__uint") || line.contains("__int")) {
            off += line.len() + 1
        } else {
            break;
        }
    }
    let file_string = format!("{}\n{}", "use super::{NativePath, FatPtr};", &res[off..]);
    let file_string = add_preamble("gen_ffi", reformat(file_string));
    let file = project_root().join("openvaf/verilogae_ffi/src/ffi/generated.rs");
    ensure_file_contents(&file, &file_string);
}
