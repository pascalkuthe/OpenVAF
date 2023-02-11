use crate::spec::Target;

const UCRT_IMPORTLIB: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/ucrt_arm64.lib"));
pub fn target() -> Target {
    let mut base = super::windows_msvc_base::opts();
    base.features = "+neon,+fp-armv8".to_string();
    base.import_lib = UCRT_IMPORTLIB;

    Target {
        llvm_target: "aarch64-pc-windows-msvc".to_string(),
        pointer_width: 64,
        data_layout: "e-m:w-p:64:64-i32:32-i64:64-i128:128-n32:64-S128".to_string(),
        arch: "aarch64".to_string(),
        options: base,
    }
}
