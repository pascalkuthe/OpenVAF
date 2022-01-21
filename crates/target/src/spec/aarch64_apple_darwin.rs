use crate::spec::{Target, LinkerFlavor, TargetOptions};


pub fn target() -> Target {
    let mut base = super::apple_base::opts();
    base.cpu = "apple-a14".to_string();

    base.pre_link_args.insert(LinkerFlavor::Gcc, vec!["-arch".to_string(), "arm64".to_string()]);

    // Clang automatically chooses a more specific target based on
    // MACOSX_DEPLOYMENT_TARGET.  To enable cross-language LTO to work
    // correctly, we do too.
    let llvm_target = super::apple_base::macos_llvm_target("arm64");

    Target {
        llvm_target,
        pointer_width: 64,
        data_layout: "e-m:o-i64:64-i128:128-n32:64-S128".to_string(),
        arch: "aarch64".to_string(),
        options: TargetOptions {
            ..base
        },
    }
}
