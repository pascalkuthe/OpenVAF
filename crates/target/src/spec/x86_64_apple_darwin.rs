use crate::spec::{LinkerFlavor, Target};

pub fn target() -> Target {
    let mut base = super::apple_base::opts();
    base.cpu = "core2".to_string();
    base.pre_link_args.insert(
        LinkerFlavor::Gcc,
        vec!["-m64".to_string(), "-arch".to_string(), "x86_64".to_string()],
    );
    // Clang automatically chooses a more specific target based on
    // MACOSX_DEPLOYMENT_TARGET.  To integrate correctly with the target simulator we do too
    let arch = "x86_64";
    let llvm_target = super::apple_base::macos_llvm_target(arch);

    Target {
        llvm_target,
        arch: arch.to_string(),
        data_layout: "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
            .to_string(),
        options: base,
        pointer_width: 64,
    }
}
