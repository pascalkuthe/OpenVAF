use crate::spec::{LinkerFlavor, Target};

pub fn target() -> Target {
    let mut base = super::linux_base::opts();
    base.cpu = "x86-64".to_string();
    let link_args = base.pre_link_args.entry(LinkerFlavor::Ld).or_default();
    for arg in "-m elf_x86_64".split(' ') {
        link_args.push(arg.to_owned())
    }

    Target {
        llvm_target: "x86_64-unknown-linux-gnu".to_string(),
        arch: "x86_64".to_string(),
        data_layout: "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
            .to_string(),
        options: base,
        pointer_width: 64,
    }
}
