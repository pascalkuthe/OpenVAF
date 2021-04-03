
use crate::spec::{LinkerFlavor, TargetOptions};

pub fn opts() -> TargetOptions {
    let mut base = super::linux_gnu_base::opts();
    base.os = "android".to_string();
    // Many of the symbols defined in compiler-rt are also defined in libgcc.
    // Android's linker doesn't like that by default.
    // base.pre_link_args
    //    .entry(LinkerFlavor::Gcc)
    //    .or_default()
    //    .push("-Wl,--allow-multiple-definition".to_string());
    base
}