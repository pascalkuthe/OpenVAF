use crate::spec::TargetOptions;

use super::LinkerFlavor;

pub fn opts() -> TargetOptions {
    TargetOptions {
        linker_flavor: LinkerFlavor::Ld64,
        is_like_osx: true,
        ..TargetOptions::default()
    }
}
