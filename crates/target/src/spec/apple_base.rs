use std::env;

use crate::spec::TargetOptions;

pub fn opts() -> TargetOptions {
    TargetOptions {
        // This environment variable is pretty magical but is intended for
        // producing deterministic builds. This was first discovered to be used
        // by the `ar` tool as a way to control whether or not mtime entries in
        // the archive headers were set to zero or not. It appears that
        // eventually the linker got updated to do the same thing and now reads
        // this environment variable too in recent versions.
        //
        // For some more info see the commentary on #47086
        link_env: vec![("ZERO_AR_DATE".to_string(), "1".to_string())],
        is_like_osx: true,
        ..TargetOptions::default()
    }
}

fn macos_deployment_target() -> (u32, u32) {
    let deployment_target = env::var("MACOSX_DEPLOYMENT_TARGET").ok();
    let version = deployment_target
        .as_ref()
        .and_then(|s| {
            let mut i = s.splitn(2, '.');
            i.next().and_then(|a| i.next().map(|b| (a, b)))
        })
        .and_then(|(a, b)| a.parse::<u32>().and_then(|a| b.parse::<u32>().map(|b| (a, b))).ok());

    version.unwrap_or((10, 7))
}

pub fn macos_llvm_target(arch: &str) -> String {
    let (major, minor) = macos_deployment_target();
    format!("{}-apple-macosx{}.{}.0", arch, major, minor)
}
