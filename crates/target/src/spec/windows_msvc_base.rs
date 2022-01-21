use crate::spec::{LinkArgs, LinkerFlavor, TargetOptions};

pub fn opts() -> TargetOptions {
    let pre_link_args_msvc = vec![
        // Suppress the verbose logo and authorship debugging output, which would needlessly
        // clog any log files.
        "/NOLOGO".to_string(),
    ];
    let mut pre_link_args = LinkArgs::new();
    pre_link_args.insert(LinkerFlavor::Msvc, pre_link_args_msvc);
    // pre_link_args.insert(LinkerFlavor::Lld(LldFlavor::Link), pre_link_args_msvc);

    TargetOptions {
        //       dll_prefix: "".to_string(),
        is_like_windows: true,
        is_like_msvc: true,
        linker_flavor: LinkerFlavor::Msvc,
        pre_link_args,
        ..Default::default()
    }
}
