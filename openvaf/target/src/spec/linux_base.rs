use crate::spec::{LinkerFlavor, TargetOptions};

pub fn opts() -> TargetOptions {
    let mut opts = TargetOptions::default();

    let link_args = opts.pre_link_args.entry(LinkerFlavor::Ld).or_default();
    for arg in "--no-add-needed --hash-style=gnu".split(' ') {
        link_args.push(arg.to_owned())
    }
    opts
}
