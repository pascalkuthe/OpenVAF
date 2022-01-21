mod apple_base;
mod linux_base;
mod windows_msvc_base;

use std::collections::BTreeMap;

use crate::host_triple;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum LinkerFlavor {
    Gcc,
    Ld,
    Msvc,
}

macro_rules! flavor_mappings {
    ($((($($flavor:tt)*), $string:expr),)*) => (
        impl LinkerFlavor {
            pub const fn one_of() -> &'static str {
                concat!("one of: ", $($string, " ",)*)
            }

            #[allow(clippy::should_implement_trait)]
            pub fn from_str(s: &str) -> Option<Self> {
                Some(match s {
                    $($string => $($flavor)*,)*
                    _ => return None,
                })
            }

            pub fn desc(&self) -> &str {
                match *self {
                    $($($flavor)* => $string,)*
                }
            }
        }
    )
}

flavor_mappings! {
    ((LinkerFlavor::Gcc), "gcc"),
    ((LinkerFlavor::Ld), "ld"),
    ((LinkerFlavor::Msvc), "msvc"),
}

pub type LinkArgs = BTreeMap<LinkerFlavor, Vec<String>>;

/// Everything `rustc` knows about how to compile for a specific target.
///
/// Every field here must be specified, and has no default value.
#[derive(PartialEq, Clone, Debug)]
pub struct Target {
    /// Target triple to pass to LLVM.
    pub llvm_target: String,

    pub pointer_width: u32,
    /// Architecture to use for ABI considerations. Valid options include: "x86",
    /// "x86_64", "arm", "aarch64", "mips", "powerpc", "powerpc64", and others.
    pub arch: String,
    /// [Data layout](https://llvm.org/docs/LangRef.html#data-layout) to pass to LLVM.
    pub data_layout: String,
    /// Optional settings with defaults.
    pub options: TargetOptions,
}

/// Optional aspects of target specification.
#[derive(PartialEq, Clone, Debug)]
pub struct TargetOptions {
    /// True if this is a built-in target
    pub is_builtin: bool,

    /// Default CPU to pass to LLVM. Corresponds to `llc -mcpu=$cpu`. Defaults to "generic".
    pub cpu: String,

    /// Default target features to pass to LLVM. These features will *always* be passed, and cannot
    /// be disabled even via `-C`. Corresponds to `llc -mattr=$features`.
    pub features: String,

    /// Default linker flavor used if `-C linker-flavor` or `-C linker` are not passed
    /// on the command line. Defaults to `LinkerFlavor::Gcc`.
    pub linker_flavor: LinkerFlavor,

    /// Linker to invoke
    pub linker: Option<String>,

    /// Linker arguments that are passed *before* any user-defined libraries.
    pub pre_link_args: LinkArgs,

    /// Linker arguments that are unconditionally passed after any
    /// user-defined but before post-link objects. Standard platform
    /// libraries that should be always be linked to, usually go here.
    pub post_link_args: LinkArgs,

    /// Environment variables to be set for the linker invocation.
    pub link_env: Vec<(String, String)>,
    /// Environment variables to be removed for the linker invocation.
    pub link_env_remove: Vec<String>,

    /// Whether the target toolchain is like Windows
    pub is_like_windows: bool,
    pub is_like_msvc: bool,
    pub is_like_osx: bool,
}

impl Default for TargetOptions {
    fn default() -> Self {
        TargetOptions {
            is_builtin: true,
            cpu: "generic".to_string(),
            features: "".to_string(),
            is_like_windows: false,
            is_like_msvc: false,
            is_like_osx: false,
            linker_flavor: LinkerFlavor::Gcc,
            linker: None,
            pre_link_args: BTreeMap::default(),
            post_link_args: BTreeMap::default(),
            link_env: Vec::new(),
            link_env_remove: Vec::new(),
        }
    }
}

pub type TargetResult = Result<Target, String>;

macro_rules! supported_targets {
    ( $(($( $triple:literal, )+ $module:ident ),)+ ) => {
        $ ( mod $ module; ) +

        // /// List of supported targets
        // const TARGETS: &[&str] = &[$($($triple),+),+];

        fn load_specific(target: &str) -> Option<Target> {
            match target {
                $(
                    $($triple)|+ => {
                        let mut t = $module::target();
                        t.options.is_builtin = true;

                        Some(t)
                    },
                )+
                    _ => None
            }
        }

        // pub fn get_targets() -> impl Iterator<Item = String> {
        //     TARGETS.iter().filter_map(|t| -> Option<String> {
        //         load_specific(t).map(|t|t.to_string())
        //     })
        // }
    }
}

supported_targets!(
    ("x86_64-apple-darwin", x86_64_apple_darwin),
    ("x86_64-pc-windows-msvc", x86_64_pc_windows_msvc),
    ("x86_64-unknown-linux-gnu", x86_64_unknown_linux_gnu),
    ("x86_64-unknown-linux-musl", x86_64_unknown_linux_musl),
    ("aarch64-apple-darwin", aarch64_apple_darwin),
    ("aarch64-pc-windows-msvc", aarch64_pc_windows_msvc),
    ("aarch64-unknown-linux-gnu", aarch64_unknown_linux_gnu),
    ("aarch64-unknown-linux-musl", aarch64_unknown_linux_musl),
);

impl Target {
    pub fn search(target_triple: &str) -> Option<Target> {
        load_specific(target_triple)
    }

    pub fn host_target() -> Option<Target> {
        Self::search(host_triple())
    }
}
