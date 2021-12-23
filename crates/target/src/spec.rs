mod apple_base;
mod linux_base;
mod windows_msvc_base;

use crate::host_triple;

// #[derive(Debug, Clone, Copy, Eq, Ord, PartialOrd, PartialEq, Hash)]
// pub enum LinkerFlavor {
//     Ld,
//     Ld64,
//     Msvc,
// }

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

    /// Whether the target toolchain is like Windows
    pub is_like_windows: bool,
    pub is_like_msvc: bool,
}

impl Default for TargetOptions {
    fn default() -> Self {
        TargetOptions {
            is_builtin: false,
            cpu: "generic".to_string(),
            features: "".to_string(),
            is_like_windows: false,
            is_like_msvc: false,
        }
    }
}
#[derive(Debug)]
pub enum LoadTargetError {
    BuiltinTargetNotFound(String),
    Other(String),
}

pub type TargetResult = Result<Target, String>;

macro_rules! supported_targets {
    ( $(($( $triple:literal, )+ $module:ident ),)+ ) => {
        $ ( mod $ module; ) +

        /// List of supported targets
        const TARGETS: &[&str] = &[$($($triple),+),+];

        fn load_specific(target: &str) -> Result<Target, LoadTargetError> {
            match target {
                $(
                    $($triple)|+ => {
                        let mut t = $module::target()
                            .map_err(LoadTargetError::Other)?;
                        t.options.is_builtin = true;

                        Ok(t)
                    },
                )+
                    _ => Err(LoadTargetError::BuiltinTargetNotFound(
                        format!("Unable to find target: {}", target)))
            }
        }

        pub fn get_targets() -> impl Iterator<Item = String> {
            TARGETS.iter().filter_map(|t| -> Option<String> {
                load_specific(t)
                    .and(Ok(t.to_string()))
                    .ok()
            })
        }
    }
}

supported_targets!(
    ("x86_64-apple-darwin", x86_64_apple_darwin),
    ("x86_64-pc-windows-msvc", x86_64_pc_windows_msvc),
    ("x86_64-unknown-linux-gnu", x86_64_unknown_linux_gnu),
    ("x86_64-unknown-linux-musl", x86_64_unknown_linux_musl),
);

impl Target {
    pub fn search(target_triple: &str) -> Result<Target, LoadTargetError> {
        load_specific(target_triple)
    }

    pub fn host_target() -> Result<Target, LoadTargetError> {
        Self::search(host_triple())
    }
}
