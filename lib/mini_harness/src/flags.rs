use crate::Format;

xflags::xflags! {
    src "./src/flags.rs"

    /// Run custom build command.
    cmd test
    {
        /// Filter string. Only tests which contain this string are run.
        optional filter: String
        /// Run ignored and non-ignored tests.
        optional --include_ignored
        /// Run only ignored tests.
        optional --ignored
        // List all tests
        optional --list
        /// No-op, ignored (lways runs in no-capture mode)        optional --nocapture
        optional --nocapture
        /// If set, filters are matched exactly rather than by substring.
        optional --exact

        /// A list of filters. Tests whoSe names contain parts of any of these
        /// filters are skipped.
        repeated --skip filter: String

        /// Configure formatting of output:
        /// pretty = Print verbose output;
        /// terse = Display one character per test;
        optional --format fmt: Format

    }
}
// generated start
// The following code is generated by `xflags` macro.
// Run `env UPDATE_XFLAGS=1 cargo build` to regenerate.
#[derive(Debug)]
pub struct Test {
    pub filter: Option<String>,

    pub include_ignored: bool,
    pub ignored: bool,
    pub list: bool,
    pub nocapture: bool,
    pub exact: bool,
    pub skip: Vec<String>,
    pub format: Option<Format>,
}

impl Test {
    #[allow(dead_code)]
    pub fn from_env_or_exit() -> Self {
        Self::from_env_or_exit_()
    }

    #[allow(dead_code)]
    pub fn from_env() -> xflags::Result<Self> {
        Self::from_env_()
    }

    #[allow(dead_code)]
    pub fn from_vec(args: Vec<std::ffi::OsString>) -> xflags::Result<Self> {
        Self::from_vec_(args)
    }
}
// generated end
