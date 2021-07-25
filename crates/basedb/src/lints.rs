use crate::FileId;
use data_structures::index_vec::define_index_type;

/// Lints can be set to different levls
/// This enum represents these levls
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LintLevel {
    /// Same as `Deny` but can not be overwritten by attributes later
    Forbid,
    /// Generates an error and will cause an error upon generating user diagnostics
    Deny,
    /// A warning
    Warn,
    /// Lints set to allow will not be displayed
    Allow,
}

// Implementation defered to HIR database
pub trait LintResolver {
    fn lint_overwrite(&self, lint: Lint, sctx: SyntaxCtx, root_file: FileId) -> Option<LintLevel>;
}

define_index_type! {
    pub struct SyntaxCtx = u32;

    DISPLAY_FORMAT = "synctx{}";
    DEBUG_FORMAT = "synctx{}";

    IMPL_RAW_CONVERSIONS = true;
}

define_index_type! {
    /// A lint is an Error that a user might plausibly want to ignore
    /// Most compiler error are **not lints** because they indicate that an
    /// assumption the compiler made no longer holds
    pub struct Lint = u16;

    DISPLAY_FORMAT = "lint{}";
    DEBUG_FORMAT = "lint{}";
    IMPL_RAW_CONVERSIONS = true;

    // Checks are done when literals are added
    DISABLE_MAX_INDEX_CHECK = ! cfg!(debug_assertions);
}
