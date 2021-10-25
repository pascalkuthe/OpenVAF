use std::{
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use crate::{BaseDB, FileId};
use data_structures::{index_vec::define_index_type, IndexMap};

/// Lints can be set to different levls
/// This enum represents these levls
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl Display for LintLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = match self {
            LintLevel::Forbid => "forbid",
            LintLevel::Deny => "deny",
            LintLevel::Warn => "warn",
            LintLevel::Allow => "allow",
        };
        write!(f, "{}", name)
    }
}

// Implementation defered to HIR database
pub trait LintResolver {
    fn lint_overwrite(&self, _lint: Lint, _sctx: SyntaxCtx, _root_file: FileId) -> Option<LintLevel>{None}
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

/// The data associated with a lint
/// Dont create and register this directly if you are writing a plugin
/// use [`declare_plugin_lint!`](crate::declare_plugin_lint) instead
#[derive(Copy, Clone, PartialEq, Debug, Eq)]
pub struct LintData {
    pub name: &'static str,
    pub documentation_id: usize,
    pub default_lvl: LintLevel,
}

/// Contains all builtin OpenVAF lints plus any plugin lints from the database
/// Can be used to map `str (lint name) -> Lint`, `Lint -> LintData`
/// and `str (lint name) -> LintData`
#[derive(Debug, Default, Eq, PartialEq)]
pub struct LintRegistry {
    lints: IndexMap<&'static str, LintData>,
}

impl LintRegistry {
    pub fn new(db: &dyn BaseDB) -> Arc<LintRegistry> {
        let lints = builtin::ALL.into_iter().chain(db.plugin_lints().iter().copied());
        Arc::new(Self { lints: lints.map(|lint| (lint.name, lint)).collect() })
    }

    pub fn lint_from_name(&self, name: &str) -> Option<Lint> {
        Some(self.lints.get_index_of(name)?.into())
    }

    pub fn lintdata_from_name(&self, name: &str) -> Option<LintData> {
        self.lints.get(name).copied()
    }

    pub fn lint_data(&self, lint: Lint) -> LintData {
        *self.lints.get_index(lint.into()).expect("Lint was not found in the registry!").1
    }
}

#[macro_export]
macro_rules! declare_lints {
    (
        $(pub const $name:ident = LintData{default_lvl: $lvl:ident, documentation_id: $doc_id:literal};)*
    ) => {
        $crate::declare_lints!(
            @OFFSET $crate::lints::builtin::ALL.len();
            $(pub const $name = LintData{default_lvl: $lvl, documentation_id: $doc_id};)*
        );
    };

    (
        @OFFSET $offset: expr;
        $(pub const $name:ident = LintData{default_lvl: $lvl:ident, documentation_id: $doc_id:literal};)*
    ) => {
        $crate::declare_lints!(@CONSTS 0, $($name),*);
        #[doc(hidden)]
        pub const ALL: [$crate::lints::LintData;[$($name),*].len()]
            = [$($crate::lints::LintData{documentation_id: $doc_id, default_lvl: LintLevel::$lvl, name: stringify!($name)}),*];
    };
    (
       @CONSTS $index: expr, $name:ident $(,$rem: ident)*
    ) => {
       #[allow(non_upper_case_globals)]
       pub const $name: $crate::lints::Lint = $crate::lints::Lint::from_raw_unchecked($index);

       $crate::declare_lints!(@CONSTS $index + 1 $(,$rem)*);
   };

   (
       @CONSTS $index: expr
   ) => {};
}

pub mod builtin {
    use super::*;

    declare_lints! {
        @OFFSET 0;
        pub const standard_nature_constants = LintData{default_lvl: Warn, documentation_id: 1};
        pub const constant_overflow = LintData{default_lvl: Deny, documentation_id: 2};
        pub const infinte_loop = LintData{default_lvl: Deny, documentation_id: 3};
        pub const macro_overwritten = LintData{default_lvl: Warn, documentation_id: 4};
        pub const attribute_overwritten = LintData{default_lvl: Warn, documentation_id:5};
        pub const rounding_derivative = LintData{default_lvl: Warn, documentation_id: 6};
        pub const noise_derivative = LintData{default_lvl: Warn, documentation_id: 7};
        pub const unkown_lint = LintData{default_lvl: Deny, documentation_id: 8};
        pub const lint_level_owerwrite = LintData{default_lvl: Warn, documentation_id: 9};
        pub const useless_function_call = LintData{default_lvl: Warn, documentation_id: 10};
        pub const non_standard_code = LintData{default_lvl: Warn, documentation_id: 11};
    }
}
