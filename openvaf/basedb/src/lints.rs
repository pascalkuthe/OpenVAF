use std::fmt::{self, Display, Formatter};
use std::sync::Arc;

use indexmap::IndexMap;
use stdx::{impl_debug_display, impl_idx_from};
use vfs::FileId;

use crate::{BaseDB, ErasedAstId};

/// Lints can be set to different levls
/// This enum represents these levls
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LintLevel {
    /// Lints set to allow will not be displayed
    Allow,
    /// A warning
    Warn,
    /// Generates an error and will cause an error upon generating user diagnostics
    Deny,
    // /// Same as `Deny` but can not be overwritten by attributes later
    // Forbid,
}

impl LintLevel {
    pub fn attr(self) -> &'static str {
        match self {
            LintLevel::Deny => "openvaf_allow",
            LintLevel::Warn => "openvaf_warn",
            LintLevel::Allow => "openvaf_deny",
        }
    }
}

impl Display for LintLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = match self {
            // LintLevel::Forbid => "forbid",
            LintLevel::Deny => "deny",
            LintLevel::Warn => "warn",
            LintLevel::Allow => "allow",
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct LintSrc {
    pub overwrite: Option<LintLevel>,
    pub ast: Option<ErasedAstId>,
}

impl LintSrc {
    pub fn item(ast: ErasedAstId) -> LintSrc {
        Self { overwrite: None, ast: Some(ast) }
    }

    pub fn lvl(&self, lint: Lint, root_file: FileId, db: &dyn BaseDB) -> (LintLevel, bool) {
        match self.overwrite {
            Some(lvl) => (lvl, false),
            None => db.lint_lvl(lint, root_file, self.ast),
        }
    }
}

impl From<ErasedAstId> for LintSrc {
    fn from(src: ErasedAstId) -> Self {
        LintSrc { overwrite: None, ast: Some(src) }
    }
}

impl LintSrc {
    pub const GLOBAL: LintSrc = LintSrc { overwrite: None, ast: None };
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Lint(u16);

impl Lint {
    /// You should not use this function directly it is only public for use in the exported macros
    #[doc(hidden)]
    pub const fn _from_raw(raw: u16) -> Lint {
        Lint(raw)
    }
}

impl_idx_from!(Lint(u16));
impl_debug_display!(c@Lint => "lint{}",c.0);

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
    lints: IndexMap<&'static str, LintData, ahash::RandomState>,
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
       pub const $name: $crate::lints::Lint = $crate::lints::Lint::_from_raw($index);

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
        // pub const standard_nature_constants = LintData{default_lvl: Warn, documentation_id: 1};
        // pub const constant_overflow = LintData{default_lvl: Deny, documentation_id: 2};
        // pub const infinite_loop = LintData{default_lvl: Deny, documentation_id: 3};
        pub const macro_overwritten = LintData{default_lvl: Warn, documentation_id: 4};
        // pub const attribute_overwritten = LintData{default_lvl: Warn, documentation_id:5};
        // pub const rounding_derivative = LintData{default_lvl: Warn, documentation_id: 6};
        // pub const noise_derivative = LintData{default_lvl: Warn, documentation_id: 7};
        pub const lint_not_found = LintData{default_lvl: Deny, documentation_id: 8};
        pub const lint_level_overwrite = LintData{default_lvl: Warn, documentation_id: 9};
        // pub const useless_function_call = LintData{default_lvl: Warn, documentation_id: 10};
        pub const non_standard_code = LintData{default_lvl: Warn, documentation_id: 11};
        pub const vams_keyword_compat = LintData{default_lvl: Warn, documentation_id: 12};
        pub const non_standard_analog_operator = LintData{default_lvl: Deny, documentation_id: 13};
        pub const const_simparam = LintData{default_lvl: Allow, documentation_id: 14};
        pub const variant_const_simparam = LintData{default_lvl: Warn, documentation_id: 15};
        pub const port_without_direction = LintData{default_lvl: Deny, documentation_id: 16};
    }
}
