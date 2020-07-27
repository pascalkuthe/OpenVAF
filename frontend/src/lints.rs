/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

//! An extensible linting system used for high quality errors warning throughout OpenVAF
//! A lint is an Error or Warning that the user might plausibly want to ignore
//! As such each lint has a [lvl](crate::lints::LintLevel).
//!
//! When a lint is declared a default lvl for this lvl is set but this lvl can be [overwritten at runtime](crate::lints::Lint::overwrite_lvl_global)
//! This is possible because every lint has an display_id: A string constant that uniquely identifies it
//! A CLI interface may then accept something like `-W foo_lint` to change the lint lvl of `foo_lint` to `Warn`

use crate::diagnostic::{
    DiagnosticSlice, DiagnosticSlicePrinter, FooterItem, LibraryDiagnostic, Text,
    UserMultiDiagnostic, UserResult,
};
use crate::ir::ids::{
    BranchId, IntegerExpressionId, ModuleId, NatureId, NetId, ParameterId, RealExpressionId,
    StatementId, StringExpressionId, VariableId,
};
use crate::ir::mir::ExpressionId;
use crate::sourcemap::SourceMap;
use crate::HashMap;
use crate::GLOBALS;
use annotate_snippets::snippet::AnnotationType;
use beef::lean::Cow;
use index_vec::{define_index_type, index_vec, IndexSlice, IndexVec};
use linkme::distributed_slice;
use more_asserts::assert_le;
use once_cell::sync::OnceCell;
use open_vaf_macros::lints;
use std::error::Error;
use std::fmt::Display;
use std::sync::Arc;

// These lints contain builtin lints that are not parse of any algorithem but instead run on their own

pub mod unused;

#[distributed_slice]
pub static PLUGIN_LINTS: [&'static LintData] = [..];

/// Allows declaring additional lints that are not built into OpenVAF
///
/// This macro will register lint with display name `*crate_name*::$name`, no documentation lvl and the specified default lvl.
/// Furthmore a static variable `$name` will be declared that can be used to access the [`Lint`](crate::lints::Lint)
///
/// Note that [`linkme`](https://crates.io/crates/linkme) currently has to be a **directy dependency** (in your Cargo.toml) of every crate that calls this macro
///
/// # Arguments
///
/// * `$name` - The name of the lint
///
/// * `$default_lvl` - The lvl of the lint that should be used when no overwrite is presen
///
///
///
/// # Example
///
/// ```
/// # use open_vaf::declare_plugin_lint;
/// # use open_vaf::lints::LintLevel;
///
/// declare_plugin_lint!(foo,Warn);
/// declare_plugin_lint!(bar,Deny);
///
/// fn main(){
///     // doc tests are compiled as tough they are part of the main crate
///     // Therefore the crate name here is open-vaf
///     assert_eq!(foo.data().display_id,"open-vaf::foo");
///     assert_eq!(foo.lvl().0,LintLevel::Warn);
///     assert_eq!(bar.data().display_id,"open-vaf::bar");
///     assert_eq!(bar.lvl().0,LintLevel::Deny);
/// }
///
/// ```
///
#[macro_export]
macro_rules! declare_plugin_lint {
    ($name:ident,$default_lvl:ident) => {
        $crate::_macro_reexports::paste::item! {
            #[$crate::_macro_reexports::linkme::distributed_slice($crate::lints::PLUGIN_LINTS)]
            #[allow(non_upper_case_globals)]
            pub static [<$name _data>]: &$crate::lints::LintData = &$crate::lints::LintData {
                display_id: concat!(env!("CARGO_PKG_NAME"), "::", stringify!($name)),

                // Documentation doesnt cover plugins
                documentation_id: None,

                default_lvl: $crate::lints::LintLevel::$default_lvl,
            };
        }

        #[allow(non_upper_case_globals)]
        pub static $name: $crate::_macro_reexports::once_cell::sync::Lazy<$crate::lints::Lint> =
            $crate::_macro_reexports::once_cell::sync::Lazy::new(|| {
                println!(concat!(env!("CARGO_PKG_NAME"), "::", stringify!($name)));
                $crate::lints::Lint::from_name(concat!(
                    env!("CARGO_PKG_NAME"),
                    "::",
                    stringify!($name)
                ))
                .unwrap()
            });
    };
}

static LINT_REGISTRY: OnceCell<LintRegistry> = OnceCell::new();

#[derive(Debug, Default)]
struct LintRegistry {
    pub names: HashMap<&'static str, Lint>,
    pub lints: IndexVec<Lint, &'static LintData>,
}

fn with_lint_registry<T>(f: impl FnOnce(&LintRegistry) -> T) -> T {
    let registry = LINT_REGISTRY.get_or_init(LintRegistry::new);
    f(registry)
}

lints! {
    pub const macro_file_cutoff = LintData{default_lvl: Warn, documentation_id: None};
    pub const macro_overwritten = LintData{default_lvl: Warn, documentation_id: None};

    pub const attribute_overwritten = LintData{default_lvl: Warn, documentation_id: None};

    pub const ignored_display_task = LintData{default_lvl: Warn, documentation_id: None};

    pub const rounding_derivative = LintData{default_lvl: Warn, documentation_id: None};
    pub const noise_derivative = LintData{default_lvl: Warn, documentation_id: None};

    pub const unused_variables = LintData{default_lvl: Warn, documentation_id: None};
    pub const unused_parameters = LintData{default_lvl: Warn, documentation_id: None};
    pub const unused_branches = LintData{default_lvl: Warn, documentation_id: None};
    pub const unused_nets = LintData{default_lvl: Warn, documentation_id: None};
    pub const dead_code = LintData{default_lvl: Warn, documentation_id: None};


    pub const standard_nature_constants = LintData{default_lvl: Warn, documentation_id: Some("L001")};

    pub const constant_overflow = LintData{default_lvl: Deny, documentation_id: Some("L002")};
}

define_index_type! {
            /// A lint is an Error that a user might plausibly want to ignore
            /// Most compiler error are **not lints** because they indicate that an
            /// assumption the compiler made no longer holds
            pub struct Lint = u16;

            DISPLAY_FORMAT = "{}";

            DEBUG_FORMAT = "<Lint {}>";

            IMPL_RAW_CONVERSIONS = true;

            // Checks are done when literals are added
            DISABLE_MAX_INDEX_CHECK = ! cfg!(debug_assertions);
}

impl Lint {
    #[must_use]
    #[inline]
    pub fn data(self) -> LintData {
        with_lint_registry(|registry| *registry.lints[self])
    }

    #[must_use]
    #[inline]
    pub fn from_name(name: &str) -> Option<Self> {
        with_lint_registry(|registry| registry.names.get(name).copied())
    }

    #[must_use]
    #[inline]
    pub fn lvl(self) -> (LintLevel, bool) {
        let overwrite = with_linter(|linter| linter.overwrites.get(self).copied().flatten());
        match overwrite {
            Some(res) => (res, true),
            None => (self.data().default_lvl, false),
        }
    }

    pub fn overwrite_lvl_global(self, lvl: LintLevel) {
        self.set_global_overwrite(Some(lvl))
    }

    pub fn remove_global_overwrite(self) {
        self.set_global_overwrite(None)
    }

    pub fn set_global_overwrite(self, overwrite: Option<LintLevel>) {
        with_linter_mut(|linter| {
            linter.overwrites[self] = overwrite;
        })
    }
}

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

/// The location a late lint occured
/// This is will be used in the future to figure out the correct lint lvl based on attributes
#[derive(Debug, Clone, Copy)]
pub enum LintLocation {
    Statement(StatementId),
    RealExpression(RealExpressionId),
    IntegerExpression(IntegerExpressionId),
    StringExpression(StringExpressionId),
    Module(ModuleId),
    Nature(NatureId),
    Parameter(ParameterId),
    Variable(VariableId),
    Net(NetId),
    Branch(BranchId),
    Root,
}

impl From<RealExpressionId> for LintLocation {
    fn from(id: RealExpressionId) -> Self {
        Self::RealExpression(id)
    }
}

impl From<IntegerExpressionId> for LintLocation {
    fn from(id: IntegerExpressionId) -> Self {
        Self::IntegerExpression(id)
    }
}

impl From<StringExpressionId> for LintLocation {
    fn from(id: StringExpressionId) -> Self {
        Self::StringExpression(id)
    }
}

impl From<ExpressionId> for LintLocation {
    fn from(id: ExpressionId) -> Self {
        match id {
            ExpressionId::Real(expr) => Self::RealExpression(expr),
            ExpressionId::String(expr) => Self::StringExpression(expr),
            ExpressionId::Integer(expr) => Self::IntegerExpression(expr),
        }
    }
}

impl From<StatementId> for LintLocation {
    fn from(id: StatementId) -> Self {
        Self::Statement(id)
    }
}

impl From<ModuleId> for LintLocation {
    fn from(id: ModuleId) -> Self {
        Self::Module(id)
    }
}

impl From<NatureId> for LintLocation {
    fn from(id: NatureId) -> Self {
        Self::Nature(id)
    }
}

impl From<ParameterId> for LintLocation {
    fn from(id: ParameterId) -> Self {
        Self::Parameter(id)
    }
}

impl From<BranchId> for LintLocation {
    fn from(id: BranchId) -> Self {
        Self::Branch(id)
    }
}

impl From<NetId> for LintLocation {
    fn from(id: NetId) -> Self {
        Self::Net(id)
    }
}

impl From<VariableId> for LintLocation {
    fn from(id: VariableId) -> Self {
        Self::Variable(id)
    }
}

/// The data associated with a lint
/// Please dont create and register this directly if you are writing a plugin
/// use [`declare_plugin_lint!`](crate::declare_plugin_lint) instead
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct LintData {
    pub display_id: &'static str,
    pub documentation_id: Option<&'static str>,
    pub default_lvl: LintLevel,
}

/// Responsible for managing all lints generated in the current thread
pub struct Linter {
    early_lints: Vec<Box<dyn LintDiagnostic>>,
    late_lints: Vec<(Box<dyn LintDiagnostic>, LintLocation)>,
    overwrites: IndexVec<Lint, Option<LintLevel>>,
}

impl Linter {
    #[must_use]
    pub fn new() -> Self {
        Self {
            early_lints: Vec::with_capacity(64),
            late_lints: Vec::with_capacity(64),
            overwrites: index_vec![None; with_lint_registry(|registry|registry.lints.len())],
        }
    }
    /// Returns user diagnostics for lints that were emitted before type checking
    /// Their lint lvl is **always global** and can not be adjust using attributes
    ///
    /// # Returns
    ///
    /// * Err(diagnostics) if any lint has a lvl of `Deny` or `Forbid`
    ///
    /// * Ok(diagnostics) if all lints have a lvl of allow or warn
    ///
    pub fn early_user_diagnostics<Printer: DiagnosticSlicePrinter>(
        sm: &Arc<SourceMap>,
        expansion_disclaimer: &'static str,
    ) -> UserResult<UserMultiDiagnostic<Printer>, Printer> {
        let mut failure = false;

        let diagnostics = with_linter(|linter| {
            linter
                .early_lints
                .iter()
                .filter_map(|diagnostic| {
                    let user_diagnostic =
                        diagnostic.user_facing(sm.clone(), expansion_disclaimer)?;
                    failure |= user_diagnostic.annotation_type == AnnotationType::Error;
                    Some(user_diagnostic)
                })
                .collect()
        });

        if failure {
            Err(UserMultiDiagnostic(diagnostics))
        } else {
            Ok(UserMultiDiagnostic(diagnostics))
        }
    }

    /// Returns user diagnostics for lints that were emitted after type checking
    ///
    /// # Returns
    ///
    /// * Err(diagnostics) if any lint has a lvl of `Deny` or `Forbid`
    ///
    /// * Ok(diagnostics) if all lints have a lvl of allow or warn
    ///
    /// # Note
    ///
    /// This currently works the same as [`early_user_diagnostics`)(crate::lints::Linter::early_user_diagnostics)
    /// However this implementation is temporary
    /// In the future this will be an Mir pass that
    /// will figure out which lint lvl applies based upon attributes similar to rustc

    pub fn late_user_diagnostics<Printer: DiagnosticSlicePrinter>(
        sm: &Arc<SourceMap>,
        expansion_disclaimer: &'static str,
    ) -> UserResult<UserMultiDiagnostic<Printer>, Printer> {
        let mut failure = false;
        let diagnostics = with_linter(|linter| {
            linter
                .late_lints
                .iter()
                .filter_map(|(diagnostic, _)| {
                    let user_diagnostic =
                        diagnostic.user_facing(sm.clone(), expansion_disclaimer)?;
                    failure |= user_diagnostic.annotation_type == AnnotationType::Error;
                    Some(user_diagnostic)
                })
                .collect()
        });

        if failure {
            Err(UserMultiDiagnostic(diagnostics))
        } else {
            Ok(UserMultiDiagnostic(diagnostics))
        }
    }

    #[inline]
    pub fn dispatch_early(diagnostic: Box<dyn LintDiagnostic>) {
        with_linter_mut(|linter| {
            linter.early_lints.push(diagnostic);
        })
    }

    #[inline]
    pub fn dispatch_late(diagnostic: Box<dyn LintDiagnostic>, location: LintLocation) {
        with_linter_mut(|linter| {
            linter.late_lints.push((diagnostic, location));
        })
    }
}

impl Default for Linter {
    fn default() -> Self {
        Self::new()
    }
}

pub trait LintDiagnostic: Display + Error {
    fn lint(&self) -> Lint;

    #[inline]
    fn title(&self) -> Text {
        Cow::owned(format!("{}", self))
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice>;

    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn footer(&self) -> Vec<FooterItem> {
        Vec::new()
    }
}

impl<T: LintDiagnostic + ?Sized> LibraryDiagnostic for T {
    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn id(&self) -> Option<&'static str> {
        self.lint().data().documentation_id
    }

    //the content of this expression is literally equal in instruction size so this should always be inlined
    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn title(&self) -> Text {
        LintDiagnostic::title(self)
    }

    #[inline]
    fn annotation_type(&self) -> Option<AnnotationType> {
        match self.lint().lvl().0 {
            LintLevel::Forbid | LintLevel::Deny => Some(AnnotationType::Error),
            LintLevel::Warn => Some(AnnotationType::Warning),
            LintLevel::Allow => None,
        }
    }

    #[inline]
    fn slices(&self) -> Vec<DiagnosticSlice> {
        if let Some(annotation_type) = self.annotation_type() {
            LintDiagnostic::slices(self, annotation_type)
        } else {
            Vec::new()
        }
    }

    #[inline]
    fn footer(&self) -> Vec<FooterItem> {
        let mut footer = LintDiagnostic::footer(self);
        let display_id = self.lint().data().display_id;
        let (lvl, overwritten) = self.lint().lvl();

        if overwritten {
            footer.push(FooterItem {
                id: None,
                label: Text::owned(format!(
                    "Lint '{}' was overwritten to {:?}",
                    display_id, lvl
                )),
                annotation_type: AnnotationType::Note,
            });
        } else {
            footer.push(FooterItem {
                id: None,
                label: Text::owned(format!("'{}' is set to {:?} by default", display_id, lvl)),
                annotation_type: AnnotationType::Note,
            });
        }

        footer
    }
}

fn with_linter<T>(f: impl FnOnce(&Linter) -> T) -> T {
    GLOBALS.with(|globals| f(&globals.linter.read()))
}

fn with_linter_mut<T>(f: impl FnOnce(&mut Linter) -> T) -> T {
    GLOBALS.with(|globals| f(&mut globals.linter.write()))
}
