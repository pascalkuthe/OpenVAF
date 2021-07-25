/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

//! An extensible linting system used for high quality errors warning throughout OpenVAF
//! A lint is an Error or Warning that the user might plausibly want to ignore
//! As such each lint has a [lvl](crate::lints::LintLevel).
//!
//! When a lint is declared a default lvl for this lvl is set but this lvl can be
//! [overwritten at runtime](crate::lints::Lint::overwrite_lvl_global)
//! This is possible because every lint has an display_id: A string constant that uniquely identifies it
//! A CLI interface may then accept something like `-W foo_lint` to change the lint lvl of `foo_lint` to `Warn`

use crate::{Diagnostic, DiagnosticSlice, FooterItem, Text};
use annotate_snippets::snippet::AnnotationType;
use linkme::distributed_slice;
use once_cell::sync::OnceCell;
pub use basedb::lints::{Lint, LintLevel, SyntaxCtx};
use basedb::{BaseDB, FileId};
use data_structures::index_vec::{index_vec, IndexBox, IndexSlice, IndexVec};
use data_structures::HashMap;
use macros::lints;
use std::fmt::Display;

// These lints contain builtin lints that are not parse of any algorithem but instead run on their own

//pub mod unused;

#[distributed_slice]
pub static PLUGIN_LINTS: [&'static LintData] = [..];

/// Allows declaring additional lints that are not built into OpenVAF
///
/// This macro will register lint with display name `*crate_name*::$name`, no documentation lvl and the specified default lvl.
/// Furthmore a static variable `$name` will be declared that can be used to access the [`Lint`](crate::lints::Lint)
///     
/// Note that [`linkme`](https://crates.io/crates/linkme) currently has to be a **directy dependency**
/// (in your Cargo.toml.old) of every crate that calls this macro
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
/// # use diagnostics::declare_plugin_lint;
/// # use diagnostics::lints::LintLevel;
/// # use session::session;
///
/// declare_plugin_lint!(foo,Warn);
/// declare_plugin_lint!(bar,Deny);
///
/// fn main(){
/// # session(||{
///     // doc tests are compiled as tough they are part of the main crate
///     // Therefore the crate name here is open-vaf
///     assert_eq!(foo.data().display_id,"diagnostics::foo");
///     assert_eq!(foo.lvl().0,LintLevel::Warn);
///     assert_eq!(bar.data().display_id,"diagnostics::bar");
///     assert_eq!(bar.lvl().0,LintLevel::Deny);
/// # });
/// }
///
/// ```
///
#[macro_export]
macro_rules! declare_plugin_lint {
    ($name:ident,$default_lvl:ident) => {
        $crate::_macro_reexports::paste::item! {
            #[::linkme::distributed_slice($crate::lints::PLUGIN_LINTS)]
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

pub fn empty_global_lint_overwrites() -> IndexBox<Lint, [Option<LintLevel>]> {
    with_lint_registry(|registry| index_vec![None;registry.lints.len()]).into_boxed_slice()
}

lints! {
    pub const macro_file_cutoff = LintData{default_lvl: Warn, documentation_id: None};
    pub const macro_overwritten = LintData{default_lvl: Warn, documentation_id: None};

    pub const attribute_overwritten = LintData{default_lvl: Warn, documentation_id: None};
    pub const event_ignored = LintData{default_lvl: Warn, documentation_id: None}; // TODO documentation

    pub const ignored_display_task = LintData{default_lvl: Warn, documentation_id: None};
    pub const empty_builtin_attribute = LintData{default_lvl: Warn, documentation_id: None};

    pub const rounding_derivative = LintData{default_lvl: Warn, documentation_id: None};
    pub const noise_derivative = LintData{default_lvl: Warn, documentation_id: None};

    pub const unused_variables = LintData{default_lvl: Warn, documentation_id: None};
    pub const unused_parameters = LintData{default_lvl: Warn, documentation_id: None};
    pub const unused_branches = LintData{default_lvl: Warn, documentation_id: None};
    pub const unused_nets = LintData{default_lvl: Warn, documentation_id: None};
    pub const dead_code = LintData{default_lvl: Warn, documentation_id: None};


    pub const standard_nature_constants = LintData{default_lvl: Warn, documentation_id: Some("L001")};

    pub const constant_overflow = LintData{default_lvl: Deny, documentation_id: Some("L002")};
    pub const infinte_loop = LintData{default_lvl: Deny, documentation_id: None};

    pub const unkown_lint = LintData{default_lvl: Deny, documentation_id: None};
    pub const lint_level_owerwrite = LintData{default_lvl: Warn, documentation_id: None};
    pub const useless_function_call = LintData{default_lvl: Warn, documentation_id: None};


    pub const non_standard_code = LintData{default_lvl: Warn, documentation_id: None};


}

enum LintLvlSrc {
    Default,
    Global,
    Local,
}

#[must_use]
#[inline]
pub fn lint_data(lint: Lint) -> LintData {
    with_lint_registry(|registry| *registry.lints[lint])
}

#[must_use]
#[inline]
pub fn lint_from_name(name: &str) -> Option<Lint> {
    with_lint_registry(|registry| registry.names.get(name).copied())
}

#[must_use]
fn lint_lvl(
    lint: Lint,
    db: &dyn BaseDB,
    sctx: Option<SyntaxCtx>,
    root_file: FileId,
) -> (LintLevel, LintLvlSrc) {
    if let Some(lvl) = sctx.and_then(|sctx| db.lint_overwrite(lint, sctx, root_file)) {
        (lvl, LintLvlSrc::Local)
    } else if let Some(lvl) = db.global_lint_overwrites(root_file)[lint] {
        (lvl, LintLvlSrc::Global)
    } else {
        with_lint_registry(|regisry| (regisry.lints[lint].default_lvl, LintLvlSrc::Default))
    }
}

/// The data associated with a lint
/// Dont create and register this directly if you are writing a plugin
/// use [`declare_plugin_lint!`](crate::declare_plugin_lint) instead
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct LintData {
    pub display_id: &'static str,
    pub documentation_id: Option<&'static str>,
    pub default_lvl: LintLevel,
}

// TODO integrate lints into SALSA instead
// /// Responsible for managing all lints generated in the current thread
// pub struct Linter {
//     early_lints: Vec<Box<dyn LintDiagnostic>>,
//     late_lints: Vec<(Box<dyn LintDiagnostic>, SyntaxCtx)>,
//     overwrites: IndexVec<Lint, Option<LintLevel>>,
// }
//
// impl Linter {
//     #[must_use]
//     pub fn new() -> Self {
//         Self {
//             early_lints: Vec::with_capacity(64),
//             late_lints: Vec::with_capacity(64),
//             overwrites: index_vec![None; with_lint_registry(|registry|registry.lints.len())],
//         }
//     }
//     /// Returns user diagnostics for lints that were emitted before type checking
//     /// Their lint lvl is **always global** and can not be adjust using attributes
//     ///
//     /// # Returns
//     ///
//     /// * Err(diagnostics) if any lint has a lvl of `Deny` or `Forbid`
//     ///
//     /// * Ok(diagnostics) if all lints have a lvl of allow or warn
//     ///
//     pub fn early_user_diagnostics<Printer: DiagnosticSlicePrinter>(
//     ) -> UserResult<UserMultiDiagnostic<Printer>, Printer> {
//         let mut failure = false;
//
//         let diagnostics = with_linter(|linter| {
//             linter
//                 .early_lints
//                 .iter()
//                 .filter_map(|diagnostic| {
//                     let user_diagnostic = diagnostic.user_facing()?;
//                     failure |= user_diagnostic.annotation_type == AnnotationType::Error;
//                     Some(user_diagnostic)
//                 })
//                 .collect()
//         });
//
//         if failure {
//             Err(UserMultiDiagnostic(diagnostics))
//         } else {
//             Ok(UserMultiDiagnostic(diagnostics))
//         }
//     }
//
//     /// Returns user diagnostics for lints that were emitted after type checking
//     ///
//     /// # Returns
//     ///
//     /// * Err(diagnostics) if any lint has a lvl of `Deny` or `Forbid`
//     ///
//     /// * Ok(diagnostics) if all lints have a lvl of allow or warn
//     ///
//     /// # Note
//     ///
//     /// This currently works the same as [`early_user_diagnostics`)(crate::lints::Linter::early_user_diagnostics)
//     /// However this implementation is temporary
//     /// In the future this will be an Mir pass that
//     /// will figure out which lint lvl applies based upon attributes similar to rustc
//
//     pub fn late_user_diagnostics<Printer: DiagnosticSlicePrinter>(
//     ) -> UserResult<UserMultiDiagnostic<Printer>, Printer> {
//         let mut failure = false;
//         let diagnostics = with_linter(|linter| {
//             linter
//                 .late_lints
//                 .iter()
//                 .filter_map(|(diagnostic, _)| {
//                     let user_diagnostic = diagnostic.user_facing()?;
//                     failure |= user_diagnostic.annotation_type == AnnotationType::Error;
//                     Some(user_diagnostic)
//                 })
//                 .collect()
//         });
//
//         if failure {
//             Err(UserMultiDiagnostic(diagnostics))
//         } else {
//             Ok(UserMultiDiagnostic(diagnostics))
//         }
//     }
//
//     #[inline]
//     pub fn dispatch_early(diagnostic: Box<dyn LintDiagnostic>) {
//         with_linter_mut(|linter| {
//             linter.early_lints.push(diagnostic);
//         })
//     }
//
//     #[inline]
//     pub fn dispatch_late(diagnostic: Box<dyn LintDiagnostic>, location: SyntaxCtx) {
//         with_linter_mut(|linter| {
//             linter.late_lints.push((diagnostic, location));
//         })
//     }
// }
//
// impl Default for Linter {
//     fn default() -> Self {
//         Self::new()
//     }
// }

pub trait LintDiagnostic: Display + 'static {
    fn lint(&self) -> Lint;

    fn sctx(&self) -> Option<SyntaxCtx>;

    #[inline]
    fn title(&self) -> Text {
        Text::owned(format!("{}", self))
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice>;

    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn footer(&self) -> Vec<FooterItem> {
        Vec::new()
    }
}

impl<T: LintDiagnostic> Diagnostic for T {
    #[inline]
    fn id(&self) -> Option<&'static str> {
        lint_data(self.lint()).documentation_id
    }

    //the content of this expression is literally equal in instruction size so this should always be inlined
    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn title(&self) -> Text {
        LintDiagnostic::title(self)
    }

    #[inline]
    fn annotation_type(&self, db: &dyn BaseDB, root_file: FileId) -> Option<AnnotationType> {
        match lint_lvl(self.lint(), db, self.sctx(), root_file).0 {
            LintLevel::Forbid | LintLevel::Deny => Some(AnnotationType::Error),
            LintLevel::Warn => Some(AnnotationType::Warning),
            LintLevel::Allow => None,
        }
    }

    #[inline]
    fn slices(&self, db: &dyn BaseDB, root_file: FileId) -> Vec<DiagnosticSlice> {
        self.annotation_type(db, root_file)
            .map_or(vec![], |annotation_type| LintDiagnostic::slices(self, annotation_type))
    }

    #[inline]
    fn footer(&self, db: &dyn BaseDB, root_file: FileId) -> Vec<FooterItem> {
        let mut footer = LintDiagnostic::footer(self);
        let display_id = lint_data(self.lint()).display_id;
        let (lvl, kind) = lint_lvl(self.lint(), db, self.sctx(), root_file);

        let msg = match kind {
            LintLvlSrc::Default => format!("'{}' is set to {:?} by default", display_id, lvl),
            LintLvlSrc::Global => format!("Lint '{}' was globally set to {:?}", display_id, lvl),
            LintLvlSrc::Local => format!("'{}' was to {:?} by an attribute", display_id, lvl),
        };

        footer.push(FooterItem {
            id: None,
            label: Text::owned(msg),
            annotation_type: AnnotationType::Note,
        });

        footer
    }
}
