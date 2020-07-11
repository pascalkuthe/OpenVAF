/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

//! This module is responsible for lowering an [`Ast`](crate::ast::Ast) to an [`Hir`](crate::hir::Hir)
//!
//! The main transformations in this module are
//!
//! * [**Name resolution**](name_resolution) -
//!   Names (such as variable references) are resolved to declarations.
//!   It is enforced that the resolved declarations are the right type of Hir node
//!   (no type checking that happens during [`hir_lowering`](crate::hir_lowering).
//!   Ids of the resolved declarations are then stored in the HIR inplace of identifiers in the AST
//!
//! * [**Branch resolution**](BranchResolver) -
//!   Unnamed branches ( accessed using for example using `<nature>(<net1>,<net2>)` )  are created as needed
//!   and tracked so that the same unnamed branch isn't created multiple times.
//!   Furthermore it is enforced that disciplines of the nets defining a branch are comparable
//!   and that branches are only accessed using the flow/potential Nature of those disciplines
//!
//! * **Context based information** -
//!     Some expressions and statements are not allowed in some places (for example in an analog/digital context).
//!     During the fold these (states)[ast_to_hir_fold::VerilogContext) are tracked and errors are generated when an illegal expressions/statements is used
//!
//!
//! The lowering process happens in a series of folds implemented in the [`ast_to_hir_fold`] module
//!
//!
pub use ast_to_hir_fold::Fold;
#[doc(inline)]
pub use branch_resolution::BranchResolver;

use crate::ast::Ast;
use crate::ast_lowering::ast_to_hir_fold::{DeclarationHandler, Global};
use crate::ast_lowering::error::Error;
use crate::diagnostic::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use crate::ir::hir::Hir;
use crate::SourceMap;
use std::sync::Arc;

#[macro_use]
pub mod name_resolution;
pub mod ast_to_hir_fold;
mod branch_resolution;
pub mod error;
pub mod lints;

//TODO input/output enforcement

impl Ast {
    /// Lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability
    pub fn lower_user_facing(
        self,
        sm: &Arc<SourceMap>,
        expansion_disclaimer: &'static str,
    ) -> UserResult<Hir> {
        self.lower_user_facing_with_printer(sm, expansion_disclaimer)
    }

    /// Lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability
    pub fn lower_user_facing_with_printer<P: DiagnosticSlicePrinter>(
        self,
        sm: &Arc<SourceMap>,
        expansion_disclaimer: &'static str,
    ) -> UserResult<Hir, P> {
        self.lower()
            .map_err(|err| err.user_facing(sm, expansion_disclaimer))
    }

    /// A Helper method to avoid code duplication until try blocks are stable
    pub fn lower(self) -> Result<Hir, MultiDiagnostic<Error>> {
        self.lower_with_decl_handler(&mut ())
    }

    pub fn lower_with_decl_handler(
        mut self,
        declaration_handler: &mut impl DeclarationHandler,
    ) -> Result<Hir, MultiDiagnostic<Error>> {
        Ok(Global::new(&mut self, declaration_handler)
            .fold()?
            .fold()?
            .fold()?)
    }
}
