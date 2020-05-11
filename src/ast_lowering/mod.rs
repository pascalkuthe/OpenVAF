/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
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
//!   (no type checking that happens during [hir_lowering](crate::hir_lowering).
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
use crate::ast_lowering::ast_to_hir_fold::Global;
use crate::ast_lowering::ast_to_hir_fold::VerilogContext;
use crate::ast_lowering::error::Error;
use crate::ir::hir::Hir;
use crate::ir::VariableId;
use crate::SourceMap;

#[cfg(test)]
mod test;
#[macro_use]
pub mod name_resolution;
pub mod ast_to_hir_fold;
mod branch_resolution;
pub mod error;

//TODO input/output enforcement

impl<'tag> Ast<'tag> {
    /// Lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability
    pub fn lower(mut self: Box<Self>) -> Result<Box<Hir<'tag>>, (Vec<Error<'tag>>, Box<Self>)> {
        self.try_fold_to_hir().map_err(|err| (err, self))
    }

    /// Lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability
    pub fn lower_with_var_decl_handle(
        mut self: Box<Self>,
        on_variable_delcaration: impl FnMut(VariableId<'tag>, &Fold, &VerilogContext, &BranchResolver),
    ) -> Result<Box<Hir<'tag>>, (Vec<Error<'tag>>, Box<Self>)> {
        self.try_fold_to_hir_with_var_decl_handle(on_variable_delcaration)
            .map_err(|err| (err, self))
    }

    /// Lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability and printing any errors or warnings that might occur
    pub fn lower_and_print_errors(
        self: Box<Self>,
        source_map: &SourceMap,
        translate_lines: bool,
    ) -> Option<Box<Hir<'tag>>> {
        self.lower()
            .map_err(|(errors, ast)| {
                errors
                    .into_iter()
                    .for_each(|err| err.print(source_map, &ast, translate_lines))
            })
            .ok()
    }

    /// Lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability and printing any errors or warnings that might occur
    pub fn lower_and_print_errors_with_var_decl_handle(
        self: Box<Self>,
        source_map: &SourceMap,
        translate_lines: bool,
        on_variable_delcaration: impl FnMut(VariableId<'tag>, &Fold, &VerilogContext, &BranchResolver),
    ) -> Option<Box<Hir<'tag>>> {
        self.lower_with_var_decl_handle(on_variable_delcaration)
            .map_err(|(errors, ast)| {
                errors
                    .into_iter()
                    .for_each(|err| err.print(source_map, &ast, translate_lines))
            })
            .ok()
    }

    /// A Helper method to avoid code duplication until try blocks are stable
    fn try_fold_to_hir(&mut self) -> Result<Box<Hir<'tag>>, Vec<Error<'tag>>> {
        Ok(Global::new(self, |_, _, _, _| ()).fold()?.fold()?.fold()?)
    }
    fn try_fold_to_hir_with_var_decl_handle(
        &mut self,
        on_variable_delcaration: impl FnMut(VariableId<'tag>, &Fold, &VerilogContext, &BranchResolver),
    ) -> Result<Box<Hir<'tag>>, Vec<Error<'tag>>> {
        Ok(Global::new(self, on_variable_delcaration)
            .fold()?
            .fold()?
            .fold()?)
    }
}
