/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use bitflags::bitflags;

#[doc(inline)]
pub(super) use branches::Branches;
#[doc(inline)]
pub(super) use expression::ExpressionFolder;
#[doc(inline)]
pub(super) use global::Global;
#[doc(inline)]
pub(super) use statements::Statements;

use crate::ast::Ast;
use crate::ast_lowering::error::Error;
use crate::ast_lowering::name_resolution::Resolver;
use crate::diagnostic::MultiDiagnostic;
use crate::hir::Hir;
use crate::symbol_table::SymbolDeclaration;

#[doc(hidden)]
mod branches;
#[doc(hidden)]
mod expression;
#[doc(hidden)]
mod global;
#[doc(hidden)]
mod statements;

pub trait DeclarationHandler {
    fn handle_declaration(&mut self, fold: &mut Fold<'_>, declaration: SymbolDeclaration);
}

impl DeclarationHandler for () {
    fn handle_declaration(&mut self, _: &mut Fold<'_>, _: SymbolDeclaration) {}
}

/// A struct that contains data and functionality all ast to hir folds share
/// It is used for abstracting over functionality/data for the `resolve!`/`resolve_hierarchical!` macros and [`BranchResolver`](crate::ast_lowering::branch_resolution::BranchResolver)
pub struct Fold<'lt> {
    pub resolver: Resolver<'lt>,
    pub errors: MultiDiagnostic<Error>,
    pub hir: Hir,
    pub ast: &'lt Ast,
}

impl<'lt> Fold<'lt> {
    pub fn error(&mut self, error: Error) {
        self.errors.add(error)
    }
}

bitflags! {
    /// The Verilog AMS standard uses multiple different grammar rules to enfoce that constants/analog exprerssions only contain items valid in their context
    /// frontend uses flags stored inside this struct instead during the AST to MIR folding process in this module
    pub struct VerilogContext: u8{
        const CONSTANT = 0b0000_0001;
        const CONDITIONAL = 0b0000_0010;
        const FUNCTION = 0b0000_1000;

    }
}
