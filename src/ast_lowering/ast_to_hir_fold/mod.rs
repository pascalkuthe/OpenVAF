/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
#[doc(inline)]
pub(super) use branches::Branches;
#[doc(inline)]
pub(super) use expression::ExpressionFolder;
#[doc(inline)]
pub(super) use global::Global;
#[doc(inline)]
pub(super) use statements::Statements;

use crate::ast_lowering::error::Error;
use crate::ast_lowering::name_resolution::Resolver;
use crate::symbol_table::SymbolDeclaration;
use crate::{Ast, Hir};

#[doc(hidden)]
mod branches;
#[doc(hidden)]
mod expression;
#[doc(hidden)]
mod global;
#[doc(hidden)]
mod statements;

pub trait DeclarationHandler<'tag> {
    fn handle_declaration(
        &mut self,
        fold: &mut Fold<'tag, '_>,
        declaration: SymbolDeclaration<'tag>,
    );
}

impl<'tag> DeclarationHandler<'tag> for () {
    fn handle_declaration(
        &mut self,
        fold: &mut Fold<'tag, '_>,
        declaration: SymbolDeclaration<'tag>,
    ) {
    }
}

/// A struct that contains data and functionality all ast to hir folds share
/// It is used for abstracting over functionality/data for the `resolve!`/`resolve_hierarchical!` macros and [`BranchResolver`](crate::ast_lowering::branch_resolution::BranchResolver)
pub struct Fold<'tag, 'lt> {
    pub resolver: Resolver<'tag, 'lt>,
    pub errors: Vec<Error<'tag>>,
    pub hir: Box<Hir<'tag>>,
    pub ast: &'lt Ast<'tag>,
}

impl<'tag, 'lt> Fold<'tag, 'lt> {
    pub fn error(&mut self, error: Error<'tag>) {
        self.errors.push(error)
    }
}

bitflags! {
    /// The Verilog AMS standard uses multiple different grammar rules to enfoce that constants/analog exprerssions only contain items valid in their context
    /// VARF uses flags stored inside this struct instead during the AST to MIR folding process in this module
    pub struct VerilogContext: u8{
        const constant = 0b0000_0001;
        const conditional = 0b0000_0010;
        const analog = 0b0000_0100;

    }
}
