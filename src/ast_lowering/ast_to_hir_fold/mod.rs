/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
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
use crate::{Ast, Hir};

#[doc(hidden)]
mod branches;
#[doc(hidden)]
mod expression;
#[doc(hidden)]
mod global;
#[doc(hidden)]
mod statements;

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

/// The Verilog AMS standard uses multiple different grammar rules to enfoce that constants/analog exprerssions only contain items valid in their context
/// VARF uses flags stored inside this struct instead during the AST to MIR folding process in this module
bitflags! {
    struct VerilogContext: u8{
        const constant = 0b00000001;
        const conditional = 0b00000010;
        const analog = 0b00000100;

    }
}

///The point of this entire Module. It lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability
pub fn fold<'tag>(
    mut ast: Box<Ast<'tag>>,
) -> Result<Box<Hir<'tag>>, (Vec<Error<'tag>>, Box<Ast<'tag>>)> {
    try_fold(&mut ast).map_err(|err| (err, ast))
}

/// A Helper method to avoid code duplication until try blocks are stable
fn try_fold<'tag>(ast: &mut Ast<'tag>) -> Result<Box<Hir<'tag>>, Vec<Error<'tag>>> {
    Ok(Global::new(ast).fold()?.fold()?.fold()?)
}
