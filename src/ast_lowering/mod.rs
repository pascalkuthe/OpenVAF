/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::Ast;
use crate::ast_lowering::ast_to_hir_fold::fold;
use crate::ir::hir::Hir;
use crate::{ast, SourceMap};

#[cfg(test)]
mod test;
#[macro_use]
pub mod name_resolution;
pub mod ast_to_hir_fold;
pub mod branch_resolution;

//TODO input/output enforcement
//TODO type checking
pub mod error;
bitflags! {
    struct VerilogContext: u8{
        const constant = 0b00000001;
        const conditional = 0b00000010;
        const analog = 0b00000100;

    }
}

pub fn fold_ast_to_hir_and_print_errors<'tag>(
    mut ast: Box<Ast<'tag>>,
    source_map: &SourceMap,
    translate_lines: bool,
) -> Result<Box<Hir<'tag>>, ()> {
    fold(ast).map_err(|(errors, ast)| {
        errors
            .into_iter()
            .for_each(|err| err.print(source_map, &ast, translate_lines))
    })
}
