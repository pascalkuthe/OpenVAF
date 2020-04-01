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
