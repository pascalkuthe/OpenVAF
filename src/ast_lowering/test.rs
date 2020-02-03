use std::path::Path;

use bumpalo::Bump;

use crate::ast_lowering::resolve_and_print;
use crate::parser::{insert_electrical_natures_and_disciplines, parse_and_print_errors};

#[test]
pub fn linear() -> Result<(), ()> {
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    let (source_map, res) = parse_and_print_errors(
        Path::new("tests/linear.va"),
        &source_map_allocator,
        &mut ast,
    );
    res?;
    insert_electrical_natures_and_disciplines(&mut ast);
    resolve_and_print(ast, source_map)?;
    Ok(())
}
