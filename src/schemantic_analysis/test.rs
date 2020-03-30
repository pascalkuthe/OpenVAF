use std::path::Path;

use bumpalo::Bump;

use crate::compact_arena::SafeRange;
use crate::ir::ast::NetType;
use crate::ir::{ExpressionId, ModuleId};
use crate::name_resolution::resolve_and_print;
use crate::parser::{insert_electrical_natures_and_disciplines, parse_and_print_errors};
use crate::schemantic_analysis::error::{Error, Type};
use crate::schemantic_analysis::run_semantic;
use crate::util::SafeRangeCreation;

#[test]
pub fn schemantic() -> Result<(), ()> {
    // loop {
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    let source_map = parse_and_print_errors(
        Path::new("tests/diode.va"),
        &source_map_allocator,
        &mut ast,
        true,
    )?;
    insert_electrical_natures_and_disciplines(&mut ast);
    let hir = resolve_and_print(ast, source_map, true)?;
    let mir = run_semantic(hir, source_map, true)?;
    Ok(())
}
/*#[test]
pub fn bjt() -> Result<(), ()> {
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    let (source_map, res) =
        parse_and_print_errors(Path::new("tests/bjt.va"), &source_map_allocator, &mut ast);
    res?;
    insert_electrical_natures_and_disciplines(&mut ast);
    let hir = resolve_and_print(ast, source_map)?;
    Ok(())
}*/

#[test]
pub fn linear() -> Result<(), ()> {
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    let source_map = parse_and_print_errors(
        Path::new("tests/linear.va"),
        &source_map_allocator,
        &mut ast,
        true,
    )?;
    insert_electrical_natures_and_disciplines(&mut ast);

    let hir = resolve_and_print(ast, source_map, true)?;
    let module: SafeRange<ModuleId> = hir.full_range();
    let module = &hir[module][0].contents;
    let mut ports = hir[module.port_list].iter();
    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    let net = hir[port.net].contents;
    assert_eq!(net.name.as_str(), "A");

    assert_eq!(net.signed, false);
    assert_eq!(hir[net.discipline].contents.name.as_str(), "electrical");
    assert_eq!(net.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    let net = hir[port.net].contents;
    assert_eq!(net.name.as_str(), "B");

    assert_eq!(net.signed, false);
    assert_eq!(hir[net.discipline].contents.name.as_str(), "electrical");
    assert_eq!(net.net_type, NetType::UNDECLARED);
    let mir = run_semantic(hir, source_map, true)?;
    Ok(())
}
