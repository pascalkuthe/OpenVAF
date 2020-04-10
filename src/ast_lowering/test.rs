/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::path::Path;

use bumpalo::Bump;

use crate::ast_lowering::fold_ast_to_hir_and_print_errors;
use crate::compact_arena::SafeRange;
use crate::ir::ast::NetType;
use crate::ir::ModuleId;
use crate::ir::SafeRangeCreation;
use crate::parser::parse_and_print_errors;

#[test]
pub fn diode() -> Result<(), ()> {
    fern::Dispatch::new()
        .format(|out, message, _record| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    let source_map = parse_and_print_errors(
        Path::new("tests/diode.va"),
        &source_map_allocator,
        &mut ast,
        true,
    )?;
    let hir = fold_ast_to_hir_and_print_errors(ast, source_map, true)?;
    Ok(())
}

#[test]
pub fn linear() -> Result<(), ()> {
    fern::Dispatch::new()
        .format(|out, message, record| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    let source_map = parse_and_print_errors(
        Path::new("tests/linear.va"),
        &source_map_allocator,
        &mut ast,
        true,
    )?;

    let hir = fold_ast_to_hir_and_print_errors(ast, source_map, true)?;
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

    Ok(())
}
