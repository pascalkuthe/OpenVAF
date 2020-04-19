/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::path::Path;

use bumpalo::Bump;

use crate::ast::NetType;
use crate::ast::NetType::{UNDECLARED, WIRE};
use crate::ast::VariableType::{INTEGER, REAL, REALTIME, TIME};
use crate::ast::{Branch, VariableType};
use crate::compact_arena::SafeRange;
use crate::ir::ModuleId;
use crate::ir::SafeRangeCreation;
use crate::symbol::keywords::EMPTY_SYMBOL;
use crate::symbol::Symbol;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::Ast;

#[test]
pub fn module() -> Result<(), ()> {
    fern::Dispatch::new()
        .format(|out, message, record| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    ast.parse_from_and_print_errors(
        Path::new("tests/parseunits/module.va"),
        &source_map_allocator,
        true,
    )
    .ok_or(())?;

    let range: SafeRange<ModuleId> = ast.full_range();
    let modules = &ast[range];
    let first_module = &modules[0].contents;
    assert_eq!(first_module.name.as_str(), "test1");
    let second_module = &modules[1].contents;
    assert_eq!(second_module.name.as_str(), "test2");
    let mut ports = ast[second_module.port_list].iter();

    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "a");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    assert_eq!(port.signed, false);
    assert_eq!(port.discipline.name, EMPTY_SYMBOL);
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, true);
    assert_eq!(port.discipline.name, EMPTY_SYMBOL);
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "c");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(port.discipline.name, EMPTY_SYMBOL);
    assert_eq!(port.net_type, NetType::WIRE);

    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "d");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(port.discipline.as_str(), "electrical");
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "e");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    assert_eq!(port.signed, false);
    assert_eq!(port.discipline.as_str(), "electrical");
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "f");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    assert_eq!(port.signed, false);
    assert_eq!(port.discipline.as_str(), "electrical");
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "g");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, true);
    assert_eq!(port.discipline.as_str(), "electrical");
    assert_eq!(port.net_type, NetType::WIRE);

    let third_module = &modules[2].contents;
    assert_eq!(third_module.name.as_str(), "test3");
    let mut ports = ast[third_module.port_list].iter();
    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "a");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    assert_eq!(port.signed, false);
    assert_eq!(port.discipline.name, EMPTY_SYMBOL);
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(port.discipline.as_str(), "electrical");
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "c");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(port.discipline.as_str(), "electrical");
    assert_eq!(port.net_type, NetType::TRI);
    Ok(())
}

#[test]
pub fn branch() -> Result<(), ()> {
    fern::Dispatch::new()
        .format(|out, message, record| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    ast.parse_from_and_print_errors(
        Path::new("tests/parseunits/branch.va"),
        &source_map_allocator,
        true,
    )
    .ok_or(())?;

    let range: SafeRange<ModuleId> = ast.full_range();
    let module = &ast[range][0].contents;
    assert_eq!(module.name.as_str(), "test");
    let ports = &ast[module.port_list];

    let port = ports[0].contents;
    assert_eq!(port.name.as_str(), "a");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);

    let port = ports[1].contents;
    assert_eq!(port.name.as_str(), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);
    let symbol_table = &module.symbol_table;
    assert_branch_decl(symbol_table, &ast, "ab1", "a", "b");
    assert_branch_decl(symbol_table, &ast, "ab2", "a", "b");
    assert_port_branch_decl(symbol_table, &ast, "pa", "a");
    assert_port_branch_decl(symbol_table, &ast, "pb", "b");
    Ok(())
}

fn assert_branch_decl<'ast>(
    symbol_table: &SymbolTable<'ast>,
    ast: &Ast<'ast>,
    name: &str,
    net1_name: &str,
    net2_name: &str,
) {
    if let Some(SymbolDeclaration::Branch(branchid)) = symbol_table.get(&Symbol::intern(name)) {
        let branch = &ast[*branchid].contents;
        if let Branch::Nets(ref net1, ref net2) = branch.branch {
            assert_eq!(net1.names[0].as_str(), net1_name);
            assert_eq!(net2.names[0].as_str(), net2_name);
        } else {
            panic!("This should be a branch between two nets")
        }
    } else {
        panic!("Branch {} not found", name);
    }
}
fn assert_port_branch_decl<'ast>(
    symbol_table: &SymbolTable<'ast>,
    ast: &Ast<'ast>,
    name: &str,
    port_name: &str,
) {
    if let Some(SymbolDeclaration::Branch(branchid)) = symbol_table.get(&Symbol::intern(name)) {
        let branch = &ast[*branchid].contents;
        if let Branch::Port(ref port) = branch.branch {
            assert_eq!(port.names[0].as_str(), port_name);
        } else {
            panic!("This should be a branch trough a ports")
        }
    } else {
        panic!("Branch {} not found", name)
    }
}
fn assert_variable_decl<'ast>(
    symbol_table: &SymbolTable<'ast>,
    ast: &Ast<'ast>,
    name: &str,
    vtype: VariableType,
) {
    if let Some(SymbolDeclaration::Variable(variableid)) = symbol_table.get(&Symbol::intern(name)) {
        let variable = &ast[*variableid].contents;
        assert_eq!(variable.variable_type, vtype)
    } else {
        panic!("Variable {} not found", name)
    }
}
fn assert_net_decl<'ast>(
    symbol_table: &SymbolTable<'ast>,
    ast: &Ast<'ast>,
    name: &str,
    net_type: NetType,
    discipline: &str,
    signed: bool,
) {
    if let Some(SymbolDeclaration::Net(netid)) = symbol_table.get(&Symbol::intern(name)) {
        let net = &ast[*netid].contents;
        assert_eq!(net.signed, signed);
        assert_eq!(net.discipline.as_str(), discipline);
        assert_eq!(net.net_type, net_type);
    } else {
        panic!("Net {} not found", name)
    }
}

fn get_module_symbol_table<'ast, 'lt>(
    symbol_table: &SymbolTable<'ast>,
    ast: &'lt Ast<'ast>,
    name: &str,
) -> &'lt SymbolTable<'ast> {
    if let Some(SymbolDeclaration::Module(module)) = symbol_table.get(&Symbol::intern(name)) {
        &ast[*module].contents.symbol_table
    } else {
        panic!("Module {} not found", name)
    }
}
#[test]
pub fn variable_decl() -> Result<(), ()> {
    fern::Dispatch::new()
        .format(|out, message, record| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    ast.parse_from_and_print_errors(
        Path::new("tests/parseunits/variable_declaration.va"),
        &source_map_allocator,
        true,
    )
    .ok_or(())?;
    let symbol_table = get_module_symbol_table(&ast.top_symbols, &ast, "test");
    assert_variable_decl(&symbol_table, &ast, "x", REAL);
    assert_variable_decl(&symbol_table, &ast, "y", INTEGER);
    assert_variable_decl(&symbol_table, &ast, "z", INTEGER);
    assert_variable_decl(&symbol_table, &ast, "t", TIME);
    assert_variable_decl(&symbol_table, &ast, "rt", REALTIME);
    Ok(())
}

#[test]
pub fn net_decl() -> Result<(), ()> {
    fern::Dispatch::new()
        .format(|out, message, record| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    ast.parse_from_and_print_errors(
        Path::new("tests/parseunits/net_declaration.va"),
        &source_map_allocator,
        true,
    )
    .ok_or(())?;
    let range: SafeRange<ModuleId> = ast.full_range();
    let module = &ast[range][0].contents;
    let symbol_table = &module.symbol_table;
    assert_eq!(module.name.as_str(), "test");
    assert_net_decl(symbol_table, &ast, "x", WIRE, " ", false);
    assert_net_decl(symbol_table, &ast, "y", WIRE, " ", false);
    assert_net_decl(symbol_table, &ast, "z", UNDECLARED, "electrical", false);
    assert_net_decl(symbol_table, &ast, "l", WIRE, "electrical", true);
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
    ast.parse_from_and_print_errors(Path::new("tests/linear.va"), &source_map_allocator, true)
        .ok_or(())?;

    let range: SafeRange<ModuleId> = ast.full_range();
    let module = &ast[range][0].contents;

    let mut ports = ast[module.port_list].iter();
    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "A");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(port.discipline.as_str(), "electrical");
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap().contents;
    assert_eq!(port.name.as_str(), "B");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(port.discipline.as_str(), "electrical");

    let symbol_table = &module.symbol_table;
    assert_net_decl(symbol_table, &ast, "x", UNDECLARED, "electrical", false);
    assert_net_decl(symbol_table, &ast, "y", UNDECLARED, "electrical", false);
    assert_branch_decl(symbol_table, &ast, "ax", "A", "x");
    assert_branch_decl(symbol_table, &ast, "ay", "A", "y");
    assert_branch_decl(symbol_table, &ast, "xb", "x", "B");
    assert_branch_decl(symbol_table, &ast, "yb", "y", "B");
    assert_branch_decl(symbol_table, &ast, "xy", "x", "y");
    assert_variable_decl(symbol_table, &ast, "C", REAL);
    Ok(())
}
