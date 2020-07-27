/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::NetType::{UNDECLARED, WIRE};
use crate::ast::VariableType;
use crate::ast::VariableType::{Integer, Real};
use crate::ast::{NetType, PortList};

use crate::ast::Ast;
use crate::ir::IdRange;
use crate::symbol::Symbol;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::test::{preprocess_test, PrettyError, TEST_EXPANSION_HINT};
use crate::SourceMap;

#[test]
pub fn module() -> Result<(), PrettyError> {
    fern::Dispatch::new()
        .format(|out, message, _| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();

    let (sm, main_file) = SourceMap::new_with_mainfile("tests/parser/module.va")?;
    let (ts, sm) = preprocess_test(sm, main_file)?;

    let ast = Ast::parse_from_token_stream_user_facing(ts, &sm, TEST_EXPANSION_HINT)?;

    let modules = ast.modules.as_slice();
    let second_module = &modules[1].contents;
    let mut ports = second_module.body_ports.clone().map(|id| ast[id]);

    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "a");
    assert_eq!(port.discipline, None);
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap();
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);
    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "b");
    assert_eq!(port.discipline, None);
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);

    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "c");
    assert_eq!(port.discipline, None);
    assert_eq!(port.net_type, NetType::WIRE);

    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);

    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "d");
    assert_eq!(port.discipline.unwrap().as_str(), "electrical");
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);

    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "e");
    assert_eq!(port.discipline.unwrap().as_str(), "electrical");
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);

    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "f");
    assert_eq!(port.discipline.unwrap().as_str(), "electrical");
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "g");
    assert_eq!(port.discipline.unwrap().as_str(), "electrical");
    assert_eq!(port.net_type, NetType::WIRE);

    let third_module = &modules[2].contents;
    assert_eq!(third_module.ident.as_str(), "test3");

    let mut ports = if let PortList::Declarations(start) = third_module.ports.contents {
        IdRange(start..third_module.body_ports.0.end).map(|id| ast[id])
    } else {
        panic!("Expected head ports!")
    };

    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "a");
    assert_eq!(port.discipline, None);
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap();
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);
    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "b");
    assert_eq!(port.discipline.unwrap().as_str(), "electrical");
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);

    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "c");
    assert_eq!(port.discipline.unwrap().as_str(), "electrical");
    assert_eq!(port.net_type, NetType::TRI);
    Ok(())
}

#[test]
pub fn branch() -> Result<(), PrettyError> {
    fern::Dispatch::new()
        .format(|out, message, _| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();

    let (sm, main_file) = SourceMap::new_with_mainfile("tests/parser/branch.va")?;
    let (ts, sm) = preprocess_test(sm, main_file)?;

    let ast = Ast::parse_from_token_stream_user_facing(ts, &sm, TEST_EXPANSION_HINT)?;

    let module = &ast.modules[0].contents;
    assert_eq!(module.ident.as_str(), "test");
    let ports = &ast.ports;

    let port = ports[0];
    assert_eq!(ast[port.net].contents.ident.as_str(), "a");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);

    let port = ports[1];
    assert_eq!(ast[port.net].contents.ident.as_str(), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);
    let symbol_table = &module.symbol_table;
    assert_branch_decl(symbol_table, &ast, "ab1", "a", "b");
    assert_branch_decl(symbol_table, &ast, "ab2", "a", "b");
    assert_port_branch_decl(symbol_table, &ast, "pa", "a");
    assert_port_branch_decl(symbol_table, &ast, "pb", "b");
    Ok(())
}

fn assert_branch_decl(
    symbol_table: &SymbolTable,
    ast: &Ast,
    name: &str,
    net1_name: &str,
    net2_name: &str,
) {
    if let Some(SymbolDeclaration::Branch(branchid)) = symbol_table.get(&Symbol::intern(name)) {
        assert_eq!(ast[*branchid].contents.hi_net.names[0].as_str(), net1_name);
        assert_eq!(
            ast[*branchid].contents.lo_net.as_ref().unwrap().names[0].as_str(),
            net2_name
        );
    } else {
        panic!("Branch {} not found", name);
    }
}
fn assert_port_branch_decl(symbol_table: &SymbolTable, ast: &Ast, name: &str, port_name: &str) {
    if let Some(SymbolDeclaration::PortBranch(id)) = symbol_table.get(&Symbol::intern(name)) {
        let port = &ast[*id].contents.port;
        assert_eq!(port.names[0].as_str(), port_name);
    } else {
        panic!("Branch {} not found", name)
    }
}
fn assert_variable_decl(symbol_table: &SymbolTable, ast: &Ast, name: &str, vtype: VariableType) {
    if let Some(SymbolDeclaration::Variable(variableid)) = symbol_table.get(&Symbol::intern(name)) {
        let variable = &ast[*variableid].contents;
        assert_eq!(variable.var_type, vtype)
    } else {
        panic!("Variable {} not found", name)
    }
}
fn assert_net_decl(
    symbol_table: &SymbolTable,
    ast: &Ast,
    name: &str,
    net_type: NetType,
    discipline: Option<&str>,
) {
    if let Some(SymbolDeclaration::Net(netid)) = symbol_table.get(&Symbol::intern(name)) {
        let net = &ast[*netid].contents;
        assert_eq!(
            net.discipline
                .map(|discipline| discipline.as_str())
                .as_deref(),
            discipline
        );
        assert_eq!(net.net_type, net_type);
    } else {
        panic!("Net {} not found", name)
    }
}

fn get_module_symbol_table<'lt>(
    symbol_table: &SymbolTable,
    ast: &'lt Ast,
    name: &str,
) -> &'lt SymbolTable {
    if let Some(SymbolDeclaration::Module(module)) = symbol_table.get(&Symbol::intern(name)) {
        &ast[*module].contents.symbol_table
    } else {
        panic!("Module {} not found", name)
    }
}
#[test]
pub fn variable_decl() -> Result<(), PrettyError> {
    fern::Dispatch::new()
        .format(|out, message, _record| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();

    let (sm, main_file) = SourceMap::new_with_mainfile("tests/parser/variable_declaration.va")?;
    let (ts, sm) = preprocess_test(sm, main_file)?;

    let ast = Ast::parse_from_token_stream_user_facing(ts, &sm, TEST_EXPANSION_HINT)?;

    let symbol_table = get_module_symbol_table(&ast.top_symbols, &ast, "test");
    assert_variable_decl(&symbol_table, &ast, "x", Real);
    assert_variable_decl(&symbol_table, &ast, "y", Integer);
    assert_variable_decl(&symbol_table, &ast, "z", Integer);
    assert_variable_decl(&symbol_table, &ast, "t", Integer);
    assert_variable_decl(&symbol_table, &ast, "rt", Real);
    Ok(())
}

#[test]
pub fn net_decl() -> Result<(), PrettyError> {
    fern::Dispatch::new()
        .format(|out, message, _record| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();

    let (sm, main_file) = SourceMap::new_with_mainfile("tests/parser/net_declaration.va")?;
    let (ts, sm) = preprocess_test(sm, main_file)?;

    let ast = Ast::parse_from_token_stream_user_facing(ts, &sm, TEST_EXPANSION_HINT)?;

    let module = &ast.modules[0].contents;
    let symbol_table = &module.symbol_table;
    assert_eq!(module.ident.as_str(), "test");
    assert_net_decl(symbol_table, &ast, "x", WIRE, None);
    assert_net_decl(symbol_table, &ast, "y", WIRE, None);
    assert_net_decl(symbol_table, &ast, "z", UNDECLARED, Some("electrical"));
    assert_net_decl(symbol_table, &ast, "l", WIRE, Some("electrical"));
    Ok(())
}

#[test]
pub fn linear() -> Result<(), PrettyError> {
    #[allow(unused_must_use)]
    fern::Dispatch::new()
        .format(|out, message, _record| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();

    let (sm, main_file) = SourceMap::new_with_mainfile("tests/integration/linear.va")?;
    let (ts, sm) = preprocess_test(sm, main_file)?;

    let ast = Ast::parse_from_token_stream_user_facing(ts, &sm, TEST_EXPANSION_HINT)?;

    let module = &ast.modules[0].contents;

    let mut ports = ast[module.body_ports.clone()].iter();
    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "A");
    assert_eq!(port.discipline.unwrap().as_str(), "electrical");
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports.next().unwrap();
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    let port = ast[port.net].contents;
    assert_eq!(port.ident.as_str(), "B");
    assert_eq!(port.discipline.unwrap().as_str(), "electrical");

    let symbol_table = &module.symbol_table;
    assert_net_decl(symbol_table, &ast, "x", UNDECLARED, Some("electrical"));
    assert_net_decl(symbol_table, &ast, "y", UNDECLARED, Some("electrical"));
    assert_branch_decl(symbol_table, &ast, "ax", "A", "x");
    assert_branch_decl(symbol_table, &ast, "ay", "A", "y");
    assert_branch_decl(symbol_table, &ast, "xb", "x", "B");
    assert_branch_decl(symbol_table, &ast, "yb", "y", "B");
    assert_branch_decl(symbol_table, &ast, "xy", "x", "y");
    assert_variable_decl(symbol_table, &ast, "C", Real);
    Ok(())
}
