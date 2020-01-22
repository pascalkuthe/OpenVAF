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

use crate::ast::{Branch, Expression, Statement, VariableType};
use crate::ast::{ModuleItem, NetType, TopNode};
use crate::symbol::keywords::EMPTY_SYMBOL;

const PARSE_UNIT_DIRECTORY: &'static str = "tests/parseunits/";
#[test]
pub fn module() -> Result<(), ()> {
    let source_map_allocator = Bump::new();
    let ast_allocator = Bump::new();
    let (source_map, res) = super::parse(
        Path::new(&format!("{}module.va", PARSE_UNIT_DIRECTORY)),
        &source_map_allocator,
        &ast_allocator,
    )
    .expect("Test File not found");
    let (ast, _) = match res {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&source_map);
            return Err(());
        }
    };
    let mut top_nodes = ast.iter();
    let first_module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(first_module.name.as_str(), "test1");
    let second_module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(second_module.name.as_str(), "test2");
    let mut ports = second_module.port_list.iter();

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

    let third_module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(third_module.name.as_str(), "test3");
    let mut ports = third_module.port_list.iter();
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
    let source_map_allocator = Bump::new();
    let ast_allocator = Bump::new();
    let (source_map, res) = super::parse(
        Path::new(&format!("{}branch.va", PARSE_UNIT_DIRECTORY)),
        &source_map_allocator,
        &ast_allocator,
    )
    .expect("Test File not found");
    let (ast, _) = match res {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&source_map);
            return Err(());
        }
    };
    let mut top_nodes = ast.iter();
    let module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(module.name.as_str(), "test");
    let ports = module.port_list;

    let port = ports[0].contents;
    assert_eq!(port.name.as_str(), "a");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);

    let port = ports[1].contents;
    assert_eq!(port.name.as_str(), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);

    let mut children = module.children.iter();
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap() {
        let branch = &branch.contents;
        assert_eq!(branch.name.as_str(), "ab1");
        if let Branch::Nets(net1, net2) = branch.branch {
            assert_eq!(net1.names[0].as_str(), "a");
            assert_eq!(net2.names[0].as_str(), "b");
        } else {
            panic!("This should be a branch between two nets")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap() {
        let branch = &branch.contents;
        assert_eq!(branch.name.as_str(), "ab2");
        if let Branch::Nets(net1, net2) = branch.branch {
            assert_eq!(net1.names[0].as_str(), "a");
            assert_eq!(net2.names[0].as_str(), "b");
        } else {
            panic!("This should be a branch between two nets")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap() {
        let branch = &branch.contents;
        assert_eq!(branch.name.as_str(), "pa");
        if let Branch::Port(port) = branch.branch {
            assert_eq!(port.names[0].as_str(), "a");
        } else {
            panic!("This should be a branch trough a port")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap() {
        let branch = &branch.contents;
        assert_eq!(branch.name.as_str(), "pb");
        if let Branch::Port(port) = branch.branch {
            assert_eq!(port.names[0].as_str(), "b");
        } else {
            panic!("This should be a branch trough a port")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    Ok(())
}

#[test]
pub fn variable_decl() -> Result<(), ()> {
    let source_map_allocator = Bump::new();
    let ast_allocator = Bump::new();
    let (source_map, res) = super::parse(
        Path::new(&format!("{}variable_declaration.va", PARSE_UNIT_DIRECTORY)),
        &source_map_allocator,
        &ast_allocator,
    )
    .expect("Test File not found");
    let (ast, _) = match res {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&source_map);
            return Err(());
        }
    };
    let mut top_nodes = ast.iter();
    let module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(module.name.as_str(), "test");
    let ports = module.port_list;

    let mut children = module.children.iter();
    if let ModuleItem::VariableDecl(variable) = children.next().unwrap() {
        let variable = &variable.contents;
        assert_eq!(variable.name.as_str(), "x");
        assert_eq!(variable.variable_type, VariableType::REAL)
    } else {
        panic!("Found something else than a branch decl")
    }

    if let ModuleItem::VariableDecl(variable) = children.next().unwrap() {
        let variable = &variable.contents;
        assert_eq!(variable.name.as_str(), "y");
        assert_eq!(variable.variable_type, VariableType::INTEGER)
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::VariableDecl(variable) = children.next().unwrap() {
        let variable = &variable.contents;
        assert_eq!(variable.name.as_str(), "z");
        assert_eq!(variable.variable_type, VariableType::INTEGER)
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::VariableDecl(variable) = children.next().unwrap() {
        let variable = &variable.contents;
        assert_eq!(variable.name.as_str(), "t");
        assert_eq!(variable.variable_type, VariableType::TIME)
    } else {
        panic!("Found something else than a variable decl")
    }
    if let ModuleItem::VariableDecl(variable) = children.next().unwrap() {
        let variable = &variable.contents;
        assert_eq!(variable.name.as_str(), "rt");
        assert_eq!(variable.variable_type, VariableType::REALTIME)
    } else {
        panic!("Found something else than a variable decl")
    }
    Ok(())
}

#[test]
pub fn net_decl() -> Result<(), ()> {
    let source_map_allocator = Bump::new();
    let ast_allocator = Bump::new();
    let (source_map, res) = super::parse(
        Path::new(&format!("{}net_declaration.va", PARSE_UNIT_DIRECTORY)),
        &source_map_allocator,
        &ast_allocator,
    )
    .expect("Test File not found");
    let (ast, _) = match res {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&source_map);
            return Err(());
        }
    };
    let mut top_nodes = ast.iter();
    let module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(module.name.as_str(), "test");

    let mut children = module.children.iter();
    if let ModuleItem::NetDecl(net) = children.next().unwrap() {
        let net = &net.contents;
        assert_eq!(net.name.as_str(), "x");
        assert_eq!(net.signed, false);
        assert_eq!(net.discipline.name, EMPTY_SYMBOL);
        assert_eq!(net.net_type, NetType::WIRE);
    } else {
        panic!("Found something else than a net decl")
    }

    if let ModuleItem::NetDecl(net) = children.next().unwrap() {
        let net = &net.contents;
        assert_eq!(net.name.as_str(), "y");
        assert_eq!(net.signed, false);
        assert_eq!(net.discipline.name, EMPTY_SYMBOL);
        assert_eq!(net.net_type, NetType::WIRE);
    } else {
        panic!("Found something else than a net decl")
    }

    if let ModuleItem::NetDecl(net) = children.next().unwrap() {
        let net = &net.contents;
        assert_eq!(net.name.as_str(), "z");
        assert_eq!(net.signed, false);
        assert_eq!(net.discipline.as_str(), "electrical");
        assert_eq!(net.net_type, NetType::UNDECLARED);
    } else {
        panic!("Found something else than a net decl")
    }

    if let ModuleItem::NetDecl(net) = children.next().unwrap() {
        let net = &net.contents;
        assert_eq!(net.name.as_str(), "l");
        assert_eq!(net.signed, true);
        assert_eq!(net.discipline.as_str(), "electrical");
        assert_eq!(net.net_type, NetType::WIRE);
    } else {
        panic!("Found something else than a net decl")
    }
    Ok(())
}

#[test]
pub fn linear() -> Result<(), ()> {
    let source_map_allocator = Bump::new();
    let ast_allocator = Bump::new();
    let (source_map, res) = super::parse(
        Path::new("tests/linear.va"),
        &source_map_allocator,
        &ast_allocator,
    )
    .expect("Test File not found");
    let (ast, symbol_table) = match res {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&source_map);
            return Err(());
        }
    };
    let mut top_nodes = ast.iter();
    let module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };

    let mut ports = module.port_list.iter();
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
    let mut children = module.children.iter();
    if let ModuleItem::NetDecl(net) = children.next().unwrap() {
        let net = &net.contents;
        assert_eq!(net.name.as_str(), "x");
        assert_eq!(net.signed, false);
        assert_eq!(net.discipline.as_str(), "electrical");
        assert_eq!(net.net_type, NetType::UNDECLARED);
    } else {
        panic!("Found something else than a net decl")
    }
    if let ModuleItem::NetDecl(net) = children.next().unwrap() {
        let net = &net.contents;
        assert_eq!(net.name.as_str(), "y");
        assert_eq!(net.signed, false);
        assert_eq!(net.discipline.as_str(), "electrical");
        assert_eq!(net.net_type, NetType::UNDECLARED);
    } else {
        panic!("Found something else than a net decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap() {
        let branch = &branch.contents;
        assert_eq!(branch.name.as_str(), "ax");
        if let Branch::Nets(net1, net2) = branch.branch {
            assert_eq!(net1.names[0].as_str(), "A");
            assert_eq!(net2.names[0].as_str(), "x");
        } else {
            panic!("This should be a branch between two nets")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap() {
        let branch = &branch.contents;
        assert_eq!(branch.name.as_str(), "ay");
        if let Branch::Nets(net1, net2) = branch.branch {
            assert_eq!(net1.names[0].as_str(), "A");
            assert_eq!(net2.names[0].as_str(), "y");
        } else {
            panic!("This should be a branch between two nets")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap() {
        let branch = &branch.contents;
        assert_eq!(branch.name.as_str(), "xb");
        if let Branch::Nets(net1, net2) = branch.branch {
            assert_eq!(net1.names[0].as_str(), "x");
            assert_eq!(net2.names[0].as_str(), "B");
        } else {
            panic!("This should be a branch between two nets")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap() {
        let branch = &branch.contents;
        assert_eq!(branch.name.as_str(), "yb");
        if let Branch::Nets(net1, net2) = branch.branch {
            assert_eq!(net1.names[0].as_str(), "y");
            assert_eq!(net2.names[0].as_str(), "B");
        } else {
            panic!("This should be a branch between two nets")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap() {
        let branch = &branch.contents;
        assert_eq!(branch.name.as_str(), "xy");
        if let Branch::Nets(net1, net2) = branch.branch {
            assert_eq!(net1.names[0].as_str(), "x");
            assert_eq!(net2.names[0].as_str(), "y");
        } else {
            panic!("This should be a branch between two nets")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::VariableDecl(variable) = children.next().unwrap() {
        let variable = &variable.contents;
        assert_eq!(variable.name.as_str(), "C");
        assert_eq!(variable.variable_type, VariableType::REAL)
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::AnalogStmt(analog) = children.next().unwrap() {
        if let Statement::Block(block) = analog.contents {
            assert!(block.scope.is_none());
            let contents = Vec::from(block.statements);
            let tmp = 2;
        }
    } else {
        panic!("Found something else than an analog_block")
    }
    Ok(())
}

/*#[test]
pub fn contribute() -> Result<(), ()> {
    let (source_map, res) =
        super::parse(Path::new(&format!("{}contribute.va", PARSE_UNIT_DIRECTORY)))
            .expect("Test File not found");
    let (ast,_) = match res {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&source_map);
            return Err(());
        }
    };
    let mut top_nodes = ast.top_nodes().iter();
    let module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents.contents {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(ast.data.get_str(module.name), "test");
    Ok(());
}
#[test]
pub fn assignment() -> Result<(), ()> {
    let (source_map, res) =
        super::parse(Path::new(&format!("{}assignment.va", PARSE_UNIT_DIRECTORY)))
            .expect("Test File not found");
    let (ast,_) = match res {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&source_map);
            return Err(());
        }
    };
    let mut top_nodes = ast.top_nodes().iter();
    let module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents.contents {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(ast.data.get_str(module.name), "test");
    Ok(());
}
#[test]
pub fn condition() -> Result<(), ()> {
    let (source_map, res) = super::parse(Path::new(&format!("{}if.va", PARSE_UNIT_DIRECTORY)))
        .expect("Test File not found");
    let (ast,_) = match res {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&source_map);
            return Err(());
        }
    };
    let mut top_nodes = ast.top_nodes().iter();
    let module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents.contents {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(ast.data.get_str(module.name), "test");
    Ok(());
}*/
