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

use crate::ast::{Branch, VariableType};
use crate::ast::{ModuleItem, NetType, TopNode};

const PARSE_UNIT_DIRECTORY: &'static str = "tests/parseunits/";
#[test]
pub fn module() -> Result<(), ()> {
    let (source_map, res) = super::parse(Path::new(&format!("{}module.va", PARSE_UNIT_DIRECTORY)))
        .expect("Test File not found");
    let ast = match res {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&source_map);
            return Err(());
        }
    };
    let mut top_nodes = ast.top_nodes().iter();
    let first_module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents.contents
    {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(ast.data.get_str(first_module.name), "test1");
    let second_module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents.contents
    {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(ast.data.get_str(second_module.name), "test2");
    let ports = ast.data.get_slice(second_module.port_list);

    let port = ports[0].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "a");
    assert_eq!(port.output, false);
    assert_eq!(port.input, false);

    let port = ports[1].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, false);

    let port = ports[2].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "c");
    assert_eq!(port.output, false);
    assert_eq!(port.input, false);

    let port = ports[3].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "d");
    assert_eq!(port.output, false);
    assert_eq!(port.input, false);

    let port = ports[4].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "e");
    assert_eq!(port.output, false);
    assert_eq!(port.input, false);

    let port = ports[5].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "f");
    assert_eq!(port.output, false);
    assert_eq!(port.input, false);

    let port = ports[6].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "g");
    assert_eq!(port.output, false);
    assert_eq!(port.input, false);

    //declarations in body

    let port = ports[7].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "a");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    assert_eq!(port.signed, false);
    assert!(port.discipline.is_none());
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports[8].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, true);
    assert!(port.discipline.is_none());
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports[9].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "c");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert!(port.discipline.is_none());
    assert_eq!(port.net_type, NetType::WIRE);

    let port = ports[10].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "d");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports[11].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "e");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    assert_eq!(port.signed, false);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports[12].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "f");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    assert_eq!(port.signed, false);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports[13].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "g");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, true);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.net_type, NetType::WIRE);

    let third_module = if let TopNode::Module(module) = top_nodes.next().unwrap().contents.contents
    {
        module
    } else {
        panic!("Parsed Something else than a module!")
    };
    assert_eq!(ast.data.get_str(third_module.name), "test3");
    let ports = ast.data.get_slice(third_module.port_list);
    let port = ports[0].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "a");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    assert_eq!(port.signed, false);
    assert!(port.discipline.is_none());
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports[1].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.net_type, NetType::UNDECLARED);

    let port = ports[2].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "c");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.net_type, NetType::TRI);
    Ok(())
}
#[test]
pub fn branch() -> Result<(), ()> {
    let (source_map, res) = super::parse(Path::new(&format!("{}branch.va", PARSE_UNIT_DIRECTORY)))
        .expect("Test File not found");
    let ast = match res {
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
    let ports = ast.data.get_slice(module.port_list);

    let port = ports[0].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "a");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);

    let port = ports[1].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);

    let mut children = ast.data.get_slice(module.children).iter();
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(branch.name), "ab1");
        if let Branch::Nets(net1, net2) = branch.branch {
            assert_eq!(ast.data.get_str(net1.name), "a");
            assert_eq!(ast.data.get_str(net2.name), "b");
        } else {
            panic!("This should be a branch between two nets")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(branch.name), "ab2");
        if let Branch::Nets(net1, net2) = branch.branch {
            assert_eq!(ast.data.get_str(net1.name), "a");
            assert_eq!(ast.data.get_str(net2.name), "b");
        } else {
            panic!("This should be a branch between two nets")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(branch.name), "pa");
        if let Branch::Port(port) = branch.branch {
            assert_eq!(ast.data.get_str(port.name), "a");
        } else {
            panic!("This should be a branch trough a port")
        }
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::BranchDecl(branch) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(branch.name), "pb");
        if let Branch::Port(port) = branch.branch {
            assert_eq!(ast.data.get_str(port.name), "b");
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
    let (source_map, res) = super::parse(Path::new(&format!(
        "{}variable_declaration.va",
        PARSE_UNIT_DIRECTORY
    )))
    .expect("Test File not found");
    let ast = match res {
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
    let ports = ast.data.get_slice(module.port_list);

    let mut children = ast.data.get_slice(module.children).iter();
    if let ModuleItem::VariableDecl(variable) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(variable.name), "x");
        assert_eq!(variable.variable_type, VariableType::REAL)
    } else {
        panic!("Found something else than a branch decl")
    }

    if let ModuleItem::VariableDecl(variable) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(variable.name), "y");
        assert_eq!(variable.variable_type, VariableType::INTEGER)
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::VariableDecl(variable) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(variable.name), "z");
        assert_eq!(variable.variable_type, VariableType::INTEGER)
    } else {
        panic!("Found something else than a branch decl")
    }
    if let ModuleItem::VariableDecl(variable) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(variable.name), "t");
        assert_eq!(variable.variable_type, VariableType::TIME)
    } else {
        panic!("Found something else than a variable decl")
    }
    if let ModuleItem::VariableDecl(variable) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(variable.name), "rt");
        assert_eq!(variable.variable_type, VariableType::REALTIME)
    } else {
        panic!("Found something else than a variable decl")
    }
    Ok(())
}

#[test]
pub fn net_decl() -> Result<(), ()> {
    let (source_map, res) = super::parse(Path::new(&format!(
        "{}net_declaration.va",
        PARSE_UNIT_DIRECTORY
    )))
    .expect("Test File not found");
    let ast = match res {
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

    let mut children = ast.data.get_slice(module.children).iter();
    if let ModuleItem::NetDecl(net) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(net.name), "x");
        assert_eq!(net.signed, false);
        assert!(net.discipline.is_none());
        assert_eq!(net.net_type, NetType::WIRE);
    } else {
        panic!("Found something else than a net decl")
    }

    if let ModuleItem::NetDecl(net) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(net.name), "y");
        assert_eq!(net.signed, false);
        assert!(net.discipline.is_none());
        assert_eq!(net.net_type, NetType::WIRE);
    } else {
        panic!("Found something else than a net decl")
    }

    if let ModuleItem::NetDecl(net) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(net.name), "x");
        assert_eq!(net.signed, false);
        assert_eq!(ast.data.get_str(net.discipline.unwrap().name), "electrical");
        assert_eq!(net.net_type, NetType::UNDECLARED);
    } else {
        panic!("Found something else than a net decl")
    }

    if let ModuleItem::NetDecl(net) = children.next().unwrap().contents.contents {
        assert_eq!(ast.data.get_str(net.name), "x");
        assert_eq!(net.signed, true);
        assert_eq!(ast.data.get_str(net.discipline.unwrap().name), "electrical");
        assert_eq!(net.net_type, NetType::WIRE);
    } else {
        panic!("Found something else than a net decl")
    }
    Ok(())
}
