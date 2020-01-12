use std::path::Path;

use crate::ast::Branch;
use crate::ast::{ModuleItem, TopNode, VerilogType};

const parse_unit_directory: &'static str = "tests/parseunits/";
#[test]
pub fn module() -> Result<(), ()> {
    let (source_map, res) = super::parse(Path::new(&format!("{}module.va", parse_unit_directory)))
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
    assert_eq!(port.verilog_type, VerilogType::UNDECLARED);

    let port = ports[8].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, true);
    assert!(port.discipline.is_none());
    assert_eq!(port.verilog_type, VerilogType::UNDECLARED);

    let port = ports[9].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "c");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert!(port.discipline.is_none());
    assert_eq!(port.verilog_type, VerilogType::WIRE);

    let port = ports[10].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "d");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.verilog_type, VerilogType::UNDECLARED);

    let port = ports[11].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "e");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    assert_eq!(port.signed, false);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.verilog_type, VerilogType::UNDECLARED);

    let port = ports[12].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "f");
    assert_eq!(port.output, true);
    assert_eq!(port.input, false);
    assert_eq!(port.signed, false);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.verilog_type, VerilogType::UNDECLARED);

    let port = ports[13].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "g");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, true);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.verilog_type, VerilogType::WIRE);

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
    assert_eq!(port.verilog_type, VerilogType::UNDECLARED);

    let port = ports[1].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "b");
    assert_eq!(port.output, false);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.verilog_type, VerilogType::UNDECLARED);

    let port = ports[2].contents.contents;
    assert_eq!(ast.data.get_str(port.name), "c");
    assert_eq!(port.output, true);
    assert_eq!(port.input, true);
    assert_eq!(port.signed, false);
    assert_eq!(
        ast.data.get_str(port.discipline.unwrap().name),
        "electrical"
    );
    assert_eq!(port.verilog_type, VerilogType::TRI);
    Ok(())
}
#[test]
pub fn branch() -> Result<(), ()> {
    let (source_map, res) = super::parse(Path::new(&format!("{}branch.va", parse_unit_directory)))
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
        assert_eq!(ast.data.get_str(branch.name), "ab");
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
