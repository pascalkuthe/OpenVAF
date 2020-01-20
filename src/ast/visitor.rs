use crate::ast::{
    Ast, AttributeNode, BranchDeclaration, Module, ModuleItem, Net, NetType, Node, Port, Reference,
    VariableType,
};

pub trait Visitor<'ast> {
    fn visit_name(&mut self, ident: Ident) {
        /*Nothing to do*/
    }
    fn visit_net_type(&mut self, net_type: NetType, ast: &'astAst) {
        /*Nothing to do*/
    }
    fn visit_variable_type(&mut self, variable_type: VariableType, ast: &'astAst) {
        /*Nothing to do*/
    }
    fn visit_reference<T>(&mut self, reference: Reference<T>, ast: &'astAst) -> Option<&'astT> {
        walk_reference(self, reference, ast)
    }
    fn visit_attribute_node<T>(
        &mut self,
        attribute_node: &'ast AttributeNode<T>,
        ast: &'astAst,
    ) -> &'astT {
        walk_attribute_node(self, attribute_node, ast)
    }
    fn visit_node<T>(&mut self, node: &'ast Node<T>, ast: &'astAst) -> &'astT {
        walk_node(self, node, ast)
    }
    fn visit_module(&mut self, module: &'astModule, ast: &'astAst) {
        walk_module(self, module, ast)
    }
    fn visit_port_declaration(&mut self, port: &'astPort, ast: &'astAst) {
        walk_port_declaration(self, port, ast)
    }
    fn visit_net_declaration(&mut self, net: &'astNet, ast: &'astAst) {
        walk_net_declaration(self, net, ast)
    }
    fn visit_branch_declaration(&mut self, branch: &'astBranchDeclaration, ast: &'astAst) {
        walk_branch_declaration(self, net, ast)
    }
    fn visit_branch(&mut self, branch: &'astBranchDeclaration, ast: &'astAst) {
        walk_branch(self, net, ast)
    }
}

/// This does not visit the declaration it just returns a reference to it so you can do it yourself
pub fn walk_reference<'a, V: Visitor<'a>, T>(
    visitor: &mut V,
    reference: Reference<T>,
    ast: &'a Ast,
) -> Option<&'a T> {
    visitor.visit_name(reference.name, ast);
    if let Some(declaration) = reference.declaration {
        let declaration = ast.data.get_ref(declaration);
        Some(visitor.visit_node(declaration))
    } else {
        None
    }
}
/// This does not visit the contents of the attribute not instead it just returns a reference to it so you can do it yourself
pub fn walk_attribute_node<'a, V: Visitor<'a>, T>(
    visitor: &mut V,
    attribute_node: &'a AttributeNode<T>,
    ast: &'a Ast,
) -> &'a T {
    visitor.visit_node(&attribute_node.contents)
}
/// This does not visit the contents of the attribute not instead it just returns a reference to it so you can do it yourself
pub fn walk_node<'a, V: Visitor<'a>, T>(visitor: &mut V, node: &'a Node<T>, ast: &'a Ast) -> &'a T {
    &node.contents
}
pub fn walk_module<'a, V: Visitor<'a>>(visitor: &mut V, module: &'a Module, ast: &'a Ast) {
    visitor.visit_name(module.name, ast);
    let ports = ast.data.get_slice(module.port_list);
    for port in ports {
        let port = visitor.visit_attribute_node(port);
        visitor.visit_port_declaration(port);
    }
    let module_items = ast.data.get_slice(module.children);
    for module_item in module_items {
        let module_item = visitor.visit_attribute_node(module_item, ast);
        match module_item {
            ModuleItem::NetDecl(net) => visitor.visit_net(net, ast),
            ModuleItem::BranchDecl(branch_decl) => visitor.visit_branch_decl(branch_decl, ast),
            ModuleItem::VariableDecl(variable) => visitor.visit_variable_declaration(variable),
            ModuleItem::ParameterDecl => unimplemented!("Parameters"),
            ModuleItem::AnalogStmt(statement) => visitor.visit_statement(statement),
        }
    }
}
pub fn walk_port_declaration<'a, V: Visitor<'a>>(visitor: &mut V, port: &'a Port, ast: &'a Ast) {
    visitor.visit_name(port.name, ast);
    if let Some(discipline) = port.discipline {
        let discipline_declaration = visitor.visit_reference(discipline, ast);
        //TODO discipline declaration
    }
    visitor.visit_net_type(port.net_type);
}
pub fn walk_net_declaration<'a, V: Visitor<'a>>(visitor: &mut V, net: &'a Net, ast: &'a Ast) {
    visitor.visit_name(net.name, ast);
    if let Some(discipline) = net.discipline {
        let discipline_declaration = visitor.visit_reference(discipline, ast);
        //TODO discipline declaration
    }
    visitor.visit_net_type(net.net_type);
}
