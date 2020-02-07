use crate::ast::Primary::{FunctionCall, Integer, Real, UnsignedInteger, VariableOrNetReference};
use crate::ast::{AttributeNode, Attributes};
use crate::ir::hir::{BranchAccess, Condition, Hir, Module};
use crate::ir::{
    BlockId, BranchId, DisciplineId, ExpressionId, ModuleId, NatureId, NetId, PortId, StatementId,
    VariableId,
};
use crate::symbol::Ident;
use crate::symbol_table::SymbolDeclaration;

#[allow(unused)]
pub trait Visitor<'hir, E = ()>: Sized {
    fn visit_module(&mut self, module: ModuleId<'hir>, ast: &Hir<'hir>) -> Result<(), E> {
        walk_module(self, &ast[module], ast)
    }
    fn visit_statement(&mut self, statement: StatementId<'hir>, ast: &Hir<'hir>) -> Result<(), E> {
        walk_statement(self, &ast[statement], ast)
    }
    fn visit_block(&mut self, block: BlockId<'hir>, ast: &Hir<'hir>) -> Result<(), E> {
        walk_block(self, &ast[block], ast)
    }
    fn visit_condition(
        &mut self,
        condition: &AttributeNode<'hir, Condition<'hir>>,
        ast: &Hir<'hir>,
    ) -> Result<(), E> {
        walk_condition(self, condition, ast)
    }
    fn visit_assign(
        &mut self,
        attributes: Attributes<'hir>,
        ident: &HierarchicalId,
        value: ExpressionId<'hir>,
        ast: &Hir<'hir>,
    ) -> Result<(), E> {
        walk_assign(self, ident, value, ast)
    }
    fn visit_contribute(
        &mut self,
        attributes: Attributes<'hir>,
        nature: &Ident,
        branch: &BranchAccess,
        value: ExpressionId<'hir>,
        ast: &Hir<'hir>,
    ) -> Result<(), E> {
        walk_contribute(self, nature, branch, value, ast)
    }

    fn visit_branch_access(
        &mut self,
        nature: &Ident,
        branch: &BranchAccess,
        ast: &Hir<'hir>,
    ) -> Result<(), E> {
        walk_branch_access(self, nature, branch, ast)
    }
    fn visit_branch(&mut self, branch: &Branch, ast: &Hir<'hir>) -> Result<(), E> {
        walk_branch(self, branch, ast)
    }
    fn visit_expression(&mut self, expr: ExpressionId<'hir>, ast: &Hir<'hir>) -> Result<(), E> {
        walk_expression(self, &ast[expr], ast)
    }
    fn visit_expression_primary(
        &mut self,
        primary: &Primary<'hir>,
        ast: &Hir<'hir>,
    ) -> Result<(), E> {
        walk_expression_primary(self, primary, ast)
    }

    fn visit_branch_declaration(
        &mut self,
        branch_declaration: BranchId<'hir>,
        ast: &Hir<'hir>,
    ) -> Result<(), E> {
        walk_branch_declaration(self, &ast[branch_declaration].contents, ast)
    }

    fn visit_port(&mut self, port: PortId<'hir>, ast: &Hir<'hir>) -> Result<(), E> {
        /*Nothing to do*/
        Ok(())
    }

    fn visit_net(&mut self, net: NetId<'hir>, ast: &Hir<'hir>) -> Result<(), E> {
        /*Nothing to do*/
        Ok(())
    }

    fn visit_variable(&mut self, variable: VariableId<'hir>, ast: &Hir<'hir>) -> Result<(), E> {
        /*Nothing to do*/
        Ok(())
    }
    fn visit_nature(&mut self, nature: NatureId<'hir>, ast: &Hir<'hir>) -> Result<(), E> {
        /*Nothing to do*/
        Ok(())
    }
    fn visit_discipline(
        &mut self,
        discipline: DisciplineId<'hir>,
        ast: &Hir<'hir>,
    ) -> Result<(), E> {
        /*Nothing to do*/
        Ok(())
    }
}

pub fn walk_module<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    module: &AttributeNode<'hir, Module<'hir>>,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    let module = &module.contents;
    for decl in module.symbol_table.values().copied() {
        match decl {
            SymbolDeclaration::Nature(_) => unreachable_unchecked!("Natures can't be declared inside nature so the parser won't ever place this here"),
            SymbolDeclaration::Module(_)=>unreachable_unchecked!("Module cant be declared inside modules so the parser won't ever place this here"),
            SymbolDeclaration::Discipline(_) => unreachable_unchecked!("Discipline can't be declared inside modules so the parser won't ever place this here"),
            SymbolDeclaration::Function(_) => unimplemented!("Functions"),
            SymbolDeclaration::Branch(branch) => v.visit_branch_declaration(branch,ast)?,
            SymbolDeclaration::Block(_) => (),//Blocks are visited specially as analog / digital blocks since their order and whether they are digital or analog matters and not all blocks are inside the symbol table
            SymbolDeclaration::Port(portid) => v.visit_port(portid,ast)?,
            SymbolDeclaration::Net(netid) => v.visit_net(netid,ast)?,
            SymbolDeclaration::Variable(variableid) => v.visit_variable(variableid,ast)?,
            //TODO parameters
        }
    }
    //TODO parameters
    for module_item in module.children.iter() {
        match module_item {
            ModuleItem::AnalogStmt(statement) => v.visit_statement(*statement, ast)?,
            ModuleItem::GenerateStatement => unimplemented!("Generate Statement"),
        }
    }
    Ok(())
}
pub fn walk_statement<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    statement: &Statement<'hir>,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    match statement {
        Statement::Block(id) => v.visit_block(*id, ast),
        Statement::Condition(ref condition) => v.visit_condition(condition, ast),
        Statement::Assign(ref attr, ref ident, value) => v.visit_assign(*attr, ident, *value, ast),
        Statement::Contribute(ref attr, ref nature_name, ref branch, value) => {
            v.visit_contribute(*attr, nature_name, branch, *value, ast)
        }
        Statement::FunctionCall(ref _attr, ref name, ref args) => unimplemented!("Functions"),
    }
}
pub fn walk_block<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    block: &AttributeNode<'hir, SeqBlock<'hir>>,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    for statement in block.contents.statements.iter() {
        v.visit_statement(*statement, ast)?
    }
    Ok(())
}
pub fn walk_condition<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    condition: &AttributeNode<Condition<'hir>>,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    v.visit_expression(condition.contents.main_condition, ast)?;
    v.visit_statement(condition.contents.main_condition_statement, ast)?;
    for (condition, statement) in condition.contents.else_ifs.iter().copied() {
        v.visit_expression(condition, ast)?;
        v.visit_statement(statement, ast)?;
    }
    if let Some(statement) = condition.contents.else_statement {
        v.visit_statement(statement, ast)
    } else {
        Ok(())
    }
}
pub fn walk_assign<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    ident: &HierarchicalId,
    value: ExpressionId<'hir>,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    v.visit_hierarchical_reference(ident, ast)?;
    v.visit_expression(value, ast)
}
pub fn walk_branch_access<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    nature: &Ident,
    branch_access: &BranchAccess,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    v.visit_reference(nature, ast)?;
    match branch_access {
        BranchAccess::Explicit(ref ident) => v.visit_hierarchical_reference(ident, ast),
        BranchAccess::Implicit(ref branch) => v.visit_branch(branch, ast),
    }
}
pub fn walk_branch<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    branch: &Branch,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    match branch {
        Branch::Port(port) => v.visit_hierarchical_reference(port, ast),
        Branch::Nets(net1, net2) => {
            v.visit_hierarchical_reference(net1, ast)?;
            v.visit_hierarchical_reference(net2, ast)
        }
    }
}
pub fn walk_branch_declaration<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    branch_decl: &BranchDeclaration,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    v.visit_declaration_name(&branch_decl.name, ast)?;
    v.visit_branch(&branch_decl.branch, ast)
}
pub fn walk_contribute<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    nature: &Ident,
    branch: &BranchAccess,
    value: ExpressionId<'hir>,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    v.visit_branch_access(nature, branch, ast)?;
    v.visit_expression(value, ast)
}
pub fn walk_expression<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    expr: &Node<Expression<'hir>>,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    match expr.contents {
        Expression::BinaryOperator(lhs, _op, rhs) => {
            v.visit_expression(lhs, ast)?;
            v.visit_expression(rhs, ast)
        }
        Expression::UnaryOperator(_op, rhs) => v.visit_expression(rhs, ast),
        Expression::Primary(ref primary) => v.visit_expression_primary(primary, ast),
    }
}
pub fn walk_expression_primary<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    primary: &Primary<'hir>,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    match primary {
        Primary::BranchAccess(ref nature, ref branch_access) => {
            v.visit_branch_access(nature, branch_access, ast)?
        }
        Primary::VariableOrNetReference(ref ident) => v.visit_hierarchical_reference(ident, ast)?,
        Primary::FunctionCall(ref ident, ref parameters) => {
            v.visit_hierarchical_reference(ident, ast)?;
            for parameter in parameters.iter().copied() {
                v.visit_expression(parameter, ast)?;
            }
        }
        Integer(_) | UnsignedInteger(_) | Real(_) => { /*Nothing to do*/ }
    }
    Ok(())
}

fn walk_variable<'hir, E, V: Visitor<'hir, E>>(
    v: &mut V,
    variable: VariableId<'hir>,
    ast: &Hir<'hir>,
) -> Result<(), E> {
    if let Some(default) = ast[variable].contents.default_value {
        v.visit_expression(default, ast)?;
    }
    Ok(())
}
