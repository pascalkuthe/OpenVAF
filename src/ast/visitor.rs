use crate::ast::Primary::{FunctionCall, Integer, Real, UnsignedInteger, VariableOrNetReference};
use crate::ast::{
    Ast, AttributeNode, Attributes, BlockId, Branch, BranchAccess, BranchDeclaration, BranchId,
    Condition, Expression, HierarchicalId, Module, ModuleId, ModuleItem, NetType, Node, Primary,
    SeqBlock, Statement, StatementId, VariableType,
};
use crate::symbol::Ident;
use crate::symbol_table::SymbolDeclaration;

pub trait Visitor<'ast, E>: Sized {
    fn visit_declaration_name(&mut self, ident: &Ident, ast: &Ast<'ast>) -> Result<(), E> {
        /*Nothing to do*/
        Ok(())
    }
    fn visit_reference(&mut self, ident: &Ident, ast: &Ast<'ast>) -> Result<(), E> {
        /*Nothing to do*/
        Ok(())
    }
    fn visit_hierarchical_reference(
        &mut self,
        ident: &HierarchicalId,
        ast: &Ast<'ast>,
    ) -> Result<(), E> {
        /*Nothing to do*/
        Ok(())
    }
    fn visit_net_type(&mut self, net_type: NetType, ast: &Ast<'ast>) -> Result<(), E> {
        /*Nothing to do*/
        Ok(())
    }
    fn visit_variable_type(
        &mut self,
        variable_type: VariableType,
        ast: &Ast<'ast>,
    ) -> Result<(), E> {
        /*Nothing to do*/
        Ok(())
    }
    fn visit_module(&mut self, module: ModuleId<'ast>, ast: &Ast<'ast>) -> Result<(), E> {
        walk_module(self, &ast[module], ast)
    }
    fn visit_statement(&mut self, statement: StatementId<'ast>, ast: &Ast<'ast>) -> Result<(), E> {
        walk_statement(self, &ast[statement], ast)
    }
    fn visit_block(&mut self, block: BlockId<'ast>, ast: &Ast<'ast>) -> Result<(), E> {
        walk_block(self, &ast[block], ast)
    }
    fn visit_condition(
        &mut self,
        condition: &AttributeNode<'ast, Condition<'ast>>,
        ast: &Ast<'ast>,
    ) -> Result<(), E> {
        walk_condition(self, condition, ast)
    }
    fn visit_assign(
        &mut self,
        attributes: Attributes<'ast>,
        ident: &HierarchicalId,
        value: &Node<Expression<'ast>>,
        ast: &Ast<'ast>,
    ) -> Result<(), E> {
        walk_assign(self, ident, value, ast)
    }
    fn visit_contribute(
        &mut self,
        attributes: Attributes<'ast>,
        nature: &Ident,
        branch: &BranchAccess,
        value: &Node<Expression<'ast>>,
        ast: &Ast<'ast>,
    ) -> Result<(), E> {
        walk_contribute(self, nature, branch, value, ast)
    }
    fn visit_branch_declaration(
        &mut self,
        branch_declaration: BranchId<'ast>,
        ast: &Ast<'ast>,
    ) -> Result<(), E> {
        walk_branch_declaration(self, &ast[branch_declaration].contents, ast)
    }
    fn visit_branch_access(
        &mut self,
        nature: &Ident,
        branch: &BranchAccess,
        ast: &Ast<'ast>,
    ) -> Result<(), E> {
        walk_branch_access(self, nature, branch, ast)
    }
    fn visit_branch(&mut self, branch: &Branch, ast: &Ast<'ast>) -> Result<(), E> {
        walk_branch(self, branch, ast)
    }
    fn visit_expression(
        &mut self,
        expr: &Node<Expression<'ast>>,
        ast: &Ast<'ast>,
    ) -> Result<(), E> {
        walk_expression(self, expr, ast)
    }
}

pub fn walk_module<'ast, E, V: Visitor<'ast, E>>(
    v: &mut V,
    module: &AttributeNode<'ast, Module<'ast>>,
    ast: &Ast<'ast>,
) -> Result<(), E> {
    let module = &module.contents;
    for decl in module.symbol_table.values() {
        match decl {
            SymbolDeclaration::Nature => unreachable_unchecked!("Natures can't be declared inside nature so the parser won't ever place this here"),
            SymbolDeclaration::Module(_)=>unreachable_unchecked!("Module cant be declared inside modules so the parser won't ever place this here"),
            SymbolDeclaration::Discipline(_) => unreachable_unchecked!("Discipline can't be declared inside modules so the parser won't ever place this here"),
            SymbolDeclaration::Function(_) => unimplemented!("Functions"),
            SymbolDeclaration::Branch(branch) => v.visit_branch_declaration(*branch,ast)?,
            SymbolDeclaration::Block(_) => (),//Blocks are visited specially as analog / digital blocks since their order and whether they are digital or analog matters and not all blocks are inside the symbol table
            SymbolDeclaration::Port(_) => (),//TODO Port declaration visit
            SymbolDeclaration::Net(_) => (),//TODO net declaration visit
            SymbolDeclaration::Variable(_) => (),//TODO variable declaration visit
            //TODO parameters
        }
    }
    //TODO parameters
    //TODO visit variable/branch declarations?
    for module_item in module.children.iter() {
        match module_item {
            ModuleItem::AnalogStmt(statement) => v.visit_statement(*statement, ast)?,
            ModuleItem::GenerateStatement => unimplemented!("Generate Statement"),
        }
    }
    Ok(())
}
pub fn walk_statement<'ast, E, V: Visitor<'ast, E>>(
    v: &mut V,
    statement: &Statement<'ast>,
    ast: &Ast<'ast>,
) -> Result<(), E> {
    match statement {
        Statement::Block(id) => v.visit_block(*id, ast),
        Statement::Condition(ref condition) => v.visit_condition(condition, ast),
        Statement::Assign(ref attr, ref ident, ref value) => {
            v.visit_assign(*attr, ident, value, ast)
        }
        Statement::Contribute(ref attr, ref nature_name, ref branch, ref value) => {
            v.visit_contribute(*attr, nature_name, branch, value, ast)
        }
        Statement::FunctionCall(ref _attr, ref name, ref args) => unimplemented!("Functions"),
    }
}
pub fn walk_block<'ast, E, V: Visitor<'ast, E>>(
    v: &mut V,
    block: &AttributeNode<'ast, SeqBlock<'ast>>,
    ast: &Ast<'ast>,
) -> Result<(), E> {
    for statement in block.contents.statements.iter() {
        v.visit_statement(*statement, ast)?
    }
    Ok(())
}
pub fn walk_condition<'ast, E, V: Visitor<'ast, E>>(
    v: &mut V,
    condition: &AttributeNode<Condition<'ast>>,
    ast: &Ast<'ast>,
) -> Result<(), E> {
    v.visit_expression(&condition.contents.main_condition, ast)?;
    v.visit_statement(condition.contents.main_condition_statement, ast)?;
    for (condition, statement) in condition.contents.else_ifs.iter() {
        v.visit_expression(&ast[*condition], ast)?;
        v.visit_statement(*statement, ast)?;
    }
    if let Some(statement) = condition.contents.else_statement {
        v.visit_statement(statement, ast)
    } else {
        Ok(())
    }
}
pub fn walk_assign<'ast, E, V: Visitor<'ast, E>>(
    v: &mut V,
    ident: &HierarchicalId,
    value: &Node<Expression<'ast>>,
    ast: &Ast<'ast>,
) -> Result<(), E> {
    v.visit_hierarchical_reference(ident, ast)?;
    v.visit_expression(value, ast)
}
pub fn walk_branch_access<'ast, E, V: Visitor<'ast, E>>(
    v: &mut V,
    nature: &Ident,
    branch_access: &BranchAccess,
    ast: &Ast<'ast>,
) -> Result<(), E> {
    v.visit_reference(nature, ast)?;
    match branch_access {
        BranchAccess::Explicit(ref ident) => v.visit_hierarchical_reference(ident, ast),
        BranchAccess::Implicit(ref branch) => v.visit_branch(branch, ast),
    }
}
pub fn walk_branch<'ast, E, V: Visitor<'ast, E>>(
    v: &mut V,
    branch: &Branch,
    ast: &Ast<'ast>,
) -> Result<(), E> {
    match branch {
        Branch::Port(port) => v.visit_hierarchical_reference(port, ast),
        Branch::Nets(net1, net2) => {
            v.visit_hierarchical_reference(net1, ast)?;
            v.visit_hierarchical_reference(net2, ast)
        }
    }
}
pub fn walk_branch_declaration<'ast, E, V: Visitor<'ast, E>>(
    v: &mut V,
    branch_decl: &BranchDeclaration,
    ast: &Ast<'ast>,
) -> Result<(), E> {
    v.visit_declaration_name(&branch_decl.name, ast)?;
    v.visit_branch(&branch_decl.branch, ast)
}
pub fn walk_contribute<'ast, E, V: Visitor<'ast, E>>(
    v: &mut V,
    nature: &Ident,
    branch: &BranchAccess,
    value: &Node<Expression<'ast>>,
    ast: &Ast<'ast>,
) -> Result<(), E> {
    v.visit_branch_access(nature, branch, ast)?;
    v.visit_expression(value, ast)
}
pub fn walk_expression<'ast, E, V: Visitor<'ast, E>>(
    v: &mut V,
    expr: &Node<Expression<'ast>>,
    ast: &Ast<'ast>,
) -> Result<(), E> {
    match expr.contents {
        Expression::BinaryOperator(lhs, _op, rhs) => {
            v.visit_expression(&ast[lhs], ast)?;
            v.visit_expression(&ast[rhs], ast)
        }
        Expression::UnaryOperator(_op, rhs) => v.visit_expression(&ast[rhs], ast),
        Expression::Primary(Primary::BranchAccess(ref nature, ref branch_access)) => {
            v.visit_branch_access(nature, branch_access, ast)
        }
        Expression::Primary(VariableOrNetReference(ref ident)) => {
            v.visit_hierarchical_reference(ident, ast)
        }
        Expression::Primary(FunctionCall(ref ident, ref parameters)) => {
            v.visit_hierarchical_reference(ident, ast)?;
            if let Some(args) = parameters {
                for arg in &ast[args] {
                    v.visit_expression(arg, ast)?;
                }
            }
            Ok(())
        }
        Expression::Primary(Integer(_))
        | Expression::Primary(Real(_))
        | Expression::Primary(UnsignedInteger(_)) => {
            /*Nothing to do*/
            Ok(())
        }
    }
}
