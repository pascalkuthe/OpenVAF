/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::BuiltInFunctionCall::*;
use crate::ast::Primary::{Integer, Real, UnsignedInteger};
use crate::ast::{
    Ast, AttributeNode, Attributes, Branch, BranchAccess, BranchDeclaration, BuiltInFunctionCall,
    Condition, Expression, HierarchicalId, Module, ModuleItem, NetType, Node, ParameterType,
    Primary, SeqBlock, Statement, VariableType,
};
use crate::ir::ast::NumericalParameterRangeExclude;
use crate::ir::{
    BlockId, BranchId, DisciplineId, ExpressionId, ModuleId, NatureId, NetId, ParameterId, PortId,
    StatementId, VariableId,
};
use crate::symbol::Ident;
use crate::symbol_table::SymbolDeclaration;

#[allow(unused)]
pub trait Visitor<'ast, T = ()>: Sized {
    fn visit_declaration_name(&mut self, ident: &Ident, ast: &Ast<'ast>) -> T;
    fn visit_reference(&mut self, ident: &Ident, ast: &Ast<'ast>) -> T;
    fn visit_hierarchical_reference(&mut self, ident: &HierarchicalId, ast: &Ast<'ast>) -> T;
    fn visit_net_type(&mut self, net_type: NetType, ast: &Ast<'ast>) -> T;
    fn visit_variable_type(&mut self, variable_type: VariableType, ast: &Ast<'ast>) -> T;
    fn visit_module(&mut self, module: ModuleId<'ast>, ast: &Ast<'ast>) -> T;
    fn visit_statement(&mut self, statement: StatementId<'ast>, ast: &Ast<'ast>) -> T;
    fn visit_block(&mut self, block: BlockId<'ast>, ast: &Ast<'ast>) -> T;
    fn visit_condition(
        &mut self,
        condition: &AttributeNode<'ast, Condition<'ast>>,
        ast: &Ast<'ast>,
    ) -> T;
    fn visit_assign(
        &mut self,
        attributes: Attributes<'ast>,
        ident: &HierarchicalId,
        value: ExpressionId<'ast>,
        ast: &Ast<'ast>,
    ) -> T;
    fn visit_contribute(
        &mut self,
        attributes: Attributes<'ast>,
        nature: &Ident,
        branch: &Node<BranchAccess>,
        value: ExpressionId<'ast>,
        ast: &Ast<'ast>,
    ) -> T;

    fn visit_branch_access(
        &mut self,
        nature: &Ident,
        branch: &Node<BranchAccess>,
        ast: &Ast<'ast>,
    ) -> T;
    fn visit_branch(&mut self, branch: &Branch, ast: &Ast<'ast>) -> T;
    fn visit_expression(&mut self, expr: ExpressionId<'ast>, ast: &Ast<'ast>) -> T;
    fn visit_expression_primary(&mut self, primary: &Primary<'ast>, ast: &Ast<'ast>) -> T;

    fn visit_branch_declaration(
        &mut self,
        branch_declaration: BranchId<'ast>,
        ast: &Ast<'ast>,
    ) -> T;

    fn visit_port(&mut self, port: PortId<'ast>, ast: &Ast<'ast>) -> T;

    fn visit_net(&mut self, net: NetId<'ast>, ast: &Ast<'ast>) -> T;

    fn visit_variable(&mut self, variable: VariableId<'ast>, ast: &Ast<'ast>) -> T;
    fn visit_nature(&mut self, nature: NatureId<'ast>, ast: &Ast<'ast>) -> T;
    fn visit_discipline(&mut self, discipline: DisciplineId<'ast>, ast: &Ast<'ast>) -> T;
    fn visit_parameter(&mut self, parameter_id: ParameterId<'ast>, ast: &Ast<'ast>) -> T;
    fn visit_built_in_function_call(
        &mut self,
        function_call: BuiltInFunctionCall<'ast>,
        ast: &Ast<'ast>,
    ) -> T;
}

pub fn walk_module<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    module: &AttributeNode<'ast, Module<'ast>>,
    ast: &Ast<'ast>,
) {
    let module = &module.contents;
    for decl in module.symbol_table.values().copied() {
        match decl {
            SymbolDeclaration::Nature(_) => unreachable_unchecked!("Natures can't be declared inside nature so the parser won't ever place this here"),
            SymbolDeclaration::Module(_)=>unreachable_unchecked!("Module cant be declared inside modules so the parser won't ever place this here"),
            SymbolDeclaration::Discipline(_) => unreachable_unchecked!("Discipline can't be declared inside modules so the parser won't ever place this here"),
            SymbolDeclaration::Function(_) => unimplemented!("Functions"),
            SymbolDeclaration::Branch(branch) => {
                v.visit_branch_declaration(branch,ast);
            },
            SymbolDeclaration::Block(_) => (),//Blocks are visited specially as analog / digital blocks since not all blocks are inside the symbol table
            SymbolDeclaration::Port(portid) => {
                v.visit_port(portid,ast);
            },
            SymbolDeclaration::Net(netid) => {
                v.visit_net(netid,ast);
            },
            SymbolDeclaration::Variable(variableid) => {
                v.visit_variable(variableid,ast);
            },
            SymbolDeclaration::Parameter(parameter_id)=> {
                v.visit_parameter(parameter_id,ast);
            },
        };
    }
    //TODO parameters
    for module_item in module.children.iter() {
        match module_item {
            ModuleItem::AnalogStmt(statement) => v.visit_statement(*statement, ast),
            ModuleItem::GenerateStatement => unimplemented!("Generate Statement"),
        };
    }
}
pub fn walk_statement<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    statement: &Statement<'ast>,
    ast: &Ast<'ast>,
) -> T {
    match statement {
        Statement::Block(id) => v.visit_block(*id, ast),
        Statement::Condition(ref condition) => v.visit_condition(condition, ast),
        Statement::Assign(ref attr, ref ident, value) => v.visit_assign(*attr, ident, *value, ast),
        Statement::Contribute(ref attr, ref nature_name, ref branch, value) => {
            v.visit_contribute(*attr, nature_name, branch, *value, ast)
        }
        Statement::FunctionCall(ref _attr, ref _name, ref _args) => unimplemented!("Functions"),
        Statement::BuiltInFunctionCall(bifc) => v.visit_built_in_function_call(bifc.contents, ast),
    }
}
pub fn walk_block<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    block: &AttributeNode<'ast, SeqBlock<'ast>>,
    ast: &Ast<'ast>,
) {
    for statement in block.contents.statements.iter() {
        v.visit_statement(*statement, ast);
    }
}
pub fn walk_condition<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    condition: &AttributeNode<Condition<'ast>>,
    ast: &Ast<'ast>,
) {
    v.visit_expression(condition.contents.main_condition, ast);
    v.visit_statement(condition.contents.main_condition_statement, ast);
    for (condition, statement) in condition.contents.else_ifs.iter().copied() {
        v.visit_expression(condition, ast);
        v.visit_statement(statement, ast);
    }
    if let Some(statement) = condition.contents.else_statement {
        v.visit_statement(statement, ast);
    }
}
pub fn walk_assign<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    ident: &HierarchicalId,
    value: ExpressionId<'ast>,
    ast: &Ast<'ast>,
) {
    v.visit_hierarchical_reference(ident, ast);
    v.visit_expression(value, ast);
}
pub fn walk_branch_access<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    nature: &Ident,
    branch_access: &BranchAccess,
    ast: &Ast<'ast>,
) {
    v.visit_reference(nature, ast);
    match branch_access {
        BranchAccess::Explicit(ref ident) => v.visit_hierarchical_reference(ident, ast),
        BranchAccess::Implicit(ref branch) => v.visit_branch(branch, ast),
    };
}
pub fn walk_branch<'ast, T, V: Visitor<'ast, T>>(v: &mut V, branch: &Branch, ast: &Ast<'ast>) {
    match branch {
        Branch::Port(port) => {
            v.visit_hierarchical_reference(port, ast);
        }
        Branch::Nets(net1, net2) => {
            v.visit_hierarchical_reference(net1, ast);
            v.visit_hierarchical_reference(net2, ast);
        }
    }
}
pub fn walk_branch_declaration<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    branch_decl: &BranchDeclaration,
    ast: &Ast<'ast>,
) {
    v.visit_declaration_name(&branch_decl.name, ast);
    v.visit_branch(&branch_decl.branch, ast);
}
pub fn walk_contribute<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    nature: &Ident,
    branch: &Node<BranchAccess>,
    value: ExpressionId<'ast>,
    ast: &Ast<'ast>,
) {
    v.visit_branch_access(nature, branch, ast);
    v.visit_expression(value, ast);
}
pub fn walk_expression<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    expr: &Node<Expression<'ast>>,
    ast: &Ast<'ast>,
) {
    match expr.contents {
        Expression::BinaryOperator(lhs, _op, rhs) => {
            v.visit_expression(lhs, ast);
            v.visit_expression(rhs, ast);
        }
        Expression::UnaryOperator(_op, rhs) => {
            v.visit_expression(rhs, ast);
        }
        Expression::Primary(ref primary) => {
            v.visit_expression_primary(primary, ast);
        }
        Expression::Condtion(condition, _, if_val, _, else_val) => {
            v.visit_expression(condition, ast);
            v.visit_expression(if_val, ast);
            v.visit_expression(else_val, ast);
        }
    }
}
pub fn walk_expression_primary<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    primary: &Primary<'ast>,
    ast: &Ast<'ast>,
) {
    match *primary {
        Primary::BranchAccess(ref nature, ref branch_access) => {
            v.visit_branch_access(nature, branch_access, ast);
        }
        Primary::VariableOrNetReference(ref ident) => {
            v.visit_hierarchical_reference(ident, ast);
        }
        Primary::FunctionCall(ref ident, ref parameters) => {
            v.visit_hierarchical_reference(ident, ast);
            for parameter in parameters.iter().copied() {
                v.visit_expression(parameter, ast);
            }
        }
        Primary::BuiltInFunctionCall(built_in_function_call) => {
            v.visit_built_in_function_call(built_in_function_call, ast);
        }
        Primary::SystemFunctionCall(_) => {
            //todo args
        }
        Integer(_) | UnsignedInteger(_) | Real(_) => { /*Nothing to do*/ }
    }
}

pub fn walk_variable<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    variable: VariableId<'ast>,
    ast: &Ast<'ast>,
) {
    if let Some(default) = ast[variable].contents.default_value {
        v.visit_expression(default, ast);
    }
}
pub fn walk_parameter<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    parameter: ParameterId<'ast>,
    ast: &Ast<'ast>,
) {
    if let Some(default) = ast[parameter].contents.default_value {
        v.visit_expression(default, ast);
    }
    if let ParameterType::Numerical {
        parameter_type: _,
        included_ranges,
        excluded_ranges,
    } = &ast[parameter].contents.parameter_type
    {
        for range in included_ranges {
            v.visit_expression(range.start.bound, ast);
            v.visit_expression(range.end.bound, ast);
        }
        for exclude in excluded_ranges {
            match exclude {
                NumericalParameterRangeExclude::Value(val) => {
                    v.visit_expression(*val, ast);
                }
                NumericalParameterRangeExclude::Range(range) => {
                    v.visit_expression(range.start.bound, ast);
                    v.visit_expression(range.end.bound, ast);
                }
            }
        }
    } else {
        unimplemented!("String Parameters")
    }
}
pub fn walk_builtin_function_call<'ast, T, V: Visitor<'ast, T>>(
    v: &mut V,
    function_call: BuiltInFunctionCall<'ast>,
    ast: &Ast<'ast>,
) {
    match function_call {
        Pow(expr0, expr1)
        | Hypot(expr0, expr1)
        | Min(expr0, expr1)
        | Max(expr0, expr1)
        | ArcTan2(expr0, expr1) => {
            v.visit_expression(expr0, ast);
            v.visit_expression(expr1, ast);
        }
        Sqrt(expr) | Exp(expr) | Ln(expr) | Log(expr) | Abs(expr) | Floor(expr) | Ceil(expr)
        | Sin(expr) | Cos(expr) | Tan(expr) | ArcSin(expr) | ArcCos(expr) | ArcTan(expr)
        | SinH(expr) | CosH(expr) | TanH(expr) | ArcSinH(expr) | ArcCosH(expr) | ArcTanH(expr) => {
            v.visit_expression(expr, ast);
        }
    }
}
