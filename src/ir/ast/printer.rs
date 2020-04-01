/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use pretty::{Arena, DocAllocator, DocBuilder, Pretty};

use crate::Ast;
use crate::ast::Expression;
use crate::compact_arena::SafeRange;
use crate::ir::{
    AttributeId, BlockId, BranchId, DisciplineId, ExpressionId, ModuleId, NatureId, NetId,
    ParameterId, PortId, StatementId, VariableId,
};
use crate::ir::ast::{
    AttributeNode, Branch, BranchAccess, BuiltInFunctionCall, Condition, HierarchicalId, NetType,
    Primary, VariableType, Visitor,
};
use crate::ir::ast::visitor::walk_statement;
use crate::symbol::Ident;

pub struct AstPrinter<'lt, 'lt, 'ast> {
    pub ast: &'lt Ast<'ast>,
    pub doc_allocator: &'lt Arena<'lt>,
    pub ident:usize,
}
impl<'lt, 'lt, 'ast> Visitor<'ast, DocBuilder<'lt, Arena<'lt>>>
    for AstPrinter<'lt, 'lt, 'ast>
{
    fn visit_declaration_name(
        &mut self,
        ident: &Ident,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt>, ()> {
        self.doc_allocator.text(ident.as_str().to_string())
    }

    fn visit_reference(
        &mut self,
        ident: &Ident,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt>, ()> {
        self.doc_allocator.text(ident.as_str().to_string())
    }

    fn visit_hierarchical_reference(
        &mut self,
        ident: &HierarchicalId,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        self.doc_allocator
            .intersperse(
                ident
                    .names
                    .iter()
                    .map(|ident| ident.name.as_str()),
                ".",
            )
            .group()
    }

    fn visit_net_type(
        &mut self,
        net_type: NetType,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        self.doc_allocator.text(format!("{:?}", net_type))
    }

    fn visit_variable_type(
        &mut self,
        variable_type: VariableType,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        self.doc_allocator.text(format!("{:?}", variable_type))
    }

    fn visit_module(
        &mut self,
        module: ModuleId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_statement(
        &mut self,
        statement: StatementId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        walk_statement(self, &ast[statement], ast)
    }

    fn visit_block(
        &mut self,
        block: BlockId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_condition(
        &mut self,
        condition: &AttributeNode<'ast, Condition<'ast>>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_assign(
        &mut self,
        attributes: SafeRange<AttributeId<'ast>>,
        ident: &HierarchicalId,
        value: ExpressionId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_contribute(
        &mut self,
        attributes: SafeRange<AttributeId<'ast>>,
        nature: &Ident,
        branch: &BranchAccess,
        value: ExpressionId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_branch_access(
        &mut self,
        nature: &Ident,
        branch: &BranchAccess,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_branch(
        &mut self,
        branch: &Branch,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_expression(
        &mut self,
        expr: ExpressionId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        match expr.contents {
            Expression::BinaryOperator(lhs, op, rhs) => {
                v.visit_expression(lhs, ast);
                v.visit_expression(rhs, ast);
            }
            Expression::UnaryOperator(op, rhs) => {
                let doc__builder = self.doc_allocator.text(format!("{:?}",op))
                doc__builder.hang(self.visit_expression(rhs, ast).nest(1))
            }
            Expression::Primary(ref primary) => {
                v.visit_expression_primary(primary, ast);
            }
        }
    }

    fn visit_expression_primary(
        &mut self,
        primary: &Primary<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_branch_declaration(
        &mut self,
        branch_declaration: BranchId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_port(
        &mut self,
        port: PortId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_net(
        &mut self,
        net: NetId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_variable(
        &mut self,
        variable: VariableId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_nature(
        &mut self,
        nature: NatureId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_discipline(
        &mut self,
        discipline: DisciplineId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_parameter(
        &mut self,
        parameter_id: ParameterId<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }

    fn visit_built_in_function_call(
        &mut self,
        function_call: &BuiltInFunctionCall<'ast>,
        ast: &Ast<'ast>,
    ) -> DocBuilder<'lt, Arena<'lt, ()>, ()> {
        unimplemented!()
    }
}
