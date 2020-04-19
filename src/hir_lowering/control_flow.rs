//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::compact_arena::{invariant_lifetime, InvariantLifetime, TinyHeapArena};
use crate::hir::Block;
use crate::hir_lowering::error::{Error, Type};
use crate::hir_lowering::HirToMirFold;
use crate::ir::mir::{ControlFlowGraph, ExpressionId, Statement};
use crate::ir::{ModuleId, Node, RealExpressionId};
use crate::ir::{Push, SafeRangeCreation};
use crate::mir::control_flow_graph::{BasicBlock, BasicBlockId, Terminator};
use crate::mir::{IntegerExpression, VariableType};
use crate::{hir, Hir};

impl<'tag, 'lt> HirToMirFold<'tag, 'lt> {
    /// Creates a controls flow Graph from a statement block
    pub fn fold_block_into_cfg<'cfg>(
        &mut self,
        statements: Block<'tag>,
    ) -> ControlFlowGraph<'tag, 'tag> {
        let mut allocator = unsafe { TinyHeapArena::new(invariant_lifetime(), 512) };
        self.fold_block_internal(statements, Terminator::End, &mut allocator);
        ControlFlowGraph { blocks: allocator }
    }

    fn fold_block_internal(
        &mut self,
        mut statements: Block<'tag>,
        terminator: Terminator<'tag, 'tag>,
        allocator: &mut TinyHeapArena<'tag, BasicBlock<'tag, 'tag>>,
    ) -> BasicBlockId<'tag> {
        let mut current_block = allocator.add(BasicBlock {
            statements: Vec::new(),
            terminator,
        });

        while let Some(statement) = statements.next_back() {
            match self.hir[statement] {
                hir::Statement::Condition(ref condition) => {
                    allocator[current_block].statements.reverse();
                    let terminator = Terminator::Goto(current_block);
                    let false_block = self.fold_block_internal(
                        statements.enter_back(condition.contents.else_statement),
                        terminator,
                        allocator,
                    );
                    let true_block = self.fold_block_internal(
                        statements.enter_back(condition.contents.if_statements),
                        terminator,
                        allocator,
                    );

                    let terminator = if let Some(condition) =
                        self.fold_integer_expression(condition.contents.condition)
                    {
                        Terminator::Split {
                            condition,
                            true_block,
                            false_block,
                            merge: current_block,
                        }
                    } else {
                        Terminator::Goto(current_block)
                    };

                    current_block = allocator.add(BasicBlock {
                        statements: Vec::new(),
                        terminator,
                    });

                    statements.skip_backward(1); //skip start
                }

                hir::Statement::While(while_loop) => {
                    allocator[current_block].statements.reverse();

                    let condition_block = allocator.add(BasicBlock {
                        statements: Vec::new(),
                        terminator: Terminator::Goto(current_block), //just a placeholder
                    });

                    let loop_body = self.fold_block_internal(
                        statements.enter_back(while_loop.contents.body),
                        Terminator::Goto(condition_block),
                        allocator,
                    );

                    if let Some(condition) =
                        self.fold_integer_expression(while_loop.contents.condition)
                    {
                        allocator[condition_block].terminator = Terminator::Split {
                            condition,
                            true_block: loop_body,
                            false_block: current_block,
                            merge: condition_block,
                        };
                    }
                    current_block = condition_block;

                    statements.skip_backward(1); //skip start
                }

                hir::Statement::WhileStart {
                    while_info_and_start,
                } => unreachable_unchecked!("Should have been skipped"),

                hir::Statement::ConditionStart {
                    condition_info_and_end,
                } => unreachable_unchecked!("Should have been skipped"),

                hir::Statement::Assignment(attr, dst, val)
                    if matches!(self.mir[dst].contents.variable_type, VariableType::Real(..)) =>
                {
                    if let Some(value) = self.fold_real_expression(val) {
                        let stmt = self.mir.push(Statement::Assignment(
                            attr,
                            dst,
                            ExpressionId::Real(value),
                        ));
                        allocator[current_block].statements.push(stmt);
                    }
                }

                hir::Statement::Assignment(attr, dst, value) => match self.fold_expression(value) {
                    Some(ExpressionId::Real(val)) => {
                        let expr = self.mir.push(Node {
                            source: self.mir[val].source,
                            contents: IntegerExpression::RealCast(val),
                        });

                        let stmt = self.mir.push(Statement::Assignment(
                            attr,
                            dst,
                            ExpressionId::Integer(expr),
                        ));
                        allocator[current_block].statements.push(stmt);
                    }

                    Some(ExpressionId::Integer(val)) => {
                        let stmt = self.mir.push(Statement::Assignment(
                            attr,
                            dst,
                            ExpressionId::Integer(val),
                        ));
                        allocator[current_block].statements.push(stmt);
                    }

                    Some(ExpressionId::String(val)) => {
                        self.errors.push(Error {
                            error_type: Type::ExpectedNumber,
                            source: self.mir[val].source,
                        });
                    }
                    None => {}
                },

                hir::Statement::Contribute(attr, access, branch, value) => {
                    if let Some(value) = self.fold_real_expression(value) {
                        let stmt = self
                            .mir
                            .push(Statement::Contribute(attr, access, branch, value));
                        allocator[current_block].statements.push(stmt);
                    }
                }
                hir::Statement::FunctionCall(_, _, _) => todo!("Function Calls"),
            }
        }
        allocator[current_block].statements.reverse();
        current_block
    }
}
