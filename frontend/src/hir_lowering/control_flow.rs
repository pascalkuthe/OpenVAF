//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::cfg::{BasicBlock, BasicBlockId, Terminator};
use crate::hir::Block;
use crate::hir_lowering::error::Error::TypeMissmatch;
use crate::hir_lowering::error::MockType;
use crate::hir_lowering::expression_semantic::{InliningSchemanticAnalysis, SchematicAnalysis};
use crate::hir_lowering::HirToMirFold;
use crate::ir::mir::{ComparisonOperator, ExpressionId, IntegerBinaryOperator, Statement};
use crate::ir::{AttributeNode, Attributes, IntegerExpressionId, Node, VariableId};
use crate::mir::{IntegerExpression, VariableType};
use crate::{hir, ir, ControlFlowGraph, Span};
use index_vec::IndexVec;

impl<'lt> HirToMirFold<'lt> {
    /// Creates a controls flow Graph from a statement block
    pub fn fold_block_into_cfg(&mut self, statements: Block) -> ControlFlowGraph {
        let mut blocks = IndexVec::new();

        self.fold_block_internal(statements, Terminator::End, &mut blocks);
        for block in blocks.iter_mut() {
            block.statements.reverse()
        }
        let mut cfg = ControlFlowGraph::new(blocks, &self.mir);
        cfg.statement_owner_cache.stmt_count = self.mir.statements.len();
        cfg
    }

    fn fold_loop<F>(
        &mut self,
        statements: &mut Block,
        blocks: &mut IndexVec<BasicBlockId, BasicBlock>,
        exit: BasicBlockId,
        body: Block,
        condition: ir::ExpressionId,
        tail: F,
    ) -> BasicBlockId
    where
        F: FnOnce(&mut Self, BasicBlockId, &mut IndexVec<BasicBlockId, BasicBlock>) -> BasicBlockId,
    {
        let condition_block = blocks.push(BasicBlock {
            statements: Vec::new(),
            terminator: Terminator::Goto(exit), //just a placeholder
        });

        let mut inliner =
            InliningSchemanticAnalysis::new(blocks, condition_block, self.hir[condition].span);

        if let Some(condition) = inliner.fold_integer_expression(self, condition) {
            let loop_start = inliner.current_block;
            let tail_block = blocks.push(BasicBlock {
                statements: Vec::new(),
                terminator: Terminator::Goto(loop_start),
            });

            let tail_block = tail(self, tail_block, blocks);

            let loop_body = self.fold_block_internal(
                statements.enter_back(&body),
                Terminator::Goto(tail_block),
                blocks,
            );

            blocks[condition_block].terminator = Terminator::Split {
                condition,
                true_block: loop_body,
                false_block: exit,
                merge: condition_block,
            };

            blocks.push(BasicBlock {
                statements: Vec::new(),
                terminator: Terminator::Goto(loop_start),
            })
        } else {
            condition_block
        }
    }

    fn fold_assignment(
        &mut self,
        current_block: BasicBlockId,
        blocks: &mut IndexVec<BasicBlockId, BasicBlock>,
        attr: Attributes,
        dst: VariableId,
        val: ir::ExpressionId,
    ) -> BasicBlockId {
        match self.mir[dst].contents.variable_type {
            VariableType::Real(_) => {
                // Function calls can insert basic blocks
                let mut assignment_schematic =
                    InliningSchemanticAnalysis::new(blocks, current_block, self.hir[val].span);
                if let Some(value) = assignment_schematic.fold_real_expression(self, val) {
                    let stmt = self.mir.statements.push(Statement::Assignment(
                        attr,
                        dst,
                        ExpressionId::Real(value),
                    ));
                    // Innefficent but way cleaner than anything else
                    let res = assignment_schematic.current_block;
                    blocks[current_block].statements.push(stmt);
                    res
                } else {
                    current_block
                }
            }
            VariableType::Integer(_) => {
                let mut assignment_schematic =
                    InliningSchemanticAnalysis::new(blocks, current_block, self.hir[val].span);
                match assignment_schematic.fold_expression(self, val) {
                    Some(ExpressionId::Real(val)) => {
                        let value =
                            ExpressionId::Integer(self.mir.integer_expressions.push(Node {
                                span: self.mir[val].span,
                                contents: IntegerExpression::RealCast(val),
                            }));
                        let stmt = self
                            .mir
                            .statements
                            .push(Statement::Assignment(attr, dst, value));

                        let res = assignment_schematic.current_block;
                        blocks[current_block].statements.push(stmt);
                        res
                    }

                    Some(ExpressionId::Integer(val)) => {
                        let value = ExpressionId::Integer(val);
                        let stmt = self
                            .mir
                            .statements
                            .push(Statement::Assignment(attr, dst, value));

                        let res = assignment_schematic.current_block;
                        blocks[current_block].statements.push(stmt);
                        res
                    }

                    Some(ExpressionId::String(val)) => {
                        self.errors.add(TypeMissmatch {
                            span: self.mir[val].span,
                            expected_type: MockType::String,
                        });
                        current_block
                    }

                    None => current_block,
                }
            }
        }
    }

    pub(super) fn fold_block_internal(
        &mut self,
        mut statements: Block,
        terminator: Terminator,
        blocks: &mut IndexVec<BasicBlockId, BasicBlock>,
    ) -> BasicBlockId {
        let mut current_block = blocks.push(BasicBlock {
            statements: Vec::new(),
            terminator,
        });

        while let Some(statement) = statements.next_back() {
            match self.hir[statement] {
                hir::Statement::StopTask(kind, print) => {
                    let stmt = self.mir.statements.push(Statement::StopTask(kind, print));
                    blocks[current_block].statements.push(stmt);
                }

                hir::Statement::Case(ref case_node) => {
                    let start = blocks.push(BasicBlock {
                        statements: Vec::new(),
                        terminator: Terminator::Goto(current_block), //placeholder
                    });

                    let end = current_block;

                    let expr_span = self.hir[case_node.contents.expr].span;

                    let mut inline = InliningSchemanticAnalysis::new(blocks, start, expr_span);
                    let expr = inline.fold_expression(self, case_node.contents.expr);

                    current_block = inline.current_block;
                    let mut false_block = self.fold_block_internal(
                        statements.enter_back(&case_node.contents.default),
                        Terminator::Goto(end),
                        blocks,
                    );

                    for case in case_node.contents.cases.iter().rev() {
                        let true_block = self.fold_block_internal(
                            statements.enter_back(&case.contents.body),
                            Terminator::Goto(end),
                            blocks,
                        );
                        let cond_block = blocks.push(BasicBlock {
                            statements: vec![],
                            terminator: Terminator::Goto(end), //placeholder
                        });
                        let mut current = cond_block;
                        let mut cond = None;

                        //TODO take advantage of short circuit
                        for &val in &case.contents.values {
                            match expr {
                                Some(ExpressionId::Real(expr)) => {
                                    let mut inliner = InliningSchemanticAnalysis::new(
                                        blocks,
                                        current,
                                        self.hir[val].span,
                                    );

                                    if let Some(val) = self.fold_real_expression(val, &mut inliner)
                                    {
                                        let value_cond = Node {
                                            span: expr_span,
                                            contents: IntegerExpression::RealComparison(
                                                val,
                                                Node::new(
                                                    ComparisonOperator::LogicEqual,
                                                    expr_span,
                                                ),
                                                expr,
                                            ),
                                        };
                                        current = inliner.current_block;
                                        self.merge_conditions(&mut cond, value_cond, expr_span)
                                    }
                                }

                                Some(ExpressionId::Integer(expr)) => {
                                    let mut inliner = InliningSchemanticAnalysis::new(
                                        blocks,
                                        current,
                                        self.hir[val].span,
                                    );

                                    if let Some(val) =
                                        self.fold_integer_expression(val, &mut inliner)
                                    {
                                        let value_cond = Node {
                                            span: expr_span,
                                            contents: IntegerExpression::IntegerComparison(
                                                val,
                                                Node::new(
                                                    ComparisonOperator::LogicEqual,
                                                    expr_span,
                                                ),
                                                expr,
                                            ),
                                        };
                                        current = inliner.current_block;
                                        self.merge_conditions(&mut cond, value_cond, expr_span)
                                    }
                                }
                                Some(ExpressionId::String(expr)) => {
                                    let mut inline = InliningSchemanticAnalysis::new(
                                        blocks,
                                        current,
                                        self.hir[val].span,
                                    );

                                    if let Some(val) = self.fold_string_expression(val, &mut inline)
                                    {
                                        let value_cond = Node {
                                            span: expr_span,
                                            contents: IntegerExpression::StringEq(val, expr),
                                        };

                                        current = inline.current_block;
                                        self.merge_conditions(&mut cond, value_cond, expr_span)
                                    }
                                }

                                None => {
                                    let mut inliner = InliningSchemanticAnalysis::new(
                                        blocks,
                                        current,
                                        self.hir[val].span,
                                    );
                                    self.fold_expression(val, &mut inliner);
                                }
                            }
                        }

                        if let Some(condition) = cond {
                            blocks[cond_block].terminator = Terminator::Split {
                                condition,
                                true_block,
                                false_block,
                                merge: end,
                            };
                            false_block = current;
                        }
                    }

                    // Default is always folded last during ast lowering
                    statements.0.end = case_node.contents.default.0.end;
                }

                hir::Statement::Condition(ref condition) => {
                    let terminator = Terminator::Goto(current_block);

                    let false_block = self.fold_block_internal(
                        statements.enter_back(&condition.contents.else_statements),
                        terminator,
                        blocks,
                    );

                    let true_block = self.fold_block_internal(
                        statements.enter_back(&condition.contents.if_statements),
                        terminator,
                        blocks,
                    );

                    let merge = current_block;

                    current_block = blocks.push(BasicBlock {
                        statements: Vec::new(),
                        terminator, //Placeholder
                    });
                    let mut inliner = InliningSchemanticAnalysis::new(
                        blocks,
                        current_block,
                        self.hir[condition.contents.condition].span,
                    );
                    if let Some(condition) =
                        inliner.fold_integer_expression(self, condition.contents.condition)
                    {
                        let tmp = inliner.current_block;
                        blocks[current_block].terminator = Terminator::Split {
                            condition,
                            true_block,
                            false_block,
                            merge,
                        };
                        current_block = tmp;
                    }
                }

                hir::Statement::While(ref while_loop) => {
                    current_block = self.fold_loop(
                        &mut statements,
                        blocks,
                        current_block,
                        while_loop.contents.body.clone(),
                        while_loop.contents.condition,
                        |_, loop_tail, _| loop_tail,
                    );
                }

                hir::Statement::For(AttributeNode {
                    contents: ref for_loop,
                    attributes,
                    ..
                }) => {
                    current_block = self.fold_loop(
                        &mut statements,
                        blocks,
                        current_block,
                        for_loop.body.clone(),
                        for_loop.condition,
                        |fold, loop_tail, blocks| {
                            fold.fold_assignment(
                                loop_tail,
                                blocks,
                                attributes,
                                for_loop.increment_var,
                                for_loop.increment_expr,
                            )
                        },
                    );

                    current_block = self.fold_assignment(
                        current_block,
                        blocks,
                        attributes,
                        for_loop.initial_var,
                        for_loop.initial_expr,
                    )
                }

                hir::Statement::Assignment(attr, dst, val) => {
                    current_block = self.fold_assignment(current_block, blocks, attr, dst, val)
                }

                hir::Statement::Contribute(attr, access, branch, value) => {
                    let mut inliner = InliningSchemanticAnalysis::new(
                        blocks,
                        current_block,
                        self.hir[value].span,
                    );
                    if let Some(value) = inliner.fold_real_expression(self, value) {
                        current_block = inliner.current_block;
                        let stmt = self
                            .mir
                            .statements
                            .push(Statement::Contribute(attr, access, branch, value));
                        blocks[current_block].statements.push(stmt);
                    }
                }
            }
        }
        current_block
    }

    fn merge_conditions(
        &mut self,
        cond: &mut Option<IntegerExpressionId>,
        value_cond: Node<IntegerExpression>,
        expr_span: Span,
    ) {
        let value_cond = self.mir.integer_expressions.push(value_cond);
        if let Some(ref mut cond) = cond {
            let or_expr = Node {
                span: expr_span,
                contents: IntegerExpression::BinaryOperator(
                    *cond,
                    Node::new(IntegerBinaryOperator::LogicOr, expr_span),
                    value_cond,
                ),
            };

            let or_expr = self.mir.integer_expressions.push(or_expr);

            *cond = or_expr;
        } else {
            *cond = Some(value_cond);
        }
    }
}

/*
TODO reimplement implicit derivative check lint
struct ImplicitDerivativeCheck<'lt> {
    warnings: &'lt mut Vec<Warning>,
    modified_variables: HashSet<VariableId>,
    condition_span: Span,
}
impl<'lt> DependencyHandler for ImplicitDerivativeCheck<'lt> {
    fn handle_variable_reference(&mut self, var: VariableId) {
        if self.modified_variables.remove(&var) {
            self.warnings.push(Warning {
                error_type: WarningType::ImplicitDerivative(var),
                source: self.condition_span,
            })
        }
    }

    fn handle_parameter_reference(&mut self, _: ParameterId) {}

    fn handle_branch_reference(&mut self, _: DisciplineAccess, _: BranchId, _: u8) {}

    fn handle_system_function_call(
        &mut self,
        _: SystemFunctionCall<RealExpressionId, StringExpressionId, PortId, ParameterId>,
    ) {
    }
}
*/
