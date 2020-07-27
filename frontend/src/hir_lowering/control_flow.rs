//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::constant_fold::NoConstResolution;
use crate::cfg::{BasicBlock, BasicBlockId, ControlFlowGraph, Terminator};
use crate::hir::Block;
use crate::hir_lowering::error::Error::{IllegalFinishNumber, TypeMissmatch};
use crate::hir_lowering::error::MockType;
use crate::hir_lowering::expression_semantic::{
    ConstantSchematicAnalysis, InliningSchemanticAnalysis, SchematicAnalysis,
};
use crate::hir_lowering::HirToMirFold;
use crate::ir::ids::{IntegerExpressionId, VariableId};
use crate::ir::mir::{ComparisonOperator, ExpressionId, IntegerBinaryOperator, Statement};
use crate::ir::{Attributes, Node, PrintOnFinish, Spanned};
use crate::mir::{IntegerExpression, VariableType};
use crate::sourcemap::Span;
use crate::{hir, ir};
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
        cfg.statement_owner_cache.stmt_count = self.mir.statements().len();
        cfg
    }

    fn fold_loop<F>(
        &mut self,
        statements: &mut Block,
        blocks: &mut IndexVec<BasicBlockId, BasicBlock>,
        exit: BasicBlockId,
        body: Block,
        condition: ir::ids::ExpressionId,
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
        attributes: Attributes,
        span: Span,
        dst: VariableId,
        val: ir::ids::ExpressionId,
    ) -> BasicBlockId {
        match self.mir[dst].contents.variable_type {
            VariableType::Real(_) => {
                // Function calls can insert basic blocks
                let mut assignment_schematic =
                    InliningSchemanticAnalysis::new(blocks, current_block, span);
                if let Some(value) = assignment_schematic.fold_real_expression(self, val) {
                    let stmt = self.mir.add_new_stmt(Node {
                        attributes,
                        span,
                        contents: Statement::Assignment(dst, ExpressionId::Real(value)),
                    });

                    let res = assignment_schematic.current_block;
                    blocks[current_block].statements.push(stmt);
                    res
                } else {
                    current_block
                }
            }
            VariableType::Integer(_) => {
                let mut assignment_schematic =
                    InliningSchemanticAnalysis::new(blocks, current_block, span);

                match assignment_schematic.fold_expression(self, val) {
                    Some(ExpressionId::Real(val)) => {
                        let value =
                            ExpressionId::Integer(self.mir.integer_expressions.push(Spanned {
                                span: self.mir[val].span,
                                contents: IntegerExpression::RealCast(val),
                            }));
                        let stmt = self.mir.add_new_stmt(Node {
                            attributes,
                            span,
                            contents: Statement::Assignment(dst, value),
                        });

                        let res = assignment_schematic.current_block;
                        blocks[current_block].statements.push(stmt);
                        res
                    }

                    Some(ExpressionId::Integer(val)) => {
                        let value = ExpressionId::Integer(val);
                        let stmt = self.mir.add_new_stmt(Node {
                            attributes,
                            span,
                            contents: Statement::Assignment(dst, value),
                        });

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
            VariableType::String(_) => {
                let mut assignment_schematic =
                    InliningSchemanticAnalysis::new(blocks, current_block, span);
                if let Some(val) = assignment_schematic.fold_string_expression(self, val) {
                    let value = ExpressionId::String(val);
                    let stmt = self.mir.add_new_stmt(Node {
                        attributes,
                        span,
                        contents: Statement::Assignment(dst, value),
                    });

                    let res = assignment_schematic.current_block;
                    blocks[current_block].statements.push(stmt);
                    res
                } else {
                    current_block
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
            let attributes = self.hir[statement].attributes;
            // TODO fold attributes
            let span = self.hir[statement].span;

            match self.hir[statement].contents {
                hir::Statement::StopTask(kind, print) => {
                    let print = print
                        .and_then(|print| {
                            self.fold_integer_expression(print, &mut ConstantSchematicAnalysis)
                        })
                        .and_then(|expr| {
                            match self
                                .mir
                                .constant_fold_int_expr(expr, &mut NoConstResolution)
                                .expect("Should have been enforced during ast lowering")
                            {
                                0 => Some(PrintOnFinish::Nothing),
                                1 => Some(PrintOnFinish::Location),
                                2 => Some(PrintOnFinish::LocationAndResourceUsage),
                                illegal => {
                                    self.errors
                                        .add(IllegalFinishNumber(illegal, self.mir[expr].span));
                                    None
                                }
                            }
                        })
                        .unwrap_or(PrintOnFinish::Location);

                    let stmt = self.mir.add_new_stmt(Node {
                        attributes,
                        span,
                        contents: Statement::StopTask(kind, print),
                    });
                    blocks[current_block].statements.push(stmt);
                }

                hir::Statement::Case(ref cases) => {
                    let start = blocks.push(BasicBlock {
                        statements: Vec::new(),
                        terminator: Terminator::Goto(current_block), //placeholder
                    });

                    let end = current_block;

                    let expr_span = self.hir[cases.expr].span;

                    let mut inline = InliningSchemanticAnalysis::new(blocks, start, expr_span);
                    let expr = inline.fold_expression(self, cases.expr);

                    current_block = inline.current_block;
                    let mut false_block = self.fold_block_internal(
                        statements.enter_back(&cases.default),
                        Terminator::Goto(end),
                        blocks,
                    );

                    for case in cases.cases.iter().rev() {
                        let true_block = self.fold_block_internal(
                            statements.enter_back(&case.body),
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
                        for &val in &case.values {
                            match expr {
                                Some(ExpressionId::Real(expr)) => {
                                    let mut inliner = InliningSchemanticAnalysis::new(
                                        blocks,
                                        current,
                                        self.hir[val].span,
                                    );

                                    if let Some(val) = self.fold_real_expression(val, &mut inliner)
                                    {
                                        let value_cond = Spanned {
                                            span: expr_span,
                                            contents: IntegerExpression::RealComparison(
                                                val,
                                                Spanned::new(
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
                                        let value_cond = Spanned {
                                            span: expr_span,
                                            contents: IntegerExpression::IntegerComparison(
                                                val,
                                                Spanned::new(
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
                                        let value_cond = Spanned {
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
                    statements.0.end = cases.default.0.end;
                }

                hir::Statement::Condition(cond, ref true_block, ref false_block) => {
                    let terminator = Terminator::Goto(current_block);

                    let false_block = self.fold_block_internal(
                        statements.enter_back(false_block),
                        terminator,
                        blocks,
                    );

                    let true_block = self.fold_block_internal(
                        statements.enter_back(true_block),
                        terminator,
                        blocks,
                    );

                    let merge = current_block;

                    current_block = blocks.push(BasicBlock {
                        statements: Vec::new(),
                        terminator, //Placeholder
                    });
                    let mut inliner =
                        InliningSchemanticAnalysis::new(blocks, current_block, self.hir[cond].span);
                    if let Some(condition) = inliner.fold_integer_expression(self, cond) {
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

                hir::Statement::While(cond, ref body) => {
                    current_block = self.fold_loop(
                        &mut statements,
                        blocks,
                        current_block,
                        body.clone(),
                        cond,
                        |_, loop_tail, _| loop_tail,
                    );
                }

                hir::Statement::For(ref for_loop) => {
                    current_block = self.fold_loop(
                        &mut statements,
                        blocks,
                        current_block,
                        for_loop.body.clone(),
                        for_loop.cond,
                        |fold, loop_tail, blocks| {
                            fold.fold_assignment(
                                loop_tail,
                                blocks,
                                attributes,
                                span,
                                for_loop.incr.0,
                                for_loop.incr.1,
                            )
                        },
                    );

                    current_block = self.fold_assignment(
                        current_block,
                        blocks,
                        attributes,
                        span,
                        for_loop.init.0,
                        for_loop.init.1,
                    )
                }

                hir::Statement::Assignment(dst, val) => {
                    current_block =
                        self.fold_assignment(current_block, blocks, attributes, span, dst, val)
                }

                hir::Statement::Contribute(access, branch, value) => {
                    let mut inliner = InliningSchemanticAnalysis::new(
                        blocks,
                        current_block,
                        self.hir[value].span,
                    );
                    if let Some(value) = inliner.fold_real_expression(self, value) {
                        current_block = inliner.current_block;
                        let stmt = self.mir.add_new_stmt(Node {
                            attributes,
                            span,
                            contents: Statement::Contribute(access, branch, value),
                        });
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
        value_cond: Spanned<IntegerExpression>,
        expr_span: Span,
    ) {
        let value_cond = self.mir.integer_expressions.push(value_cond);
        if let Some(ref mut cond) = cond {
            let or_expr = Spanned {
                span: expr_span,
                contents: IntegerExpression::BinaryOperator(
                    *cond,
                    Spanned::new(IntegerBinaryOperator::LogicOr, expr_span),
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
