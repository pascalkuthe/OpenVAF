//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::DependencyHandler;

use crate::cfg::{BasicBlock, BasicBlockId, Terminator};
use crate::hir::Block;
use crate::hir_lowering::error::{Error, Type, Warning, WarningType};
use crate::hir_lowering::expression_semantic::{
    AssignmentSchematicAnalysis, FunctionInline, SchematicAnalysis,
};
use crate::hir_lowering::HirToMirFold;
use crate::ir::hir::DisciplineAccess;
use crate::ir::mir::{ExpressionId, Statement};
use crate::ir::{BranchId, Node, ParameterId, VariableId};
use crate::ir::{PortId, RealExpressionId, StringExpressionId, SystemFunctionCall};
use crate::mir::{IntegerExpression, VariableType};
use crate::{hir, ControlFlowGraph, Span};
use index_vec::{IndexVec};
use rustc_hash::FxHashSet;

impl<'lt> HirToMirFold<'lt> {
    /// Creates a controls flow Graph from a statement block
    pub fn fold_block_into_cfg(&mut self, statements: Block) -> ControlFlowGraph {
        let mut blocks = IndexVec::new();

        self.fold_block_internal(statements, Terminator::End, &mut blocks);
        for block in blocks.iter_mut(){
            block.statements.reverse()
        }
        let mut cfg = ControlFlowGraph::new(blocks, &self.mir);

        self.generate_derivatives(&mut cfg);
        cfg.statement_owner_cache.stmt_count = self.mir.statements.len();
        cfg
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
                    let mut inliner = FunctionInline::new(
                        blocks,
                        current_block,
                        self.hir[condition.contents.condition].source,
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

                    statements.next_back(); //skip start
                }

                hir::Statement::While(ref while_loop) => {

                    let condition_block = blocks.push(BasicBlock {
                        statements: Vec::new(),
                        terminator: Terminator::Goto(current_block), //just a placeholder
                    });

                    let mut inliner = FunctionInline::new(
                        blocks,
                        condition_block,
                        self.hir[while_loop.contents.condition].source,
                    );

                    if let Some(condition) =
                        inliner.fold_integer_expression(self, while_loop.contents.condition)
                    {
                        let false_block = current_block;

                        let (loop_start, loop_body) = if condition_block == inliner.current_block {
                            let loop_body = self.fold_block_internal(
                                statements.enter_back(&while_loop.contents.body),
                                Terminator::Goto(condition_block),
                                blocks,
                            );
                            (condition_block, loop_body)
                        } else {
                            let loop_start = inliner.current_block;
                            let loop_body = self.fold_block_internal(
                                statements.enter_back(&while_loop.contents.body),
                                Terminator::Goto(loop_start),
                                blocks,
                            );
                            (loop_start, loop_body)
                        };

                        blocks[condition_block].terminator = Terminator::Split {
                            condition,
                            true_block: loop_body,
                            false_block,
                            merge: condition_block,
                        };

                        current_block = blocks.push(BasicBlock {
                            statements: Vec::new(),
                            terminator: Terminator::Goto(loop_start),
                        });
                    /*for attr in while_loop.attributes {
                        if self.mir[attr].name.name == keywords::IMPLICIT_SOLVER {
                            if let Some(name) = self.mir[attr].value {
                                if let ExpressionId::String(name) = name {
                                    if let Some(name) = self.mir.try_string_constant_fold(
                                        name,
                                        &Constants::default(),
                                        true,
                                    ) {
                                        let ident =
                                            Symbol::intern(&self.mir.string_literals[name]);
                                        condition = self.or_for_derivative(
                                            condition,
                                            ident,
                                            &derived_inside_loop,
                                        );
                                        derived_inside_loop.clear();
                                    } else {
                                        self.errors.push(Error {
                                            error_type:
                                                Type::ImplicitSolverDeltaIsNotAValidString,
                                            source: self.mir[name].source,
                                        });
                                    }
                                } else {
                                    self.errors.push(Error {
                                        error_type: Type::ImplicitSolverDeltaIsNotAValidString,
                                        source: name.source(&self.mir),
                                    });
                                }
                            } else {
                                self.errors.push(Error {
                                    error_type: Type::ImplicitSolverDeltaIsNotAValidString,
                                    source: self.mir[attr].name.span,
                                });
                            }

                            break;
                        }
                    }

                    if !derived_inside_loop.is_empty() {
                        self.mir.track_integer_expression(
                            condition,
                            &mut FxHashSet::default(),
                            &mut ImplicitDerivativeCheck {
                                warnings: &mut self.warnings,
                                condition_span: self.mir[condition].source,
                                modified_variables: derived_inside_loop,
                            },
                        );
                    }*/
                    } else {
                        current_block = condition_block;
                    }

                    statements.next_back(); //skip start
                }

                hir::Statement::WhileStart { .. } => {
                    unreachable_unchecked!("Should have been skipped")
                }

                hir::Statement::ConditionStart { .. } => {
                    unreachable_unchecked!("Should have been skipped")
                }

                hir::Statement::Assignment(attr, dst, val)
                    if matches!(self.mir[dst].contents.variable_type, VariableType::Real(..)) =>
                {
                    // Function calls can insert basic blocks
                    let tmp_block = current_block;
                    let mut assignment_schematic = AssignmentSchematicAnalysis::new(
                        dst,
                        self.hir[val].source,
                        blocks,
                        current_block,
                    );
                    if let Some(value) = assignment_schematic.fold_real_expression(self, val) {
                        let stmt = self.mir.statements.push(Statement::Assignment(
                            attr,
                            dst,
                            ExpressionId::Real(value),
                        ));
                        // Innefficent but way cleaner than anything else
                        current_block = assignment_schematic.inliner.current_block;
                        blocks[tmp_block].statements.push( stmt);
                    }
                }

                hir::Statement::Assignment(attr, dst, value) => {
                    let tmp_block = current_block;
                    let mut assignment_schematic = AssignmentSchematicAnalysis::new(
                        dst,
                        self.hir[value].source,
                        blocks,
                        current_block,
                    );
                    match assignment_schematic.fold_expression(self, value) {
                        Some(ExpressionId::Real(val)) => {
                            let value =
                                ExpressionId::Integer(self.mir.integer_expressions.push(Node {
                                    source: self.mir[val].source,
                                    contents: IntegerExpression::RealCast(val),
                                }));
                            let stmt = self
                                .mir
                                .statements
                                .push(Statement::Assignment(attr, dst, value));
                            // Innefficent but way cleaner than anything else
                            current_block = assignment_schematic.inliner.current_block;
                            blocks[tmp_block].statements.push( stmt);
                        }

                        Some(ExpressionId::Integer(val)) => {
                            let value = ExpressionId::Integer(val);
                            let stmt = self
                                .mir
                                .statements
                                .push(Statement::Assignment(attr, dst, value));
                            // Innefficent but way cleaner than anything else
                            current_block = assignment_schematic.inliner.current_block;
                            blocks[tmp_block].statements.push( stmt);
                        }

                        Some(ExpressionId::String(val)) => {
                            self.errors.push(Error {
                                error_type: Type::ExpectedNumber,
                                source: self.mir[val].source,
                            });
                            continue;
                        }

                        None => continue,
                    };
                }

                hir::Statement::Contribute(attr, access, branch, value) => {
                    let mut inliner =
                        FunctionInline::new(blocks, current_block, self.hir[value].source);
                    if let Some(value) = inliner.fold_real_expression(self, value) {
                        current_block = inliner.current_block;
                        let stmt = self
                            .mir
                            .statements
                            .push(Statement::Contribute(attr, access, branch, value));
                        blocks[current_block].statements.push(stmt);
                    }
                }
                hir::Statement::FunctionCall(_, _, _) => todo!("Function Calls"),
            }
        }
        current_block
    }
}
struct ImplicitDerivativeCheck<'lt> {
    warnings: &'lt mut Vec<Warning>,
    modified_variables: FxHashSet<VariableId>,
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
