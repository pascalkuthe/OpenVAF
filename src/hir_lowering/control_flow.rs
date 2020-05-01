//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::{Constants, ExtractionDependencyHandler};
use crate::compact_arena::{invariant_lifetime, InvariantLifetime, TinyHeapArena};
use crate::hir::Block;
use crate::hir_lowering::error::{Error, Type, Warning, WarningType};
use crate::hir_lowering::HirToMirFold;
use crate::ir::hir::DisciplineAccess;
use crate::ir::mir::{ControlFlowGraph, ExpressionId, Statement};
use crate::ir::{BranchId, Node, ParameterId, StatementId, VariableId};
use crate::ir::{Push, SafeRangeCreation};
use crate::mir::control_flow_graph::{BasicBlock, BasicBlockId, Terminator};
use crate::mir::{IntegerExpression, VariableType};
use crate::symbol::Ident;
use crate::{hir, Span};
use rustc_hash::FxHashSet;

impl<'tag, 'lt> HirToMirFold<'tag, 'lt> {
    /// Creates a controls flow Graph from a statement block
    pub fn fold_block_into_cfg<'cfg>(
        &mut self,
        statements: Block<'tag>,
    ) -> ControlFlowGraph<'tag, 'tag> {
        let mut allocator = unsafe { TinyHeapArena::new(invariant_lifetime(), 512) };
        self.fold_block_internal(statements, Terminator::End, &mut allocator);
        let mut res = ControlFlowGraph { blocks: allocator };
        unsafe {
            res.constant_fold(
                &mut self.mir,
                &mut Constants::default(),
                true,
                invariant_lifetime(),
            )
        }
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
                    let terminator = Terminator::Merge(current_block);

                    let mut derived_inside_condition = FxHashSet::default();

                    let mut merge_statements = Vec::with_capacity(16);
                    let derivatives_before_branch = self.variable_to_differentiate.clone();

                    let false_block = self.fold_block_internal(
                        statements.enter_back(condition.contents.else_statement),
                        terminator,
                        allocator,
                    );
                    let mir = &mut self.mir;
                    Self::merge_branched_derivatives(
                        &mut self.variable_to_differentiate,
                        derivatives_before_branch,
                        |expected_after_branch, expected_inside_branch| {
                            merge_statements.push(mir.generate_derivative_alias(
                                expected_after_branch,
                                expected_inside_branch,
                            ))
                        },
                        |derived| {
                            derived_inside_condition.insert(derived);
                        },
                    );

                    let derivatives_before_branch = self.variable_to_differentiate.clone();
                    let true_block = self.fold_block_internal(
                        statements.enter_back(condition.contents.if_statements),
                        terminator,
                        allocator,
                    );

                    let mir = &mut self.mir;
                    Self::merge_branched_derivatives(
                        &mut self.variable_to_differentiate,
                        derivatives_before_branch,
                        |expected_after_branch, expected_inside_branch| {
                            merge_statements.push(mir.generate_derivative_alias(
                                expected_after_branch,
                                expected_inside_branch,
                            ))
                        },
                        |derived| {
                            derived_inside_condition.insert(derived);
                        },
                    );

                    let terminator = if let Some(condition) =
                        self.fold_integer_expression(condition.contents.condition)
                    {
                        self.mir.track_integer_expression(
                            condition,
                            &mut FxHashSet::default(),
                            &mut ImplicitDerivativeCheck {
                                warnings: &mut self.warnings,
                                condition_span: self.mir[condition].source,
                                modified_variables: derived_inside_condition,
                            },
                        );

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
                        statements: merge_statements,
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

                    let mut derived_inside_loop = FxHashSet::default();
                    let mut derivatives_before_loop = self.variable_to_differentiate.clone();

                    let loop_body = self.fold_block_internal(
                        statements.enter_back(while_loop.contents.body),
                        Terminator::Merge(condition_block),
                        allocator,
                    );

                    let mut merge_statements = Vec::with_capacity(16);

                    let mir = &mut self.mir;
                    Self::merge_branched_derivatives(
                        &mut self.variable_to_differentiate,
                        derivatives_before_loop,
                        |after_loop, expected_inside_loop| {
                            allocator[loop_body].statements.push(
                                mir.generate_derivative_alias(expected_inside_loop, after_loop),
                            );
                            merge_statements.push(
                                mir.generate_derivative_alias(after_loop, expected_inside_loop),
                            )
                        },
                        |derived| {
                            derived_inside_loop.insert(derived);
                        },
                    );

                    if let Some(condition) =
                        self.fold_integer_expression(while_loop.contents.condition)
                    {
                        self.mir.track_integer_expression(
                            condition,
                            &mut FxHashSet::default(),
                            &mut ImplicitDerivativeCheck {
                                warnings: &mut self.warnings,
                                condition_span: self.mir[condition].source,
                                modified_variables: derived_inside_loop,
                            },
                        );

                        allocator[condition_block].terminator = Terminator::Split {
                            condition,
                            true_block: loop_body,
                            false_block: current_block,
                            merge: condition_block,
                        };
                        current_block = allocator.add(BasicBlock {
                            statements: merge_statements,
                            terminator: Terminator::Goto(condition_block),
                        });
                    } else {
                        current_block = condition_block;
                    }

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
                    let partial_derivatives = self.variable_to_differentiate.remove(&dst);

                    if let Some(value) = self.fold_real_expression(val) {
                        let stmt = self.mir.push(Statement::Assignment(
                            attr,
                            dst,
                            ExpressionId::Real(value),
                        ));

                        allocator[current_block].statements.push(stmt);

                        if let Some(partial_derivatives) = partial_derivatives {
                            self.generate_partial_derivative_assignment(
                                partial_derivatives,
                                attr,
                                ExpressionId::Real(value),
                                &mut allocator[current_block].statements,
                            );
                        }
                    }
                }

                hir::Statement::Assignment(attr, dst, value) => {
                    let partial_derivatives = self.variable_to_differentiate.remove(&dst);

                    let value = match self.fold_expression(value) {
                        Some(ExpressionId::Real(val)) => {
                            let value = ExpressionId::Integer(self.mir.push(Node {
                                source: self.mir[val].source,
                                contents: IntegerExpression::RealCast(val),
                            }));

                            let stmt = self.mir.push(Statement::Assignment(attr, dst, value));

                            allocator[current_block].statements.push(stmt);

                            value
                        }

                        Some(ExpressionId::Integer(val)) => {
                            let value = ExpressionId::Integer(val);
                            let stmt = self.mir.push(Statement::Assignment(attr, dst, value));
                            allocator[current_block].statements.push(stmt);
                            value
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

                    if let Some(partial_derivatives) = partial_derivatives {
                        self.generate_partial_derivative_assignment(
                            partial_derivatives,
                            attr,
                            value,
                            &mut allocator[current_block].statements,
                        );
                    }
                }

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
struct ImplicitDerivativeCheck<'tag, 'lt> {
    warnings: &'lt mut Vec<Warning<'tag>>,
    modified_variables: FxHashSet<VariableId<'tag>>,
    condition_span: Span,
}
impl<'tag, 'lt> ExtractionDependencyHandler<'tag> for ImplicitDerivativeCheck<'tag, 'lt> {
    fn handle_variable_reference(&mut self, var: VariableId<'tag>) {
        if self.modified_variables.remove(&var) {
            self.warnings.push(Warning {
                error_type: WarningType::ImplicitDerivative(var),
                source: self.condition_span,
            })
        }
    }

    fn handle_parameter_reference(&mut self, _: ParameterId<'tag>) {}

    fn handle_branch_reference(&mut self, _: DisciplineAccess, _: BranchId<'tag>) {}

    fn handle_system_function_call(&mut self, _: Ident) {}
}
