//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::ProgramDependenceGraph;
use crate::data_structures::{BitSet, HybridBitSet, WorkQueue};
use crate::ir::hir::DisciplineAccess;
use crate::ir::{
    BranchId, IntegerExpressionId, NoiseSource, ParameterId, PortId, RealExpressionId, StatementId,
    StringExpressionId, SystemFunctionCall, VariableId,
};
use crate::mir::{ExpressionId, Mir, StringExpression};
use crate::mir::{IntegerExpression, RealExpression};
use crate::ControlFlowGraph;
use log::debug;
use std::collections::VecDeque;
use std::iter::FromIterator;

impl ControlFlowGraph {
    pub fn backward_variable_slice(&mut self, var: VariableId, pdg: &ProgramDependenceGraph) {
        self.backward_variable_slice_assuming_input(
            HybridBitSet::new_empty(StatementId::from_raw_unchecked(0)),
            var,
            pdg,
        )
    }

    pub fn backward_variable_slice_assuming_input(
        &mut self,
        input: HybridBitSet<StatementId>,
        var: VariableId,
        pdg: &ProgramDependenceGraph,
    ) {
        if let Some(relevant_stmts) = pdg.data_dependencies.assignments[var].clone() {
            self.backward_slice(relevant_stmts.into_dense(), input.into_dense(), pdg);
        } else {
            self.blocks.clear();
            self.statement_owner_cache
                .invalidate(pdg.data_dependencies.stmt_len_idx().index());
            self.predecessor_cache.invalidate();
        }
    }

    pub fn backward_variable_slice_with_variables_as_input(
        &mut self,
        input: impl Iterator<Item = VariableId>,
        var: VariableId,
        pdg: &ProgramDependenceGraph,
    ) {
        let mut input_statements = HybridBitSet::new_empty(pdg.data_dependencies.stmt_len_idx());
        for var in input {
            if let Some(assignments) = &pdg.data_dependencies.assignments[var] {
                input_statements.union_with(assignments)
            }
        }

        self.backward_variable_slice_assuming_input(input_statements, var, pdg)
    }

    pub fn backward_slice(
        &mut self,
        mut relevant_stmts: BitSet<StatementId>,
        assumed_stmts: BitSet<StatementId>,
        pdg: &ProgramDependenceGraph,
    ) {
        debug!(
            "Computing backwardslice wit intput {} and output {}",
            assumed_stmts, relevant_stmts
        );
        // The assumed stmts are marked as visited so they wont be inserted into the work queue
        let mut set = assumed_stmts;
        // The relevant stmts are added to the work queue
        set.grow(relevant_stmts.len_idx());
        let mut stmt_work_queue = WorkQueue {
            // Add all relevant stmts to the work queue
            deque: VecDeque::from_iter(relevant_stmts.ones()),
            set,
        };

        debug!(
            "Backwardslice: Inital stmt work-list: {:?}",
            stmt_work_queue
        );

        let mut bb_work_queue = WorkQueue::with_none(self.blocks.len_idx());

        loop {
            let mut done = true;

            if let Some(stmt) = stmt_work_queue.take() {
                relevant_stmts.insert(stmt);

                if let Some(data_dependencies) = &pdg.data_dependencies.stmt_use_def_chains[stmt] {
                    for data_dependency in data_dependencies.ones() {
                        stmt_work_queue.insert(data_dependency);
                    }
                }

                if let Some(control_dependencies) = &pdg.control_dependencies[self
                    .containing_block(stmt)
                    .expect("Mallformed cfg statement owners")]
                {
                    for control_dependency in control_dependencies.ones() {
                        bb_work_queue.insert(control_dependency);
                    }
                }

                debug!(
                    "Backwardslice: stmt work-list after iteration {:?}",
                    stmt_work_queue
                );

                done = false;
            }

            if let Some(bb) = bb_work_queue.take() {
                if let Some(data_dependencies) =
                    &pdg.data_dependencies.terminator_use_def_chains[bb]
                {
                    for data_dependency in data_dependencies.ones() {
                        stmt_work_queue.insert(data_dependency);
                    }
                }

                if let Some(control_dependencies) = &pdg.control_dependencies[bb] {
                    for control_dependency in control_dependencies.ones() {
                        bb_work_queue.insert(control_dependency);
                    }
                }

                done = false;
            }

            if done {
                break;
            }
        }

        for bb in self.blocks.iter_mut() {
            bb.statements.retain(|&stmt| relevant_stmts.contains(stmt))
        }
    }
}

impl Mir {
    pub fn track_expression(
        &self,
        expr: ExpressionId,
        dependency_handler: &mut impl DependencyHandler,
    ) {
        match expr {
            ExpressionId::Real(expr) => self.track_real_expression(expr, dependency_handler),
            ExpressionId::Integer(expr) => self.track_integer_expression(expr, dependency_handler),
            ExpressionId::String(expr) => self.track_string_expression(expr, dependency_handler),
        }
    }

    pub fn track_real_expression(
        &self,
        expr: RealExpressionId,
        dependency_handler: &mut impl DependencyHandler,
    ) {
        match self[expr].contents {
            RealExpression::Literal(_) => (),
            RealExpression::VariableReference(var) => {
                dependency_handler.handle_variable_reference(var);
            }
            RealExpression::ParameterReference(param) => {
                dependency_handler.handle_parameter_reference(param)
            }
            RealExpression::SimParam(name, default) => dependency_handler
                .handle_system_function_call(SystemFunctionCall::Simparam(name, default)),
            RealExpression::Temperature => {
                dependency_handler.handle_system_function_call(SystemFunctionCall::Temperature)
            }

            RealExpression::Noise(source, _) => match source {
                NoiseSource::White(expr) => self.track_real_expression(expr, dependency_handler),
                NoiseSource::Flicker(expr1, expr2) => {
                    self.track_real_expression(expr1, dependency_handler);
                    self.track_real_expression(expr2, dependency_handler);
                }
                NoiseSource::Table(_) | NoiseSource::TableLog(_) => todo!(),
            },

            RealExpression::BuiltInFunctionCall2p(_, arg1, arg2)
            | RealExpression::BinaryOperator(arg1, _, arg2) => {
                self.track_real_expression(arg1, dependency_handler);
                self.track_real_expression(arg2, dependency_handler);
            }

            RealExpression::Condition(cond, val1, val2) => {
                self.track_integer_expression(cond, dependency_handler);
                self.track_real_expression(val1, dependency_handler);
                self.track_real_expression(val2, dependency_handler);
            }

            RealExpression::BranchAccess(discipline, branch, order) => {
                dependency_handler.handle_branch_reference(discipline, branch, order)
            }

            RealExpression::Negate(_, expr) | RealExpression::BuiltInFunctionCall1p(_, expr) => {
                self.track_real_expression(expr, dependency_handler)
            }
            RealExpression::IntegerConversion(expr) => {
                self.track_integer_expression(expr, dependency_handler)
            }
        }
    }

    pub fn track_integer_expression(
        &self,
        expr: IntegerExpressionId,
        dependency_handler: &mut impl DependencyHandler,
    ) {
        match self[expr].contents {
            IntegerExpression::Literal(_) => (),

            IntegerExpression::VariableReference(var) => {
                dependency_handler.handle_variable_reference(var);
            }
            IntegerExpression::ParameterReference(param) => {
                dependency_handler.handle_parameter_reference(param)
            }

            IntegerExpression::NetReference(_) | IntegerExpression::PortReference(_) => {
                todo!("digital")
            }

            IntegerExpression::StringEq(arg1, arg2) | IntegerExpression::StringNEq(arg1, arg2) => {
                self.track_string_expression(arg1, dependency_handler);
                self.track_string_expression(arg2, dependency_handler);
            }

            IntegerExpression::RealComparison(arg1, _, arg2) => {
                self.track_real_expression(arg1, dependency_handler);
                self.track_real_expression(arg2, dependency_handler);
            }

            IntegerExpression::RealCast(expr) => {
                self.track_real_expression(expr, dependency_handler);
            }

            IntegerExpression::UnaryOperator(_, expr) | IntegerExpression::Abs(expr) => {
                self.track_integer_expression(expr, dependency_handler)
            }

            IntegerExpression::Max(arg1, arg2)
            | IntegerExpression::Min(arg1, arg2)
            | IntegerExpression::BinaryOperator(arg1, _, arg2)
            | IntegerExpression::IntegerComparison(arg1, _, arg2) => {
                self.track_integer_expression(arg1, dependency_handler);
                self.track_integer_expression(arg2, dependency_handler);
            }

            IntegerExpression::Condition(cond, val1, val2) => {
                self.track_integer_expression(cond, dependency_handler);
                self.track_integer_expression(val1, dependency_handler);
                self.track_integer_expression(val2, dependency_handler);
            }
            IntegerExpression::ParamGiven(param) => dependency_handler
                .handle_system_function_call(SystemFunctionCall::ParameterGiven(param)),
            IntegerExpression::PortConnected(port) => dependency_handler
                .handle_system_function_call(SystemFunctionCall::PortConnected(port)),
        }
    }

    pub fn track_string_expression(
        &self,
        expr: StringExpressionId,
        dependency_handler: &mut impl DependencyHandler,
    ) {
        match self[expr].contents {
            StringExpression::Literal(_) => (),
            StringExpression::VariableReference(var) => {
                dependency_handler.handle_variable_reference(var);
            }
            StringExpression::Condition(cond, val1, val2) => {
                self.track_integer_expression(cond, dependency_handler);
                self.track_string_expression(val1, dependency_handler);
                self.track_string_expression(val2, dependency_handler);
            }
            StringExpression::ParameterReference(param) => {
                dependency_handler.handle_parameter_reference(param)
            }
            StringExpression::SimParam(name) => dependency_handler
                .handle_system_function_call(SystemFunctionCall::SimparamStr(name)),
        }
    }
}

pub trait DependencyHandler {
    fn handle_variable_reference(&mut self, var: VariableId);
    fn handle_parameter_reference(&mut self, param: ParameterId);
    fn handle_branch_reference(&mut self, access: DisciplineAccess, branch: BranchId, order: u8);
    fn handle_system_function_call(
        &mut self,
        call: SystemFunctionCall<RealExpressionId, StringExpressionId, PortId, ParameterId>,
    );
}

impl DependencyHandler for () {
    fn handle_variable_reference(&mut self, _: VariableId) {}

    fn handle_parameter_reference(&mut self, _: ParameterId) {}

    fn handle_branch_reference(&mut self, _: DisciplineAccess, _: BranchId, _: u8) {}

    fn handle_system_function_call(
        &mut self,
        _: SystemFunctionCall<RealExpressionId, StringExpressionId, PortId, ParameterId>,
    ) {
    }
}
