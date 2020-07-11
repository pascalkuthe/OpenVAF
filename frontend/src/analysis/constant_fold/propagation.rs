//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

//! Constant folding and propagation along control flow
use crate::analysis::constant_fold::resolver::{
    ConstResolver, ConstantPropagator, NoConstResolution,
};
use crate::analysis::data_flow::reaching_definitions::UseDefGraph;
use crate::cfg::Terminator;
use crate::ir::{ParameterId, StatementId};
use crate::mir::{ExpressionId, Mir, Statement};
use crate::{ControlFlowGraph, HashMap, StringLiteral};
use log::debug;

/// This struct maps all variable assigments and parameters to their values that are already known during constant propagation
#[derive(Clone, Debug, Default)]
pub struct PropagatedConstants {
    pub real_definitions: HashMap<StatementId, f64>,
    pub integer_definitions: HashMap<StatementId, i64>,
    pub string_definitions: HashMap<StatementId, StringLiteral>,
    pub real_parameters: HashMap<ParameterId, f64>,
    pub int_parameters: HashMap<ParameterId, i64>,
    pub string_parameters: HashMap<ParameterId, StringLiteral>,
}

impl ControlFlowGraph {
    /// Walks `self` in reverse post order and [constant folds](crate::mir::Mir::constant_fold_real_expr)
    /// any encountered expressions. Variables are propagated along control flow if their assignments were constant folded.
    ///
    /// # Arguments
    ///
    /// * `mir` - The MIR belonging to this cfg. Statements and expressions in this mir may be constant folded
    ///
    /// * `udg` - The result of [reaching variable analysis][reaching]. Used to propgate constant variables.
    ///     Dependencies that are removed by inlining variables will be removed during propagation
    ///
    /// * `known_values` - Variable assigments and parameters that will be assumed as known at the start of the cfg
    ///
    ///
    /// # Note
    ///
    /// Constant propagation currently always modifies the `mir` as such calling `constant_propagation`
    /// on multiple overlapping cfgs is both inefficient and may lead to incorrect constant folds
    /// if different `known_values` were passed
    ///
    /// [reaching]: crate::analysis::data_flow::reaching_definitions

    pub fn constant_propagation(
        &mut self,
        mir: &mut Mir,
        udg: &mut UseDefGraph,
        known_values: &mut PropagatedConstants,
    ) {
        for (id, bb) in self.reverse_postorder_itermut() {
            mir.constant_fold_statements(&bb.statements, udg, known_values);

            if let Terminator::Split {
                condition,
                true_block,
                false_block,
                merge,
            } = bb.terminator
            {
                let folded_condition = if let Some(dependencies) =
                    &mut udg.terminator_use_def_chains[id]
                {
                    let mut resolver =
                        ConstantPropagator::new(&mut *known_values, dependencies, &udg.assignments);
                    let res = mir.constant_fold_int_expr(condition, &mut resolver);

                    if res.is_some() {
                        udg.terminator_use_def_chains[id] = None
                    }

                    res
                } else {
                    //we dont need to handle dependencies since there are none
                    mir.constant_fold_int_expr(condition, &mut NoConstResolution)
                };

                match folded_condition {
                    Some(0) => {
                        debug!(
                            "{:?}->(false: {:?}, true: {:?}) always false (condition: {:?})",
                            id, false_block, true_block, condition
                        );

                        bb.terminator = Terminator::Goto(false_block);
                    }

                    Some(_) => {
                        if merge == id {
                            panic!("Found constant infinite loop!")
                        }

                        bb.terminator = Terminator::Goto(true_block);
                    }

                    None => (),
                }
            }
        }
    }
}

impl Mir {
    /// See [`constant_fold_stmt`](crate::mir::Mir::constant_fold_stmt)
    fn constant_fold_statements(
        &mut self,
        statements: &[StatementId],
        udg: &mut UseDefGraph,
        known_values: &mut PropagatedConstants,
    ) {
        for &stmt in statements.iter().rev() {
            if let Some(dependencies) = &mut udg.stmt_use_def_chains[stmt] {
                let resolver =
                    ConstantPropagator::new(&mut *known_values, dependencies, &udg.assignments);

                if self.constant_fold_stmt(stmt, resolver, |resolver| resolver.known_values) {
                    udg.stmt_use_def_chains[stmt] = None
                }
            } else {
                //we dont need to handle dependencies since there are none
                self.constant_fold_stmt(stmt, NoConstResolution, |_| known_values);
            }
        }
    }

    /// [Constant folds](crate::mir::Mir::constant_fold_real_expr) the expressions in an statement
    /// and adds the resulting value to the `known_values` if `stmt` is an assignment
    ///
    /// # Returns
    /// whether the statement was constant folded successfully

    fn constant_fold_stmt<'lt, T: ConstResolver>(
        &mut self,
        stmt: StatementId,
        mut resolver: T,
        known_values: impl FnOnce(T) -> &'lt mut PropagatedConstants,
    ) -> bool {
        match self[stmt] {
            Statement::Assignment(_, _, val) => match val {
                ExpressionId::Real(val) => {
                    if let Some(val) = self.constant_fold_real_expr(val, &mut resolver) {
                        let old = known_values(resolver).real_definitions.insert(stmt, val);
                        #[cfg(debug_assertions)]
                        match old{
                            Some(new) if new != val => panic!(
                                "Statement {} was assigned twice with different values (old={},new={})!",
                                stmt,
                                old.unwrap(),
                                val
                            ),
                            _ => ()
                        }
                        return true;
                    }
                }

                ExpressionId::Integer(val) => {
                    if let Some(val) = self.constant_fold_int_expr(val, &mut resolver) {
                        let old = known_values(resolver).integer_definitions.insert(stmt, val);
                        #[cfg(debug_assertions)]
                        match old{
                            Some(new) if new != val => panic!(
                                "Statement {} was assigned twice with different values (old={},new={})!",
                                stmt,
                                old.unwrap(),
                                val
                            ),
                            _ => ()
                        }
                        return true;
                    }
                }
                ExpressionId::String(val) => {
                    if let Some(val) = self.constant_fold_str_expr(val, &mut resolver) {
                        let old = known_values(resolver).string_definitions.insert(stmt, val);
                        #[cfg(debug_assertions)]
                        match old{
                            Some(old) if old != val => panic!(
                                "Statement {} was assigned twice with different values (old={},new={})!",
                                stmt,
                                old,
                                val
                            ),
                            _ => ()
                        }
                        return true;
                    }
                }
            },

            Statement::Contribute(_, _, _, val) => {
                return self.constant_fold_real_expr(val, &mut resolver).is_some()
            }

            _ => (),
        }
        false
    }
}
