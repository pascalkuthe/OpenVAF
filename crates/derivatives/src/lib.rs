/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
*/

use crate::error::Error;
use enum_map::EnumMap;
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_diagnostics::MultiDiagnostic;
use openvaf_ir::ids::StatementId;
use openvaf_ir::Type;
use openvaf_middle::cfg::{ControlFlowGraph, PhiData};
use openvaf_middle::{
    COperand, CallType, CallTypeDerivative, Derivative, Mir, Operand, OperandData, RValue,
    Statement, StmntKind,
};
use std::mem::replace;

pub mod error;
pub mod lints;
mod rvalue;

pub fn generate_derivatives<C: CallType, MC: CallType>(
    cfg: &mut ControlFlowGraph<C>,
    mir: &Mir<MC>,
    errors: &mut MultiDiagnostic<Error>,
) {
    AutoDiff {
        cfg,
        mir,
        errors,
        forward_stmnts: IndexVec::with_capacity(128),
    }
    .run()
}

struct AutoDiff<'lt, C: CallType, MC: CallType> {
    cfg: &'lt mut ControlFlowGraph<C>,
    mir: &'lt Mir<MC>,
    errors: &'lt mut MultiDiagnostic<Error>,
    forward_stmnts: IndexVec<StatementId, Statement<C>>,
}

impl<'lt, C: CallType, MC: CallType> AutoDiff<'lt, C, MC> {
    pub fn run(mut self) {
        let postorder: Vec<_> = self.cfg.postorder_iter().map(|(id, _)| id).collect();
        let mut cache = EnumMap::new();

        for id in postorder {
            let new_stmts = IndexVec::with_capacity(2 * self.cfg[id].statements.len());
            let old_stmts = replace(&mut self.cfg[id].statements, new_stmts);

            for (stmnt, info) in old_stmts.into_iter().rev() {
                match stmnt {
                    StmntKind::Assignment(mut lhs, rhs) => {
                        let derivatives = self.cfg.derivatives.get(&lhs).cloned();

                        if let Some(derivatives) = derivatives {
                            if !derivatives.is_empty() {
                                let assignment_temporary =
                                    self.cfg.new_temporary(self.cfg.locals[lhs].ty);
                                let new_rhs = RValue::Use(Operand::new(
                                    OperandData::Copy(assignment_temporary),
                                    rhs.span(),
                                ));
                                self.cfg[id]
                                    .statements
                                    .push((StmntKind::Assignment(lhs, new_rhs), info));
                                lhs = assignment_temporary;
                                for (unkown, local) in derivatives {
                                    let rhs = self.rvalue_derivative(
                                        assignment_temporary,
                                        &rhs,
                                        unkown,
                                        info,
                                        &mut cache,
                                    );
                                    self.cfg[id]
                                        .statements
                                        .push((StmntKind::Assignment(local, rhs), info));
                                }
                                self.forward_stmnts.reverse();
                                self.cfg[id].statements.append(&mut self.forward_stmnts);
                            }
                        }

                        self.cfg[id]
                            .statements
                            .push((StmntKind::Assignment(lhs, rhs), info));
                    }

                    // No need to keep NoOps around
                    StmntKind::NoOp => (),

                    stmnt => {
                        self.cfg[id].statements.push((stmnt, info));
                    }
                }
                cache.clear();
            }
            self.cfg[id].statements.reverse();

            let phis = IndexVec::with_capacity(self.cfg[id].phi_statements.len());
            let mut phis = replace(&mut self.cfg[id].phi_statements, phis);

            for phi in &phis {
                for (unkown, dst) in self
                    .cfg
                    .derivatives
                    .get(&phi.dst)
                    .cloned()
                    .into_iter()
                    .flatten()
                {
                    let sources = phi
                        .sources
                        .iter()
                        .map(|(bb, local)| {
                            (*bb, self.cfg.demand_derivative_unchecked(*local, unkown))
                        })
                        .collect();

                    self.cfg[id].phi_statements.push(PhiData {
                        dst,
                        sources,
                        sctx: phi.sctx,
                    });
                }
            }

            self.cfg[id].phi_statements.append(&mut phis)
        }
    }
}

fn operand_to_derivative<C: CallType>(operand: COperand<C>) -> CallTypeDerivative<C> {
    Derivative::Operand(operand.contents)
}
