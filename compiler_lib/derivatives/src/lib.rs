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
use openvaf_middle::cfg::{ControlFlowGraph, PhiData};
use openvaf_middle::{
    COperand, CallType, CallTypeDerivative, Derivative, Local, Mir, Operand, OperandData, RValue,
    Statement, StmntKind, SyntaxCtx,
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
    fn append_assignment_and_derivatives(
        &mut self,
        lhs: Local,
        mut rhs: RValue<C>,
        sctx: SyntaxCtx,
    ) {
        let derivatives = self.cfg.derivatives.get(&lhs).cloned();

        if let Some(derivatives) = derivatives {
            if !derivatives.is_empty() {
                let mut cache = EnumMap::new();
                let new_value = self.cfg.new_temporary(self.cfg.locals[lhs].ty);
                let new_rhs = RValue::Use(Operand::new(OperandData::Copy(new_value), rhs.span()));
                self.forward_stmnts
                    .push((StmntKind::Assignment(new_value, rhs.clone()), sctx));
                for (unkown, derivative_local) in derivatives {
                    let derivative_rhs =
                        self.rvalue_derivative(new_value, &rhs, unkown, sctx, &mut cache);
                    self.append_assignment_and_derivatives(derivative_local, derivative_rhs, sctx)
                }
                rhs = new_rhs;
            }
        }

        self.forward_stmnts
            .push((StmntKind::Assignment(lhs, rhs), sctx));
    }

    pub fn run(mut self) {
        let postorder: Vec<_> = self.cfg.postorder_iter().map(|(id, _)| id).collect();
        for id in postorder {
            let new_stmts = IndexVec::with_capacity(2 * self.cfg[id].statements.len());
            let old_stmts = replace(&mut self.cfg[id].statements, new_stmts);

            for (stmnt, sctx) in old_stmts.into_iter().rev() {
                match stmnt {
                    StmntKind::Assignment(lhs, rhs) => {
                        self.append_assignment_and_derivatives(lhs, rhs, sctx);
                        self.forward_stmnts.reverse();
                        self.cfg[id].statements.append(&mut self.forward_stmnts);
                    }

                    // No need to keep NoOps around
                    StmntKind::NoOp => (),

                    stmnt => {
                        self.cfg[id].statements.push((stmnt, sctx));
                    }
                }
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
