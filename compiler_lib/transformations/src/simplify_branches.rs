/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::lints::InfiniteLoop;
use openvaf_diagnostics::lints::Linter;
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph, TerminatorKind};
use openvaf_middle::osdi_types::ConstVal::Scalar;
use openvaf_middle::osdi_types::SimpleConstVal::Bool;
use openvaf_middle::{impl_pass_span, CallType, Operand, OperandData, RValue};
use tracing::error;

pub struct SimplifyBranches;

impl<C: CallType> CfgPass<'_, C> for SimplifyBranches {
    type Result = ();

    fn run(self, cfg: &mut ControlFlowGraph<C>) {
        for (id, bb) in cfg.blocks.iter_mut_enumerated() {
            let terminator = bb.terminator_mut();
            match terminator.kind {
                TerminatorKind::Split {
                    condition:
                        RValue::Use(Operand {
                            contents: OperandData::Constant(Scalar(Bool(true))),
                            span,
                        }),
                    true_block,
                    false_block,
                    loop_head,
                } => {
                    if loop_head {
                        error!(
                            head = id.index(),
                            body = true_block.index(),
                            exit = false_block.index(),
                            "Infinite loop"
                        );
                        Linter::dispatch_late(Box::new(InfiniteLoop(span)), terminator.sctx);
                    } else {
                        terminator.kind = TerminatorKind::Goto(true_block)
                    }
                }

                TerminatorKind::Split {
                    condition:
                        RValue::Use(Operand {
                            contents: OperandData::Constant(Scalar(Bool(false))),
                            ..
                        }),
                    false_block,
                    ..
                } => terminator.kind = TerminatorKind::Goto(false_block),

                _ => (),
            }
        }
        cfg.predecessor_cache.invalidate()
    }

    impl_pass_span!("simplify_branches");
}
