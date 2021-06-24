/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::post_dominance::PostDominators;
use crate::BuildPostDominators;
use openvaf_data_structures::bit_set::SparseBitMatrix;
use openvaf_middle::cfg::{AnalysisPass, BasicBlock, ControlFlowGraph, TerminatorKind};
use openvaf_middle::{impl_pass_span, CallType};
use std::borrow::Borrow;

pub struct ControlDependenceGraph(pub SparseBitMatrix<BasicBlock, BasicBlock>);
pub struct InvControlDependenceGraph(pub SparseBitMatrix<BasicBlock, BasicBlock>);

impl ControlDependenceGraph {
    pub fn inverse(&self) -> InvControlDependenceGraph {
        InvControlDependenceGraph(self.0.inverse())
    }
}

/// Calculates the Control Dependence Graph of a CFG.
/// A basic block is control dependent on a statement if this statements decides whether the block is executed (for example an if statement).
/// Statements that cause control flow are not represented as statements but as basic block terminators in the cfg.
/// As such the control dependence Graph simply maps basic blocks to all basics block whose terminator affects whether a statements is executed
///
///# Note
/// This analysis pass allows to manually pass the `PostDominators` so that the post dominators can be reused
/// If you dont want to reuse `ipdom` you should can simply pass `BuildPostDominators`
pub struct BuildControlDependenceGraph<I>(pub I);

impl Default for BuildControlDependenceGraph<BuildPostDominators> {
    fn default() -> Self {
        Self(BuildPostDominators)
    }
}

impl<C: CallType> AnalysisPass<'_, C> for BuildControlDependenceGraph<BuildPostDominators> {
    type Result = ControlDependenceGraph;
    impl_pass_span!("Build ControlDependence");
    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let post_dominators = cfg.analyse(BuildPostDominators);
        cfg.analyse(BuildControlDependenceGraph(post_dominators))
    }
}

impl<C: CallType, I: Borrow<PostDominators>> AnalysisPass<'_, C>
    for BuildControlDependenceGraph<I>
{
    type Result = ControlDependenceGraph;
    impl_pass_span!("Build ControlDependence");
    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let ipdom = self.0.borrow();

        let mut cdg =
            ControlDependenceGraph(SparseBitMatrix::new(cfg.blocks.len(), cfg.blocks.len()));

        for (id, bb) in cfg.blocks.iter_enumerated() {
            // we only care about control dependencies on branches since end and goto are unconditional jumps
            if let TerminatorKind::Split {
                true_block,
                false_block,
                loop_head,
                ..
            } = bb.terminator().kind
            {
                propagate_control_dependence(&mut cdg, ipdom, true_block, id);
                // There is no control flow control in Verilog-A and loops must always terminate.
                // Tt is therefore fair to assume that the blocks after the loop are not control dependent on the loop condition
                if !loop_head {
                    propagate_control_dependence(&mut cdg, ipdom, false_block, id);
                }
            }
        }

        cdg
    }
}

fn propagate_control_dependence(
    dst: &mut ControlDependenceGraph,
    ipdom: &PostDominators,
    mut from: BasicBlock,
    to: BasicBlock,
) {
    loop {
        if from == ipdom[to] {
            break;
        }
        dst.0.insert(from, to);

        if from == ipdom[from] {
            unreachable!("unclosed condition or circular post dominance (hoping to reach: {} ({}), but stuck at : {})", ipdom[to], to, from)
        }
        from = ipdom[from];
    }
}
