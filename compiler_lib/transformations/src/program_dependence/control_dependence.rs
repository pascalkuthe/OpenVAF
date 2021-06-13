/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::program_dependence::post_dominance::PostDominators;
use openvaf_data_structures::{HybridBitSet, SparseBitSetMatrix};
use openvaf_middle::cfg::{BasicBlock, ControlFlowGraph, TerminatorKind};
use openvaf_middle::CallType;
use std::ops::Index;
use tracing::{debug_span, trace};

pub struct ControlDependenceGraph {
    data: SparseBitSetMatrix<BasicBlock, BasicBlock>,
}

impl Index<BasicBlock> for ControlDependenceGraph {
    type Output = Option<HybridBitSet<BasicBlock>>;

    fn index(&self, index: BasicBlock) -> &Self::Output {
        &self.data.row(index)
    }
}

impl ControlDependenceGraph {
    /// Calculates the Control Dependence Graph of a CFG.
    /// A basic block is control dependent on a statement if this statements decides whether the block is executed (for example an if statement).
    /// Statements that cause control flow are not represented as statements but as basic block terminators in the cfg.
    /// As such the control dependence Graph simply maps basic blocks to all basics block whose terminator affects whether a statements is executed
    pub fn from_cfg<C: CallType>(cfg: &ControlFlowGraph<C>) -> Self {
        Self::from_ipdom(cfg, &PostDominators::new(cfg))
    }

    /// Calculates the control dependence graph when `ipdom(bb)` has already been calculated
    ///# Note
    /// This is only a seperate funciton to avoid recalculating the post dominators in some cases
    /// If you dont want to reuse `ipdom` you should use [`from_cfg`](crate::ControlDependenceGraph::from_cfg)
    pub fn from_ipdom<C: CallType>(cfg: &ControlFlowGraph<C>, ipdom: &PostDominators) -> Self {
        let _span = debug_span!("control_dependence");
        let _enter = _span.enter();

        let mut cdg = Self {
            data: SparseBitSetMatrix::new_empty(cfg.blocks.len_idx(), cfg.blocks.len_idx()),
        };
        for (id, bb) in cfg.blocks.iter_enumerated() {
            // we only care about control dependencies on branches since end and goto are unconditional jumps
            if let TerminatorKind::Split {
                true_block,
                false_block,
                loop_head,
                ..
            } = bb.terminator().kind
            {
                cdg.propagate_control_dependence(&ipdom, true_block, id);
                // There is no control flow control in Verilog-A and loops must always terminate.
                // Tt is therefore fair to assume that the blocks after the loop are not control dependent on the loop condition
                if !loop_head {
                    cdg.propagate_control_dependence(&ipdom, false_block, id);
                }
            }
        }

        cdg
    }

    fn propagate_control_dependence(
        &mut self,
        ipdom: &PostDominators,
        mut from: BasicBlock,
        to: BasicBlock,
    ) {
        loop {
            trace!(
                control_dep = to.index(),
                block = from.index(),
                merge = ipdom[to].index(),
                block_post_dom = ipdom[from].index(),
                "propgating control dependency"
            );

            if from == ipdom[to] {
                break;
            }
            self.data.insert(from, to);

            if from == ipdom[from] {
                unreachable!("unclosed condition or circular post dominance (hoping to reach: {} ({}), but stuck at : {})", ipdom[to], to, from)
            }
            from = ipdom[from];
        }
        trace!("Done")
    }

    pub fn inverse(&self) -> InvControlDependenceGraph {
        let mut data = SparseBitSetMatrix::new_empty(self.data.y_len_idx(), self.data.x_len_idx());
        for (bb, control_dependencies) in self.data.rows_enumerated() {
            for control_dependency in control_dependencies.ones() {
                data.insert(control_dependency, bb)
            }
        }
        InvControlDependenceGraph { data }
    }
}

pub struct InvControlDependenceGraph {
    data: SparseBitSetMatrix<BasicBlock, BasicBlock>,
}

impl Index<BasicBlock> for InvControlDependenceGraph {
    type Output = Option<HybridBitSet<BasicBlock>>;

    fn index(&self, index: BasicBlock) -> &Self::Output {
        &self.data.row(index)
    }
}
