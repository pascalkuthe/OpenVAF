/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use openvaf_data_structures::index_vec::{index_vec, IndexVec};
use openvaf_ir::id_type;
use openvaf_middle::cfg::{AnalysisPass, BasicBlock, ControlFlowGraph};
use openvaf_middle::{impl_pass_span, CallType};
use std::cmp::Ordering;
use std::ops::Index;
use tracing::{debug, debug_span};

pub struct PostDominators(pub IndexVec<BasicBlock, BasicBlock>);

id_type!(PostorderId(u16));

impl Index<BasicBlock> for PostDominators {
    type Output = BasicBlock;

    fn index(&self, index: BasicBlock) -> &Self::Output {
        &self.0[index]
    }
}

pub struct BuildPostDominators;

impl<C: CallType> AnalysisPass<'_, C> for BuildPostDominators {
    type Result = PostDominators;

    impl_pass_span!("Build Post Dominators");

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let span = debug_span!("ipdom");
        let _enter = span.enter();
        let post_order: IndexVec<PostorderId, _> = cfg.postorder_iter().collect();

        let undefined = post_order.len_idx();

        // Map cfg ids to postorder ids
        let mut post_order_mapping: IndexVec<BasicBlock, PostorderId> =
            index_vec![undefined;cfg.blocks.len()];
        for (pid, (bid, _)) in post_order.iter_enumerated() {
            post_order_mapping[*bid] = pid;
        }

        // this idx con not appear
        let mut ipdom = index_vec![undefined; cfg.blocks.len()];

        // last node only post dominates itself
        ipdom[PostorderId::from_usize(0)] = PostorderId::from_usize(0);

        let mut changed = true;
        while changed {
            changed = false;

            // Iterate in post order
            let mut iter = post_order.iter_enumerated();
            // Skip the end (root)
            iter.next();

            for (pid, (bid, bb)) in iter {
                //let span = trace_span!("iterating", block = bid.index());
                //let _enter = span.enter();

                debug_assert!(*bid != cfg.end());

                let mut sucessors = bb
                    .successors()
                    .map(|bid| post_order_mapping[bid])
                    .filter(|&p| ipdom[p] != undefined);

                if let Some(new_idom_idx) = sucessors.next() {
                    let new_idom_idx =
                        sucessors.fold(new_idom_idx, |new_idom_idx, predecessor_idx| {
                            intersect(&ipdom, new_idom_idx, predecessor_idx)
                        });
                    if new_idom_idx != ipdom[pid] {
                        ipdom[pid] = new_idom_idx;
                        changed = true;
                    }
                } else {
                    debug!(
                        block = bid.index(),
                        "No predecessors have been processed yet"
                    );
                    changed = true;
                }
            }
        }

        // Map PostorderIds back to Cfg ids
        let mut res: IndexVec<BasicBlock, BasicBlock> = index_vec![cfg.end();cfg.blocks.len()];

        for (pid, &dominator_pid) in ipdom.iter_enumerated() {
            if dominator_pid != undefined {
                res[post_order[pid].0] = post_order[dominator_pid].0;
            }
        }

        PostDominators(res)
    }
}

fn intersect(
    post_dominators: &IndexVec<PostorderId, PostorderId>,
    mut finger1: PostorderId,
    mut finger2: PostorderId,
) -> PostorderId {
    loop {
        match finger1.cmp(&finger2) {
            Ordering::Greater => finger1 = post_dominators[finger1],
            Ordering::Less => finger2 = post_dominators[finger2],
            Ordering::Equal => return finger1,
        }
    }
}
