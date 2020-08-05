//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use log::{debug, trace};
use openvaf_data_structures::index_vec::{index_vec, IndexVec};
use openvaf_ir::id_type;
use openvaf_mir::cfg::BasicBlockId;
use openvaf_mir::cfg::ControlFlowGraph;
use std::cmp::Ordering;

pub type IPDOM = IndexVec<BasicBlockId, BasicBlockId>;
id_type!(PostorderId(u16));

/// An implementation of the simple fast algorithm by [\[Keith 2006\]](https://www.cs.rice.edu/~keith/EMBED/dom.pdf).
/// It is adobted for post dominance (simply the dominance of the reverse cfg).
/// # Returns - immediate post dominator for every cfg node (`ipdom(n)`)
pub fn post_dominators(cfg: &ControlFlowGraph) -> IPDOM {
    let post_order: IndexVec<PostorderId, _> = cfg.postorder_iter().collect();

    let undefined = post_order.len_idx();

    // Map cfg ids to postorder ids
    let mut post_order_mapping: IndexVec<BasicBlockId, PostorderId> =
        index_vec![undefined;cfg.blocks.len()];
    for (pid, (bid, _)) in post_order.iter_enumerated() {
        post_order_mapping[*bid] = pid;
    }

    // this idx con not appear
    let mut ipdom = index_vec![undefined; cfg.blocks.len()];

    // last node only post dominates itself
    ipdom[PostorderId::from_usize(0)] = PostorderId::from_usize(0);

    // TOOD integrate into DFA framework?
    let mut changed = true;
    while changed {
        changed = false;

        // Iterate in post order
        let mut iter = post_order.iter_enumerated();
        // Skip the end (root)
        iter.next();

        for (pid, (bid, bb)) in iter {
            debug_assert!(*bid != cfg.end());

            trace!("Simple fast postdominators: iterating {:?}", bid);

            let mut sucessors = bb
                .successors()
                .map(|bid| post_order_mapping[bid])
                .filter(|&p| ipdom[p] != undefined);

            if let Some(new_idom_idx) = sucessors.next() {
                let new_idom_idx = sucessors.fold(new_idom_idx, |new_idom_idx, predecessor_idx| {
                    intersect(&ipdom, new_idom_idx, predecessor_idx)
                });
                if new_idom_idx != ipdom[pid] {
                    trace!("Simple fast postdomiantors: changed {:?}", bid);
                    ipdom[pid] = new_idom_idx;
                    changed = true;
                }
            } else {
                debug!("No predecessor of {:?} has ben processed yet", bid);
                changed = true;
            }
        }
    }

    // Map PostorderIds back to Cfg ids
    let mut res: IndexVec<BasicBlockId, BasicBlockId> = index_vec![cfg.start();cfg.blocks.len()];

    for (pid, &dominator_pid) in ipdom.iter_enumerated() {
        if dominator_pid != undefined {
            res[post_order[pid].0] = post_order[dominator_pid].0;
        }
    }

    res
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
        trace!("itersection {:?} {:?}", finger1, finger2);
    }
}
