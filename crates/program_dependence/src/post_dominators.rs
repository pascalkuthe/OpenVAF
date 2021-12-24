use std::cmp::Ordering;

use cfg::{BasicBlock, ControlFlowGraph};
use stdx::{impl_idx_from, impl_idx_math};
use typed_index_collections::{TiSlice, TiVec};

pub type PostDominators = TiSlice<BasicBlock, BasicBlock>;

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
struct PostorderId(u32);
impl_idx_from!(PostorderId(u32));
impl_idx_math!(PostorderId(u32));

/// Calculates the post dominators of a `ControlFlowGraph`.
/// Note: This pass assumes cannonicolized ret terminators
pub fn post_dominators(cfg: &ControlFlowGraph) -> Box<PostDominators> {
    let post_order: TiVec<PostorderId, _> = cfg.postorder_iter().collect();

    let undefined = post_order.len().into();

    // Map cfg ids to postorder ids
    let mut post_order_mapping: TiVec<BasicBlock, PostorderId> =
        vec![undefined; cfg.blocks.len()].into();
    for (pid, (bid, _)) in post_order.iter_enumerated() {
        post_order_mapping[*bid] = pid;
    }

    let mut ipdom: TiVec<PostorderId, PostorderId> = vec![undefined; cfg.blocks.len()].into();

    // exit node only post dominates itself
    ipdom.raw[0] = 0u32.into();

    let mut changed = true;
    while changed {
        changed = false;

        // Iterate in post order
        let mut iter = post_order.iter_enumerated();
        // Skip the end (root)
        iter.next();

        for (pid, (bid, bb)) in iter {
            debug_assert_ne!(*bid, cfg.blocks.len().into());

            let mut sucessors = bb
                .successors()
                .into_iter()
                .map(|bid| post_order_mapping[bid])
                .filter(|&p| ipdom[p] != undefined);

            if let Some(new_idom_idx) = sucessors.next() {
                let new_idom_idx = sucessors.fold(new_idom_idx, |new_idom_idx, predecessor_idx| {
                    intersect(&ipdom, new_idom_idx, predecessor_idx)
                });
                if new_idom_idx != ipdom[pid] {
                    ipdom[pid] = new_idom_idx;
                    changed = true;
                }
            } else {
                changed = true;
            }
        }
    }

    // Map PostorderIds back to Cfg ids
    let mut res: TiVec<BasicBlock, BasicBlock> =
        vec![cfg.blocks.len().into(); cfg.blocks.len()].into();

    for (pid, &dominator_pid) in ipdom.iter_enumerated() {
        if dominator_pid != undefined {
            res[post_order[pid].0] = post_order[dominator_pid].0;
        }
    }

    res.into_boxed_slice()
}

fn intersect(
    post_dominators: &TiSlice<PostorderId, PostorderId>,
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
