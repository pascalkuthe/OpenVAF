//! Algorithms to find post dominators and post dominator frontier.
//! Both Algorithms were taken from [A Simple, Fast Dominance Algorithm] by Keith D. Cooper, Timothy J. Harvey, and Ken Kenned
//!
//! [A Simple, Fast Dominance Algorithm]: https://www.cs.rice.edu/~keith/EMBED/dom.pdf

use std::cmp::Ordering;

use bitset::SparseBitMatrix;
use mir::ControlFlowGraph;
use mir::{Block, Function, InstructionData};
use stdx::{impl_idx_from, impl_idx_math};
use typed_index_collections::{TiSlice, TiVec};

pub type PostDominators = TiSlice<Block, Block>;

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
struct PostorderId(u32);
impl_idx_from!(PostorderId(u32));
impl_idx_math!(PostorderId(u32));

/// Calculates the post dominators of a `ControlFlowGraph`.
/// Note: This pass assumes cannonicolized ret terminators
pub fn post_dominators(cfg: &ControlFlowGraph, func: &Function) -> Box<PostDominators> {
    let post_order: TiVec<PostorderId, _> = cfg.postorder(func).collect();

    let undefined = post_order.len().into();

    // Map cfg ids to postorder ids
    let mut post_order_mapping: TiVec<Block, PostorderId> =
        vec![undefined; func.layout.num_blocks()].into();
    for (pid, bb) in post_order.iter_enumerated() {
        post_order_mapping[*bb] = pid;
    }

    let mut ipdom: TiVec<PostorderId, PostorderId> =
        vec![undefined; func.layout.num_blocks()].into();

    // exit node only post dominates itself
    ipdom.raw[0] = 0u32.into();

    let mut changed = true;
    while changed {
        changed = false;

        // Iterate in post order
        let mut iter = post_order.iter_enumerated();
        // Skip the end (root)
        iter.next();

        for (pid, bb) in iter {
            debug_assert_ne!(*bb, func.layout.num_blocks().into());

            let mut sucessors = cfg
                .succ_iter(*bb)
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
    let mut res: TiVec<Block, Block> =
        vec![func.layout.num_blocks().into(); func.layout.num_blocks()].into();

    for (pid, &dominator_pid) in ipdom.iter_enumerated() {
        if dominator_pid != undefined {
            res[post_order[pid]] = post_order[dominator_pid];
        }
    }

    res.into_boxed_slice()
}

/// Calculates the post dominance frontiers of a CFG.
///
/// The post dominance frontiers of a block `X` are all blocks `Y` that:
/// * are not post dominated by `X`
/// * have at least once sucessor that is post dominated by `X`
///
/// Intuetivly the post dominance frontier of a block form its control dependence:
/// A basic block is control dependent on another block if that blocks terminator decides whether the block is executed (for example an if statement).
pub fn postdom_frontiers(cfg: &ControlFlowGraph, func: &Function) -> PostDominanceFrontiers {
    let ipdom = post_dominators(cfg, func);
    let mut cdg = SparseBitMatrix::new(func.layout.num_blocks(), func.layout.num_blocks());

    for bb in func.layout.blocks() {
        if let Some(term) = func.layout.last_inst(bb) {
            if let InstructionData::Branch { then_dst, else_dst, .. } = func.dfg.insts[term] {
                propagate_postdom_frontiers(&mut cdg, &ipdom, then_dst, bb);
                propagate_postdom_frontiers(&mut cdg, &ipdom, else_dst, bb);
            }
        }
    }

    cdg
}

pub type PostDominanceFrontiers = SparseBitMatrix<Block, Block>;

/// walk up the postdominator tree until we reach the postdominator of
fn propagate_postdom_frontiers(
    dst: &mut PostDominanceFrontiers,
    ipdom: &PostDominators,
    mut from: Block,
    to: Block,
) {
    while from != ipdom[to] {
        dst.insert(from, to);
        debug_assert_ne!(from ,ipdom[from],"unclosed condition or circular post dominance (hoping to reach: {:?} ({:?}), but stuck at : {:?})", ipdom[to], to, from);
        from = ipdom[from];
    }
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


