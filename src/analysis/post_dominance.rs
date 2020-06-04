//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::cfg::BasicBlockId;
use crate::ControlFlowGraph;
use index_vec::*;
use log::*;
use std::cmp::Ordering;

pub type IPDOM = IndexVec<BasicBlockId, BasicBlockId>;
id_type!(PostorderId(u16));

impl ControlFlowGraph {
    /// An implementation of the simple fast algorithm by [\[Keith 2006\]](https://www.cs.rice.edu/~keith/EMBED/dom.pdf).
    /// It is adobted for post dominance (simply the dominance of the reverse cfg).
    /// **returns** - immediate Post_dominator for every cfg node (`ipdom(n)`)
    pub fn post_dominators(&self) -> IPDOM {
        let post_order: IndexVec<PostorderId, _> = self.postorder_iter().collect();

        let undefined = post_order.len_idx();

        // Map cfg ids to postorder ids
        let mut post_order_mapping: IndexVec<BasicBlockId, PostorderId> =
            index_vec![undefined;post_order.len()];
        for (pid, (bid, _)) in post_order.iter_enumerated() {
            post_order_mapping[*bid] = pid;
        }

        // this idx con not appear
        let mut ipdom = index_vec![undefined; post_order.len()];

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
                debug_assert!(*bid != self.end());

                trace!("Simple fast postdominators: iterating {:?}", bid);

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
        let mut res: IndexVec<BasicBlockId, BasicBlockId> = index_vec![self.start();ipdom.len()];
        for (pid, &dominator_pid) in ipdom.iter_enumerated() {
            res[post_order[pid].0] = post_order[dominator_pid].0;
        }

        res
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
        trace!("itersection {:?} {:?}", finger1, finger2);
    }
}

#[cfg(feature = "graph_debug")]
mod print {
    use crate::analysis::IPDOM;
    use crate::cfg::BasicBlockId;
    use crate::ControlFlowGraph;
    use rustc_ap_graphviz as dot;
    use rustc_ap_graphviz::{Edges, Id, LabelText, Nodes};
    use std::borrow::Cow;
    use std::io::Write;

    pub struct IPDOM_TREE<'lt>(&'lt ControlFlowGraph, &'lt IPDOM);

    impl ControlFlowGraph {
        pub fn render_post_dominators<W: Write>(&self, write: &mut W, ipdom: &IPDOM) {
            dot::render(&IPDOM_TREE(self, ipdom), write).expect("Rendering failed")
        }
    }

    impl<'a> dot::Labeller<'a> for IPDOM_TREE<'a> {
        type Node = BasicBlockId;
        type Edge = (BasicBlockId, BasicBlockId);

        fn graph_id(&'a self) -> Id<'a> {
            dot::Id::new("PostDominatorTree").unwrap()
        }

        fn node_id(&'a self, id: &Self::Node) -> Id<'a> {
            dot::Id::new(format!("BB_{}", id)).unwrap()
        }

        fn node_label(&'a self, &n: &Self::Node) -> LabelText<'a> {
            LabelText::LabelStr(Cow::Owned(format!("{:?}", n)))
        }
        fn edge_label(&'a self, &(start, dst): &(BasicBlockId, BasicBlockId)) -> LabelText<'a> {
            LabelText::LabelStr(Cow::Borrowed("post dominates"))
        }
    }

    impl<'a> dot::GraphWalk<'a> for IPDOM_TREE<'a> {
        type Node = BasicBlockId;
        type Edge = (BasicBlockId, BasicBlockId);

        fn nodes(&'a self) -> Nodes<'a, Self::Node> {
            Cow::Owned(self.0.blocks.indices().collect())
        }

        fn edges(&'a self) -> Edges<'a, Self::Edge> {
            Cow::Owned(
                self.1
                    .iter_enumerated()
                    .map(|(ipdom_n, &n)| (n, ipdom_n))
                    .collect(),
            )
        }

        fn source(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.0
        }

        fn target(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.1
        }
    }
}
