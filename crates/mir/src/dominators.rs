use std::cmp::Ordering;
use std::fs::File;
use std::path::Path;

use crate::flowgraph::Successors;
use crate::ControlFlowGraph;
use crate::{Block, Function};
use bitset::SparseBitMatrix;
use stdx::packed_option::PackedOption;
use typed_index_collections::{TiSlice, TiVec};

trait CfgREVERSE {
    type Successors;
    type Predecessors;

    fn successors(cfg: &ControlFlowGraph, bb: Block) -> Self::Successors;
    fn predecessors(cfg: &ControlFlowGraph, bb: Block) -> Self::Successors;
}

trait ToIter {}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DomTreeNode {
    /// Number of this node in a (reverse) post-order traversal of the CFG, starting from 1.
    /// This number is monotonic in the reverse postorder
    /// Unreachable nodes get number 0, all others are positive.
    rpo_number: u32,

    /// The immediate dominator of this block, represented as the branch or jump instruction at the
    /// end of the dominating basic block.
    ///
    /// This is `None` for unreachable blocks and the entry block which doesn't have an immediate
    /// dominator.
    idom: PackedOption<Block>,
}

const UNDEF: u32 = 0;
const DONE: u32 = 1;
const SEEN: u32 = 2;

#[derive(Default)]
pub struct DominatorTree {
    nodes: TiVec<Block, DomTreeNode>,
    reverse_nodes: TiVec<Block, DomTreeNode>,
    /// CFG post-order of all reachable blocks.
    postorder: Vec<Block>,
    stack: Vec<(Block, Successors)>,
}

impl DominatorTree {
    /// Reset and compute a CFG post-order and dominator tree.
    pub fn compute(
        &mut self,
        func: &Function,
        cfg: &ControlFlowGraph,
        dom: bool,
        pdom: bool,
        postorder: bool,
    ) {
        debug_assert!(cfg.is_valid());

        self.clear();
        if pdom {
            self.compute_reverse_postorder(func, cfg);
            self.compute_domtree::<true>(cfg);
            self.postorder.clear();
        }

        if dom || postorder {
            self.compute_postorder(func, cfg);
        }
        if dom {
            self.compute_domtree::<false>(cfg);
        }

        // self.valid = true;
    }

    /// Clear the data structures used to represent the dominator tree. This will leave the tree in
    /// a state where `is_valid()` returns false.
    pub fn clear(&mut self) {
        self.nodes.clear();
        self.reverse_nodes.clear();
        self.postorder.clear();
        debug_assert!(self.stack.is_empty());
        // self.valid = false;
    }

    /// Get the CFG post-order of blocks that was used to compute the dominator tree.
    ///
    /// Note that this post-order is not updated automatically when the CFG is modified. It is
    /// computed from scratch and cached by `compute()`.
    pub fn cfg_postorder(&self) -> &[Block] {
        &self.postorder
    }

    pub fn dominates(&self, block: Block, dominator: Block) -> bool {
        Self::dominates_(&self.nodes, block, dominator)
    }

    pub fn post_dominates(&self, block: Block, dominator: Block) -> bool {
        Self::dominates_(&self.reverse_nodes, block, dominator)
    }

    fn dominates_(nodes: &TiSlice<Block, DomTreeNode>, mut block: Block, dominator: Block) -> bool {
        while nodes[block].rpo_number > nodes[dominator].rpo_number {
            if let Some(parent) = nodes[block].idom.expand() {
                block = parent;
            } else {
                return false;
            }
        }
        block == dominator
    }

    pub fn ipdom(&self, block: Block) -> Option<Block> {
        self.reverse_nodes[block].idom.expand()
    }

    pub fn compute_dom_frontiers(
        &self,
        cfg: &ControlFlowGraph,
        dst: &mut SparseBitMatrix<Block, Block>,
    ) {
        dst.clear(self.nodes.len(), self.nodes.len());
        for bb in self.nodes.keys() {
            let mut predecessors = cfg.pred_iter(bb);
            let first = if let Some(first) = predecessors.next() { first } else { continue };
            let second = if let Some(second) = predecessors.next() { second } else { continue };
            Self::propagate_dom_frontiers(&self.nodes, first, bb, dst);
            Self::propagate_dom_frontiers(&self.nodes, second, bb, dst);
            for pred in predecessors {
                Self::propagate_dom_frontiers(&self.nodes, pred, bb, dst);
            }
        }
    }

    pub fn compute_postdom_frontiers(
        &self,
        cfg: &ControlFlowGraph,
        dst: &mut SparseBitMatrix<Block, Block>,
    ) {
        dst.clear(self.reverse_nodes.len(), self.reverse_nodes.len());
        for bb in self.reverse_nodes.keys() {
            if let Some((bb1, bb2)) = cfg.successors(bb).as_pair() {
                Self::propagate_dom_frontiers(&self.reverse_nodes, bb1, bb, dst);
                Self::propagate_dom_frontiers(&self.reverse_nodes, bb2, bb, dst);
            }
        }
    }

    /// walk up the dominator tree until we reach the dominator of to
    fn propagate_dom_frontiers(
        nodes: &TiSlice<Block, DomTreeNode>,
        mut pos: Block,
        bb: Block,
        dst: &mut SparseBitMatrix<Block, Block>,
    ) {
        let end = nodes[bb].idom;
        while PackedOption::from(pos) != end {
            dst.insert(pos, bb);
            if let Some(idom) = nodes[pos].idom.expand() {
                pos = idom
            } else {
                break;
            }
        }
    }

    // pub fn dom_frontiers(&self, block: Block) -> &HybridBitSet<Block> {
    //     debug_assert!(self.config.has_dom_frontiers, "dominance frontiers were not calculated");
    //     &self.nodes[block].dom_frontiers
    // }

    /// Reset all internal data structures and compute a post-order of the control flow graph.
    ///
    /// During this algorithm only, use `rpo_number` to hold the following state:
    ///
    ///   UNDEF:    block has not yet been reached in the pre-order.
    ///   SEEN: block has been pushed on the stack but successors not yet pushed.
    ///   DONE: Successors pushed.
    ///
    fn compute_reverse_postorder(&mut self, func: &Function, cfg: &ControlFlowGraph) {
        // self.compute_reverse_cfg_postorder(func, cfg);
        self.reverse_nodes
            .resize(func.layout.num_blocks(), DomTreeNode { rpo_number: UNDEF, idom: None.into() });
        match func.layout.last_block() {
            Some(block) => {
                self.stack.push((block, Successors(None.into(), None.into())));
                self.reverse_nodes[block].rpo_number = SEEN;
            }
            None => return,
        }

        while let Some((block, _)) = self.stack.pop() {
            match self.reverse_nodes[block].rpo_number {
                SEEN => {
                    // This is the first time we pop the block, so we need to scan its successors and
                    // then revisit it.
                    self.reverse_nodes[block].rpo_number = DONE;
                    self.stack.push((block, Successors(None.into(), None.into())));

                    for block in cfg.pred_iter(block) {
                        if self.reverse_nodes[block].rpo_number == UNDEF {
                            self.reverse_nodes[block].rpo_number = SEEN;
                            self.stack.push((block, Successors(None.into(), None.into())));
                        }
                    }
                }
                DONE => {
                    // This is the second time we pop the block, so all successors have been
                    // processed.
                    self.postorder.push(block);
                }
                _ => unreachable!(),
            }
        }

        debug_assert_eq!(self.postorder.last().copied(), func.layout.last_block());
    }

    /// Reset all internal data structures and compute a post-order of the control flow graph.
    ///
    /// During this algorithm only, use `rpo_number` to hold the following state:
    ///
    ///   UNDEF:    block has not yet been reached in the pre-order.
    ///   SEEN: block has been pushed on the stack but successors not yet pushed.
    ///   DONE: Successors pushed.
    ///
    fn compute_postorder(&mut self, func: &Function, cfg: &ControlFlowGraph) {
        // self.compute_reverse_cfg_postorder(func, cfg);
        self.nodes
            .resize(func.layout.num_blocks(), DomTreeNode { rpo_number: UNDEF, idom: None.into() });

        match func.layout.entry_block() {
            Some(block) => {
                self.stack.push((block, cfg.successors(block)));
                self.nodes[block].rpo_number = SEEN;
            }
            None => return,
        }

        loop {
            while let Some(block) = self.stack.last_mut().and_then(|(_, succ)| succ.pop()) {
                if self.nodes[block].rpo_number == UNDEF {
                    self.nodes[block].rpo_number = SEEN;
                    self.stack.push((block, cfg.successors(block)))
                }
            }

            if let Some((bb, _)) = self.stack.pop() {
                self.nodes[bb].rpo_number = DONE;
                self.postorder.push(bb)
            } else {
                break;
            }
        }
        debug_assert_eq!(self.postorder.last().copied(), func.layout.entry_block());
    }

    /// Build a dominator tree from a control flow graph using Keith D. Cooper's
    /// "Simple, Fast Dominator Algorithm."
    fn compute_domtree<const REVERSE: bool>(&mut self, cfg: &ControlFlowGraph) {
        // During this algorithm, `rpo_number` has the following values:
        //
        // 0: block is not reachable.
        // 1: block is reachable, but has not yet been visited during the first pass. This is set by
        // `compute_postorder`.
        // 2+: block is reachable and has an assigned RPO number.

        // We'll be iterating over a reverse post-order of the CFG, skipping the entry block.
        let (entry_block, postorder) = match self.postorder.as_slice().split_last() {
            Some((&eb, rest)) => (eb, rest),
            None => return,
        };

        // Do a first pass where we assign RPO numbers to all reachable nodes.
        let nodes = if REVERSE { &mut self.reverse_nodes } else { &mut self.nodes };
        nodes[entry_block].rpo_number = 2;
        for (rpo_idx, &block) in postorder.iter().rev().enumerate() {
            // Update the current node and give it an RPO number.
            // The entry block got 2, the rest start at 3
            //
            // Since `compute_idom` will only look at nodes with an assigned RPO number, the
            // function will never see an uninitialized predecessor.
            //
            // Due to the nature of the post-order traversal, every node we visit will have at
            // least one predecessor that has previously been visited during this RPO.
            let node = DomTreeNode {
                rpo_number: rpo_idx as u32 + 3,
                idom: self.compute_idom::<REVERSE>(block, cfg).into(),
            };

            let nodes = if REVERSE { &mut self.reverse_nodes } else { &mut self.nodes };
            nodes[block] = node;
        }

        // Iterate until convergence.
        //
        // If the function is free of irreducible control flow, this will exit after one iteration.
        let mut changed = true;
        while changed {
            changed = false;
            for &block in postorder.iter().rev() {
                let idom = self.compute_idom::<REVERSE>(block, cfg).into();
                let nodes = if REVERSE { &mut self.reverse_nodes } else { &mut self.nodes };
                if nodes[block].idom != idom {
                    nodes[block].idom = idom;
                    changed = true;
                }
            }
        }
    }

    fn compute_idom<const REVERSE: bool>(&self, block: Block, cfg: &ControlFlowGraph) -> Block {
        if REVERSE {
            Self::compute_idom_(&self.reverse_nodes, cfg.succ_iter(block))
        } else {
            Self::compute_idom_(&self.nodes, cfg.pred_iter(block))
        }
    }
    // Compute the immediate dominator for `block` using the current `idom` states for the reachable
    // nodes.
    fn compute_idom_(
        nodes: &TiSlice<Block, DomTreeNode>,
        preds: impl Iterator<Item = Block>,
    ) -> Block {
        // Get an iterator with just the reachable, already visited predecessors to `block`.
        // Note that during the first pass, `rpo_number` is 1 for reachable blocks that haven't
        // been visited yet, 0 for unreachable blocks.
        let mut reachable_preds = preds.filter(|bb| nodes[*bb].rpo_number > 1);

        // The RPO must visit at least one predecessor before this node.
        let mut idom =
            reachable_preds.next().expect("block node must have one reachable predecessor");

        for pred in reachable_preds {
            idom = Self::common_dominator(nodes, idom, pred);
        }

        idom
    }

    fn common_dominator(
        nodes: &TiSlice<Block, DomTreeNode>,
        mut bb1: Block,
        mut bb2: Block,
    ) -> Block {
        loop {
            let rpo1 = nodes[bb1].rpo_number;
            let rpo2 = nodes[bb2].rpo_number;
            match rpo1.cmp(&rpo2) {
                Ordering::Less => bb2 = nodes[bb2].idom.expect("Unreachable basic block?"),
                Ordering::Greater => bb1 = nodes[bb1].idom.expect("Unreachable basic block?"),
                Ordering::Equal => return bb1,
            }
        }
    }
}

pub struct DomTreeRender<'a> {
    pub dom_tree: &'a DominatorTree,
    pub func: &'a Function,
    pub name: &'a str,
    pub reverse: bool,
}

impl DominatorTree {
    pub fn render_idom(&self, dst: &Path, name: &str, func: &Function) {
        DomTreeRender { dom_tree: self, func, name, reverse: false }.to_dot(dst)
    }

    pub fn render_ipdom(&self, dst: &Path, name: &str, func: &Function) {
        DomTreeRender { dom_tree: self, func, name, reverse: true }.to_dot(dst)
    }

    // pub fn render_dom_frontier(&self, dst: &Path, name: &str, func: &Function) {
    //     DomTreeRender { dom_tree: self, func, name, postdom: false, frontiers: true }.to_dot(dst)
    // }
}

impl DomTreeRender<'_> {
    pub fn to_dot(&self, dst: &Path) {
        let mut dst = File::create(dst).unwrap();
        dot::render(self, &mut dst).unwrap()
    }
}

impl<'a> dot::Labeller<'a, Block, (Block, Block)> for DomTreeRender<'a> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new(self.name).unwrap()
    }

    fn node_id(&'a self, n: &Block) -> dot::Id<'a> {
        dot::Id::new(n.to_string()).unwrap()
    }
}

impl<'a> dot::GraphWalk<'a, Block, (Block, Block)> for DomTreeRender<'a> {
    fn nodes(&'a self) -> dot::Nodes<'a, Block> {
        self.func.layout.blocks().collect()
    }

    fn edges(&'a self) -> dot::Edges<'a, (Block, Block)> {
        let nodes = if self.reverse { &self.dom_tree.reverse_nodes } else { &self.dom_tree.nodes };
        self.func.layout.blocks().filter_map(|bb| Some((nodes[bb].idom.expand()?, bb))).collect()
    }

    fn source(&'a self, edge: &(Block, Block)) -> Block {
        edge.0
    }

    fn target(&'a self, edge: &(Block, Block)) -> Block {
        edge.1
    }
}
