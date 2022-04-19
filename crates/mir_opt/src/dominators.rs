use std::cmp::Ordering;

use mir::ControlFlowGraph;
use mir::{Block, Function};
use stdx::packed_option::PackedOption;
use typed_index_collections::TiVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DomTreeNode {
    /// Number of this node in a (reverse) post-order traversal of the CFG, starting from 1.
    /// This number is monotonic in the reverse postorder but not contiguous, since we leave
    /// holes for later localized modifications of the dominator tree.
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
const SEEN: u32 = 2;
const DONE: u32 = 1;

#[derive(Default)]
pub struct DominatorTree {
    nodes: TiVec<Block, DomTreeNode>,
    /// CFG post-order of all reachable blocks.
    postorder: Vec<Block>,

    /// Scratch memory used by `compute_postorder()`.
    stack: Vec<Block>,
}

impl DominatorTree {
    /// Reset and compute a CFG post-order and dominator tree.
    pub fn compute(&mut self, func: &Function, cfg: &ControlFlowGraph) {
        debug_assert!(cfg.is_valid());
        self.compute_postorder(func, cfg);
        self.compute_domtree(func, cfg);
        // self.valid = true;
    }

    /// Clear the data structures used to represent the dominator tree. This will leave the tree in
    /// a state where `is_valid()` returns false.
    pub fn clear(&mut self) {
        self.nodes.clear();
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

    pub fn dominates(&self, mut block: Block, dominator: Block) -> bool {
        while self.nodes[block].rpo_number > self.nodes[dominator].rpo_number {
            if let Some(parent) = self.nodes[block].idom.expand() {
                block = parent;
            } else {
                return false;
            }
        }
        block == dominator
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
        self.clear();
        self.nodes
            .resize(func.layout.num_blocks(), DomTreeNode { rpo_number: UNDEF, idom: None.into() });

        match func.layout.entry_block() {
            Some(block) => {
                self.stack.push(block);
                self.nodes[block].rpo_number = SEEN;
            }
            None => return,
        }

        while let Some(block) = self.stack.pop() {
            match self.nodes[block].rpo_number {
                SEEN => {
                    // This is the first time we pop the block, so we need to scan its successors and
                    // then revisit it.
                    self.nodes[block].rpo_number = DONE;
                    self.stack.push(block);
                    for succ in cfg.succ_iter(block) {
                        if self.nodes[succ].rpo_number == UNDEF {
                            self.nodes[succ].rpo_number = SEEN;
                            self.stack.push(succ);
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
    }

    /// Build a dominator tree from a control flow graph using Keith D. Cooper's
    /// "Simple, Fast Dominator Algorithm."
    pub fn compute_domtree(&mut self, func: &Function, cfg: &ControlFlowGraph) {
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
        debug_assert_eq!(Some(entry_block), func.layout.entry_block());

        // Do a first pass where we assign RPO numbers to all reachable nodes.
        self.nodes[entry_block].rpo_number = 2;
        for (rpo_idx, &block) in postorder.iter().rev().enumerate() {
            // Update the current node and give it an RPO number.
            // The entry block got 2, the rest start at 3
            //
            // Since `compute_idom` will only look at nodes with an assigned RPO number, the
            // function will never see an uninitialized predecessor.
            //
            // Due to the nature of the post-order traversal, every node we visit will have at
            // least one predecessor that has previously been visited during this RPO.
            self.nodes[block] = DomTreeNode {
                rpo_number: rpo_idx as u32 + 3,
                idom: self.compute_idom(block, cfg).into(),
            }
        }

        // Now that we have RPO numbers for everything and initial immediate dominator estimates,
        // iterate until convergence.
        //
        // If the function is free of irreducible control flow, this will exit after one iteration.
        let mut changed = true;
        while changed {
            changed = false;
            for &block in postorder.iter().rev() {
                let idom = self.compute_idom(block, cfg).into();
                if self.nodes[block].idom != idom {
                    self.nodes[block].idom = idom;
                    changed = true;
                }
            }
        }
    }

    // Compute the immediate dominator for `block` using the current `idom` states for the reachable
    // nodes.
    fn compute_idom(&self, block: Block, cfg: &ControlFlowGraph) -> Block {
        // Get an iterator with just the reachable, already visited predecessors to `block`.
        // Note that during the first pass, `rpo_number` is 1 for reachable blocks that haven't
        // been visited yet, 0 for unreachable blocks.
        let mut reachable_preds = cfg.pred_iter(block).filter(|bb| self.nodes[*bb].rpo_number > 1);

        // The RPO must visit at least one predecessor before this node.
        let mut idom =
            reachable_preds.next().expect("block node must have one reachable predecessor");

        for pred in reachable_preds {
            idom = self.common_dominator(idom, pred);
        }

        idom
    }

    pub fn common_dominator(&self, mut bb1: Block, mut bb2: Block) -> Block {
        loop {
            let rpo1 = self.nodes[bb1].rpo_number;
            let rpo2 = self.nodes[bb2].rpo_number;
            match rpo1.cmp(&rpo2) {
                Ordering::Less => bb2 = self.nodes[bb2].idom.expect("Unreachable basic block?"),
                Ordering::Greater => bb1 = self.nodes[bb1].idom.expect("Unreachable basic block?"),
                Ordering::Equal => return bb1,
            }
        }
    }
}
