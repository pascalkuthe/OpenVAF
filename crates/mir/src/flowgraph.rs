//! A control flow graph represented as mappings of basic blocks to their predecessors
//! and successors.
//!
//! Successors are represented as basic blocks while predecessors are represented by basic
//! blocks. Basic blocks are denoted by tuples of block and branch/jump instructions. Each
//! predecessor tuple corresponds to the end of a basic block.
//!
//! ```c
//!     Block0:
//!         ...          ; beginning of basic block
//!
//!         ...
//!
//!         brz vx, Block1 ; end of basic block
//!
//!         ...          ; beginning of basic block
//!
//!         ...
//!
//!         jmp Block2     ; end of basic block
//! ```
//!
//! Here `Block1` and `Block2` would each have a single predecessor denoted as `(Block0, brz)`
//! and `(Block0, jmp Block2)` respectively.

use std::cmp::Ordering;
use std::iter::FilterMap;
use std::ops::Index;

use bforest::Set;
use stdx::packed_option::PackedOption;
use typed_index_collections::TiVec;

pub use crate::flowgraph::transversal::{Postorder, ReversePostorder};
use crate::{Block, Function, InstructionData};

mod transversal;
// #[cfg(test)]
// mod tests;

/// A container for the successors and predecessors of some Block.
#[derive(Clone, Default)]
pub struct CFGNode {
    /// Instructions that can branch or jump to this block.
    ///
    /// This maps branch instruction -> predecessor block which is redundant since the block containing
    /// the branch instruction is available from the `layout.inst_block()` method. We store the
    /// redundant information because:
    ///
    /// 1. Many `pred_iter()` consumers want the block anyway, so it is handily available.
    /// 2. The `invalidate_block_successors()` may be called *after* branches have been removed from
    ///    their block, but we still need to remove them form the old block predecessor map.
    ///
    /// The redundant block stored here is always consistent with the CFG successor lists, even after
    /// the IR has been edited.
    pub predecessors: bforest::Set<Block>,

    /// Set of blocks that are the targets of branches and jumps in this block.
    /// The set is ordered by block number, indicated by the `()` comparator type.
    pub successors: Successors,
}

#[derive(Clone, Default, Copy, PartialEq, Eq, Debug)]
pub struct Successors(PackedOption<Block>, PackedOption<Block>);

impl Successors {
    #[inline]
    pub fn clear(&mut self) {
        self.0 = None.into();
        self.1 = None.into();
    }

    #[inline]
    pub fn insert(&mut self, bb: Block) -> bool {
        let res = PackedOption::from(bb);
        let res = match self.0.cmp(&res) {
            Ordering::Equal => false,
            Ordering::Less => {
                let changed = self.1 != res;
                debug_assert!(
                    self.1.is_none() || !changed,
                    "not space to insert {} into [{:?}, {:?}]",
                    bb,
                    self.0,
                    self.1
                );
                self.1 = res;
                changed
            }
            Ordering::Greater => {
                debug_assert!(self.0.is_none() || self.1.is_none());
                self.1 = self.0;
                self.0 = res;
                true
            }
        };

        debug_assert_ne!(self.0, self.1);
        res
    }

    #[inline]
    pub fn contains(self, bb: Block) -> bool {
        let expected = PackedOption::from(bb);
        (self.0 == expected) | (self.1 == expected)
    }

    #[inline]
    pub fn pop(&mut self) -> Option<Block> {
        self.1.take().or_else(|| self.0.take())
    }

    #[inline]
    pub fn iter(self) -> SuccIter {
        [self.0, self.1].into_iter().filter_map(|it| it.expand())
    }

    pub fn is_empty(self) -> bool {
        self.0.is_none()
    }
}

pub type SuccIter = FilterMap<
    std::array::IntoIter<PackedOption<Block>, 2>,
    fn(PackedOption<Block>) -> Option<Block>,
>;

/// The Control Flow Graph maintains a mapping of blocks to their predecessors
/// and successors where predecessors are basic blocks and successors are
/// basic blocks.
#[derive(Clone)]
pub struct ControlFlowGraph {
    data: TiVec<Block, CFGNode>,
    pred_forest: bforest::SetForest<Block>,
    valid: bool,
}

impl ControlFlowGraph {
    /// Allocate a new blank control flow graph.
    pub fn new() -> Self {
        Self { data: TiVec::new(), valid: false, pred_forest: bforest::SetForest::new() }
    }

    /// Clear all data structures in this control flow graph.
    pub fn clear(&mut self) {
        self.data.clear();
        self.pred_forest.clear();
        self.data.raw.as_mut_slice().fill(CFGNode::default());
        self.valid = false;
    }

    /// Allocate and compute the control flow graph for `func`.
    pub fn with_function(func: &Function) -> Self {
        let mut cfg = Self::new();
        cfg.compute(func);
        cfg
    }

    /// Compute the control flow graph of `func`.
    ///
    /// This will clear and overwrite any information already stored in this data structure.
    pub fn compute(&mut self, func: &Function) {
        self.clear();
        self.data.resize(func.layout.num_blocks(), CFGNode::default());

        for block in &func.layout {
            self.compute_block(func, block);
        }

        self.valid = true;
    }

    fn compute_block(&mut self, func: &Function, block: Block) {
        if let Some(inst) = func.layout.last_inst(block) {
            match func.dfg.insts[inst] {
                InstructionData::Jump { destination } => self.add_edge(block, destination),
                InstructionData::Branch { then_dst, else_dst, .. } => {
                    self.add_edge(block, then_dst);
                    self.add_edge(block, else_dst);
                }
                _ => (),
            }
        }
    }

    fn invalidate_block_successors(&mut self, block: Block) {
        // Temporarily take ownership because we need mutable access to self.data inside the loop.
        // Unfortunately borrowck cannot see that our mut accesses to predecessors don't alias
        // our iteration over successors.
        let successors = std::mem::take(&mut self.data[block].successors);
        for succ in successors.iter() {
            self.data[succ].predecessors.retain(&mut self.pred_forest, |pred| pred != block);
        }
    }

    /// Recompute the control flow graph of `block`.
    ///
    /// This is for use after modifying instructions within a specific block. It recomputes all edges
    /// from `block` while leaving edges to `block` intact. Its functionality a subset of that of the
    /// more expensive `compute`, and should be used when we know we don't need to recompute the CFG
    /// from scratch, but rather that our changes have been restricted to specific blocks.
    pub fn recompute_block(&mut self, func: &Function, block: Block) {
        debug_assert!(self.is_valid());
        self.invalidate_block_successors(block);
        self.compute_block(func, block);
    }

    pub fn replace(&mut self, old: Block, new: Block) {
        debug_assert_ne!(old, new);
        debug_assert!(self.is_valid());
        let mut pos = self.predecessors(old).read_cursor();
        let old_ = old.into();
        let new_ = new.into();
        while let Some(pred) = pos.next(&self.pred_forest) {
            if self.data[pred].successors.0 == old_ {
                self.data[pred].successors.0 = new_;
                if self.data[pred].successors.1 == new_ {
                    self.data[pred].successors.1.take();
                }
            } else {
                debug_assert_eq!(self.data[pred].successors.1, old_);
                if self.data[pred].successors.0 == new_ {
                    self.data[pred].successors.1.take();
                } else {
                    self.data[pred].successors.1 = new_;
                }
            }

            self.data[new].predecessors.insert(pred, &mut self.pred_forest, &());
        }
        self.data[old].predecessors.clear(&mut self.pred_forest);
        self.invalidate_block_successors(old);
    }

    fn add_edge(&mut self, from: Block, to: Block) {
        self.data[from].successors.insert(to);
        self.data[to].predecessors.insert(from, &mut self.pred_forest, &());
    }

    /// Get an iterator over the CFG predecessors to `block`.
    pub fn pred_iter(&self, block: Block) -> bforest::SetIter<'_, Block> {
        self.data[block].predecessors.iter(&self.pred_forest)
    }

    pub fn predecessors(&self, block: Block) -> &Set<Block> {
        &self.data[block].predecessors
    }

    pub fn successors(&self, block: Block) -> Successors {
        self.data[block].successors
    }

    pub fn is_predecessors(&self, pred: Block, block: Block) -> bool {
        self.data[block].predecessors.contains(pred, &self.pred_forest, &())
    }

    /// Get an iterator over the CFG successors to `block`.
    pub fn succ_iter(&self, block: Block) -> SuccIter {
        debug_assert!(self.is_valid());
        self.data[block].successors.iter()
    }

    pub fn unique_succ(&self, block: Block) -> Option<Block> {
        let mut iter = self.succ_iter(block);
        let res = iter.next();
        if iter.next().is_none() {
            res
        } else {
            None
        }
    }

    /// Check if the CFG is in a valid state.
    ///
    /// Note that this doesn't perform any kind of validity checks. It simply checks if the
    /// `compute()` method has been called since the last `clear()`. It does not check that the
    /// CFG is consistent with the function.
    pub fn is_valid(&self) -> bool {
        self.valid
    }

    #[inline]
    pub fn self_loop(&self, bb: Block) -> bool {
        let mut iter = self.pred_iter(bb);
        iter.all(|pred| pred == bb)
    }

    #[inline]
    pub fn single_predecessor(&self, bb: Block) -> Option<Block> {
        let mut iter = self.pred_iter(bb);
        let res = iter.next()?;
        if iter.next().is_none() {
            Some(res)
        } else {
            None
        }
    }

    #[inline]
    pub fn empty_predecessors(&self, bb: Block) -> bool {
        self.pred_iter(bb).next().is_none()
    }

    #[inline]
    pub fn reverse_postorder_from(&self, start: Block) -> ReversePostorder {
        ReversePostorder::new(self, start)
    }

    #[inline]
    pub fn reverse_postorder(&self, func: &Function) -> ReversePostorder {
        ReversePostorder::new(self, func.layout.entry_block().unwrap())
    }

    #[inline]
    pub fn postorder_from(&self, start: Block) -> Postorder {
        Postorder::new(self, start)
    }

    #[inline]
    pub fn postorder(&self, func: &Function) -> Postorder {
        Postorder::new(self, func.layout.entry_block().unwrap())
    }
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<Block> for ControlFlowGraph {
    type Output = CFGNode;

    fn index(&self, bb: Block) -> &Self::Output {
        &self.data[bb]
    }
}
