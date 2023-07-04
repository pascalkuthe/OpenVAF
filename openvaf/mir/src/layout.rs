//! Function layout.
//!
//! The order of basic blocks in a function and the order of instructions in a block is
//! determined by the `Layout` data structure defined in this module.

use std::iter::{IntoIterator, Iterator};

use stdx::packed_option::PackedOption;
use typed_index_collections::TiVec;

use crate::{Block, Inst};

#[cfg(test)]
mod tests;

/// The `Layout` struct determines the layout of blocks and instructions in a function. It does not
/// contain definitions of instructions or blocks, but depends on `Inst` and `Block` entity references
/// being defined elsewhere.
///
/// This data structure determines:
///
/// - The order of blocks in the function.
/// - Which block contains a given instruction.
/// - The order of instructions with a block.
///
/// While data dependencies are not recorded, instruction ordering does affect control
/// dependencies, so part of the semantics of the program are determined by the layout.
///
#[derive(Clone)]
pub struct Layout {
    /// Linked list nodes for the layout order of blocks Forms a doubly linked list, terminated in
    /// both ends by `None`.
    blocks: TiVec<Block, BlockNode>,

    /// Linked list nodes for the layout order of instructions. Forms a double linked list per block,
    /// terminated in both ends by `None`.
    insts: TiVec<Inst, InstNode>,

    /// First block in the layout order, or `None` when no blocks have been laid out.
    first_block: Option<Block>,

    /// Last block in the layout order, or `None` when no blocks have been laid out.
    last_block: Option<Block>,
}

impl Default for Layout {
    fn default() -> Self {
        Self::new()
    }
}

impl Layout {
    /// Create a new empty `Layout`.
    pub fn new() -> Self {
        Self { blocks: TiVec::new(), insts: TiVec::new(), first_block: None, last_block: None }
    }

    /// Clear the layout.
    pub fn clear(&mut self) {
        self.blocks.clear();
        self.insts.clear();
        self.first_block = None;
        self.last_block = None;
    }

    /// Returns the capacity of the `BlockData` map.
    pub fn num_blocks(&self) -> usize {
        self.blocks.len()
    }
}

/// Methods for laying out blocks.
///
/// An unknown block starts out as *not inserted* in the block layout. The layout is a linear order of
/// inserted blocks. Once a block has been inserted in the layout, instructions can be added. A block
/// can only be removed from the layout when it is empty.
///
/// Since every block must end with a terminator instruction which cannot fall through, the layout of
/// blocks do not affect the semantics of the program.
///
impl Layout {
    pub fn make_block(&mut self) -> Block {
        self.blocks.push_and_get_key(BlockNode {
            prev: None.into(),
            next: None.into(),
            first_inst: None.into(),
            last_inst: None.into(),
        })
    }

    pub fn append_new_block(&mut self) -> Block {
        let res = self.blocks.push_and_get_key(BlockNode {
            prev: None.into(),
            next: None.into(),
            first_inst: None.into(),
            last_inst: None.into(),
        });

        self.append_block(res);

        res
    }

    /// Is `block` currently part of the layout?
    pub fn is_block_inserted(&self, block: Block) -> bool {
        Some(block) == self.first_block || self.blocks[block].prev.is_some()
    }

    /// Insert `block` as the last block in the layout.
    pub fn append_block(&mut self, block: Block) {
        debug_assert!(
            !self.is_block_inserted(block),
            "Cannot append block that is already in the layout"
        );
        {
            let node = &mut self.blocks[block];
            debug_assert!(node.first_inst.is_none() && node.last_inst.is_none());
            node.prev = self.last_block.into();
            node.next = None.into();
        }
        if let Some(last) = self.last_block {
            self.blocks[last].next = block.into();
        } else {
            self.first_block = Some(block);
        }
        self.last_block = Some(block);
    }

    /// Insert `block` in the layout before the existing block `before`.
    pub fn insert_block(&mut self, block: Block, before: Block) {
        debug_assert!(
            !self.is_block_inserted(block),
            "Cannot insert block that is already in the layout"
        );
        debug_assert!(self.is_block_inserted(before), "block Insertion point not in the layout");
        let after = self.blocks[before].prev;
        {
            let node = &mut self.blocks[block];
            node.next = before.into();
            node.prev = after;
        }
        self.blocks[before].prev = block.into();
        match after.expand() {
            None => self.first_block = Some(block),
            Some(a) => self.blocks[a].next = block.into(),
        }
    }

    /// Insert `block` in the layout *after* the existing block `after`.
    pub fn insert_block_after(&mut self, block: Block, after: Block) {
        debug_assert!(
            !self.is_block_inserted(block),
            "Cannot insert block that is already in the layout"
        );
        debug_assert!(self.is_block_inserted(after), "block Insertion point not in the layout");
        let before = self.blocks[after].next;
        {
            let node = &mut self.blocks[block];
            node.next = before;
            node.prev = after.into();
        }
        self.blocks[after].next = block.into();
        match before.expand() {
            None => self.last_block = Some(block),
            Some(b) => self.blocks[b].prev = block.into(),
        }
    }

    pub fn clear_block(&mut self, block: Block) {
        let mut curr = self.first_inst(block);
        while let Some(inst) = curr {
            let n = &mut self.insts[inst];
            curr = n.next.expand();
            *n = InstNode::default();
        }

        self.blocks[block].first_inst = None.into();
        self.blocks[block].last_inst = None.into();
    }

    pub fn remove_and_clear_block(&mut self, block: Block) {
        self.clear_block(block);
        self.remove_empty_block(block);
    }

    /// Remove `block` from the layout.
    pub fn remove_empty_block(&mut self, block: Block) {
        debug_assert!(self.is_block_inserted(block), "block not in the layout");
        debug_assert!(self.first_inst(block).is_none(), "block must be empty.");

        // Clear the `block` node and extract links.
        let n = &mut self.blocks[block];
        let prev = n.prev;
        let next = n.next;
        n.prev = None.into();
        n.next = None.into();

        // Fix up links to `block`.
        match prev.expand() {
            None => self.first_block = next.expand(),
            Some(p) => self.blocks[p].next = next,
        }
        match next.expand() {
            None => self.last_block = prev.expand(),
            Some(n) => self.blocks[n].prev = prev,
        }

        if self.last_block.unwrap() == block {
            self.last_block = prev.expand();
        }
    }

    /// Return an iterator over all blocks in layout order.
    pub fn blocks(&self) -> Blocks {
        Blocks { layout: self, next: self.first_block }
    }

    pub fn blocks_cursor(&self) -> BlockCursor {
        BlockCursor { next: self.first_block }
    }

    pub fn rev_blocks_cursor(&self) -> RevBlockCursor {
        RevBlockCursor { next: self.last_block }
    }

    /// Get the function's entry block.
    /// This is simply the first block in the layout order.
    pub fn entry_block(&self) -> Option<Block> {
        self.first_block
    }

    /// Get the last block in the layout.
    pub fn last_block(&self) -> Option<Block> {
        self.last_block
    }

    /// Get the block preceding `block` in the layout order.
    pub fn prev_block(&self, block: Block) -> Option<Block> {
        self.blocks[block].prev.expand()
    }

    /// Get the block following `block` in the layout order.
    pub fn next_block(&self, block: Block) -> Option<Block> {
        self.blocks[block].next.expand()
    }
}

#[derive(Clone, Debug, Default)]
struct BlockNode {
    prev: PackedOption<Block>,
    next: PackedOption<Block>,
    first_inst: PackedOption<Inst>,
    last_inst: PackedOption<Inst>,
}

/// Iterate over blocks in layout order. See [crate::ir::layout::Layout::blocks].
pub struct Blocks<'f> {
    layout: &'f Layout,
    next: Option<Block>,
}

impl<'f> Iterator for Blocks<'f> {
    type Item = Block;

    fn next(&mut self) -> Option<Block> {
        match self.next {
            Some(block) => {
                self.next = self.layout.next_block(block);
                Some(block)
            }
            None => None,
        }
    }
}

pub struct BlockCursor {
    pub next: Option<Block>,
}

impl BlockCursor {
    pub fn next(&mut self, layout: &Layout) -> Option<Block> {
        match self.next {
            Some(block) => {
                self.next = layout.next_block(block);
                Some(block)
            }
            None => None,
        }
    }
}

pub struct RevBlockCursor {
    pub next: Option<Block>,
}

impl RevBlockCursor {
    pub fn next(&mut self, layout: &Layout) -> Option<Block> {
        match self.next {
            Some(block) => {
                self.next = layout.prev_block(block);
                Some(block)
            }
            None => None,
        }
    }
}

/// Use a layout reference in a for loop.
impl<'f> IntoIterator for &'f Layout {
    type Item = Block;
    type IntoIter = Blocks<'f>;

    fn into_iter(self) -> Blocks<'f> {
        self.blocks()
    }
}

/// Methods for arranging instructions.
///
/// An instruction starts out as *not inserted* in the layout. An instruction can be inserted into
/// a block at a given position.
impl Layout {
    /// Get the block containing `inst`, or `None` if `inst` is not inserted in the layout.
    pub fn inst_block(&self, inst: Inst) -> Option<Block> {
        self.insts.get(inst).and_then(|inst| inst.block.expand())
    }

    // /// Get the block containing the program point `pp`. Panic if `pp` is not in the layout.
    // pub fn pp_block<PP>(&self, pp: PP) -> Block
    // where
    //     PP: Into<ExpandedProgramPoint>,
    // {
    //     match pp.into() {
    //         ExpandedProgramPoint::Block(block) => block,
    //         ExpandedProgramPoint::Inst(inst) => {
    //             self.inst_block(inst).expect("Program point not in layout")
    //         }
    //     }
    // }

    /// Append `inst` to the end of `block`.
    pub fn append_inst_to_bb(&mut self, inst: Inst, block: Block) {
        if self.insts.len() <= usize::from(inst) {
            self.insts.resize(usize::from(inst) + 1, InstNode::default())
        }

        debug_assert_eq!(self.inst_block(inst), None);
        debug_assert!(
            self.is_block_inserted(block),
            "Cannot append instructions to block not in layout"
        );

        let block_node = &mut self.blocks[block];
        {
            let inst_node = &mut self.insts[inst];
            inst_node.block = block.into();
            inst_node.prev = block_node.last_inst;
            debug_assert!(inst_node.next.is_none());
        }
        if block_node.first_inst.is_none() {
            block_node.first_inst = inst.into();
        } else {
            self.insts[block_node.last_inst.unwrap()].next = inst.into();
        }
        block_node.last_inst = inst.into();
    }

    /// Fetch a block's first instruction.
    pub fn first_inst(&self, block: Block) -> Option<Inst> {
        self.blocks[block].first_inst.into()
    }

    /// Fetch a block's last instruction.
    pub fn last_inst(&self, block: Block) -> Option<Inst> {
        self.blocks[block].last_inst.into()
    }

    /// Fetch the instruction following `inst`.
    pub fn next_inst(&self, inst: Inst) -> Option<Inst> {
        self.insts[inst].next.expand()
    }

    /// Fetch the instruction preceding `inst`.
    pub fn prev_inst(&self, inst: Inst) -> Option<Inst> {
        self.insts[inst].prev.expand()
    }

    /// Insert `inst` before the instruction `before` in the same block.
    pub fn prepend_inst(&mut self, inst: Inst, before: Inst) {
        if self.insts.len() <= usize::from(inst) {
            self.insts.resize(usize::from(inst) + 1, InstNode::default())
        }

        debug_assert_eq!(self.inst_block(inst), None);
        let block =
            self.inst_block(before).expect("Instruction before insertion point not in the layout");

        let after = self.insts[before].prev;
        let inst_node = &mut self.insts[inst];
        inst_node.block = block.into();
        inst_node.next = before.into();
        inst_node.prev = after;
        self.insts[before].prev = inst.into();
        match after.expand() {
            None => self.blocks[block].first_inst = inst.into(),
            Some(a) => self.insts[a].next = inst.into(),
        }
    }

    /// Insert `inst` after the instruction `after` in the same block.
    pub fn append_inst(&mut self, inst: Inst, after: Inst) {
        if self.insts.len() <= usize::from(inst) {
            self.insts.resize(usize::from(inst) + 1, InstNode::default())
        }

        debug_assert_eq!(self.inst_block(inst), None);
        let block =
            self.inst_block(after).expect("Instruction before insertion point not in the layout");

        let before = self.insts[after].next;
        let inst_node = &mut self.insts[inst];
        inst_node.block = block.into();
        inst_node.next = before;
        inst_node.prev = after.into();
        self.insts[after].next = inst.into();
        match before.expand() {
            None => self.blocks[block].last_inst = inst.into(),
            Some(prev) => self.insts[prev].prev = inst.into(),
        }
    }

    /// Remove `inst` from the layout.
    pub fn remove_inst(&mut self, inst: Inst) {
        let block = self.inst_block(inst).expect("Instruction already removed.");
        // Clear the `inst` node and extract links.
        let prev;
        let next;
        {
            let n = &mut self.insts[inst];
            prev = n.prev;
            next = n.next;
            n.block = None.into();
            n.prev = None.into();
            n.next = None.into();
        }
        // Fix up links to `inst`.
        match prev.expand() {
            None => self.blocks[block].first_inst = next,
            Some(p) => self.insts[p].next = next,
        }
        match next.expand() {
            None => self.blocks[block].last_inst = prev,
            Some(n) => self.insts[n].prev = prev,
        }
    }

    /// Iterate over the instructions in `block` in layout order.
    pub fn block_insts(&self, block: Block) -> InstIter {
        InstIter { layout: self, cursor: self.block_inst_cursor(block) }
    }

    pub fn block_insts_no_term(&self, block: Block) -> InstIter {
        let mut insts = self.block_insts(block);
        insts.next_back();
        insts
    }

    /// Iterate over the instructions in `block` in layout order.
    pub fn block_inst_cursor(&self, block: Block) -> InstCursor {
        InstCursor {
            head: self.blocks[block].first_inst.into(),
            tail: self.blocks[block].last_inst.into(),
        }
    }

    pub fn block_terminator(&self, block: Block) -> Option<Inst> {
        self.blocks[block].last_inst.into()
    }

    /// Merges `succ` ito `pred` by remvoing the terminator from `pred` and appding all instructions
    /// to `pred`. Aftwars `succ` is removed from the layout
    ///
    /// #Note
    /// It is up to the caller to ensure that this merge is valid:
    ///
    /// * No phis remain in `succ`
    /// * `pred` is terminated by a `jump` to `succ`
    /// * `no other branches to `succ` remain
    /// that `succ` has no arguments
    pub fn merge_blocks(&mut self, pred: Block, succ: Block) {
        // remove branch instructions from `pred`
        if let Some(succ_start) = self.blocks[succ].first_inst.expand() {
            let pred_end = {
                let mut cursor = self.block_inst_cursor(pred);
                let jmp_branch = cursor.next_back(self).unwrap();
                self.insts[jmp_branch] = InstNode::default();
                cursor.next_back(self)
            };

            let mut cursor = self.block_inst_cursor(succ);
            while let Some(inst) = cursor.next(self) {
                self.insts[inst].block = pred.into();
            }

            if let Some(pred_end) = pred_end {
                // link up insts
                self.insts[pred_end].next = succ_start.into();
                self.insts[succ_start].prev = pred_end.into();
            } else {
                // predecessor only contained jmp
                // just update the block
                self.blocks[pred].first_inst = self.blocks[succ].first_inst;
            }

            self.blocks[pred].last_inst = self.blocks[succ].last_inst;
        } else {
            // successor is empty... Kind of odd but probably valid (collapse empty jump the
            // terminator). Just remove the branch
            self.remove_inst(self.last_inst(pred).unwrap())
        }

        self.blocks[succ].first_inst = None.into();
        self.blocks[succ].last_inst = None.into();

        // finally delete `succ`
        self.remove_empty_block(succ)
    }

    /// Split the block containing `before` in two.
    ///
    /// Insert `new_block` after the old block and move `before` and the following instructions to
    /// `new_block`:
    ///
    /// ```text
    /// old_block:
    ///     i1
    ///     i2
    ///     i3 << before
    ///     i4
    /// ```
    /// becomes:
    ///
    /// ```text
    /// old_block:
    ///     i1
    ///     i2
    /// new_block:
    ///     i3 << before
    ///     i4
    /// ```
    pub(crate) fn split_block(&mut self, new_block: Block, before: Inst) {
        let old_block =
            self.inst_block(before).expect("The `before` instruction must be in the layout");
        debug_assert!(!self.is_block_inserted(new_block));

        // Insert new_block after old_block.
        let next_block = self.blocks[old_block].next;
        let last_inst = self.blocks[old_block].last_inst;
        {
            let node = &mut self.blocks[new_block];
            node.prev = old_block.into();
            node.next = next_block;
            node.first_inst = before.into();
            node.last_inst = last_inst;
        }
        self.blocks[old_block].next = new_block.into();

        // Fix backwards link.
        if Some(old_block) == self.last_block {
            self.last_block = Some(new_block);
        } else {
            self.blocks[next_block.unwrap()].prev = new_block.into();
        }

        // Disconnect the instruction links.
        let prev_inst = self.insts[before].prev;
        self.insts[before].prev = None.into();
        self.blocks[old_block].last_inst = prev_inst;
        match prev_inst.expand() {
            None => self.blocks[old_block].first_inst = None.into(),
            Some(pi) => self.insts[pi].next = None.into(),
        }

        // Fix the instruction -> block pointers.
        let mut opt_i = Some(before);
        while let Some(i) = opt_i {
            debug_assert_eq!(self.insts[i].block.expand(), Some(old_block));
            self.insts[i].block = new_block.into();
            opt_i = self.insts[i].next.into();
        }
    }
}

#[derive(Clone, Debug, Default)]
struct InstNode {
    /// The Block containing this instruction, or `None` if the instruction is not yet inserted.
    block: PackedOption<Block>,
    prev: PackedOption<Inst>,
    next: PackedOption<Inst>,
}

#[derive(Clone)]
/// Iterate over instructions in a block in layout order. See `Layout::block_insts()`.
pub struct InstIter<'f> {
    pub layout: &'f Layout,
    pub cursor: InstCursor,
}

impl<'f> Iterator for InstIter<'f> {
    type Item = Inst;

    fn next(&mut self) -> Option<Inst> {
        self.cursor.next(self.layout)
    }
}

impl<'f> DoubleEndedIterator for InstIter<'f> {
    fn next_back(&mut self) -> Option<Inst> {
        self.cursor.next_back(self.layout)
    }
}

/// Iterate over instructions in a block in layout order. See `Layout::block_insts()`.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct InstCursor {
    pub head: Option<Inst>,
    pub tail: Option<Inst>,
}

impl InstCursor {
    pub fn next(&mut self, layout: &Layout) -> Option<Inst> {
        let rval = self.head;
        if let Some(inst) = rval {
            if self.head == self.tail {
                self.head = None;
                self.tail = None;
            } else {
                self.head = layout.insts[inst].next.into();
            }
        }
        rval
    }
}

impl InstCursor {
    pub fn next_back(&mut self, layout: &Layout) -> Option<Inst> {
        let rval = self.tail;
        if let Some(inst) = rval {
            if self.head == self.tail {
                self.head = None;
                self.tail = None;
            } else {
                self.tail = layout.insts[inst].prev.into();
            }
        }
        rval
    }
}
