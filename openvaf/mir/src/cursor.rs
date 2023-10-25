//! Cursor library.
//!
//! This module defines cursor data types that can be used for inserting instructions.

use crate::builder::{InsertBuilder, InstInserterBase};
use crate::dfg::DataFlowGraph;
use crate::layout::Layout;
use crate::{Block, Function, Inst, SourceLoc};

/// The possible positions of a cursor.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CursorPosition {
    /// Cursor is not pointing anywhere. No instructions can be inserted.
    Nowhere,
    /// Cursor is pointing at an existing instruction.
    /// New instructions will be inserted *before* the current instruction.
    At(Inst),
    /// Cursor is before the beginning of a block. No instructions can be inserted. Calling
    /// `next_inst()` will move to the first instruction in the block.
    Before(Block),
    /// Cursor is pointing after the end of a block.
    /// New instructions will be appended to the block.
    After(Block),
}

/// All cursor types implement the `Cursor` which provides common navigation operations.
pub trait Cursor {
    /// Get the current cursor position.
    fn position(&self) -> CursorPosition;

    /// Set the current position.
    fn set_position(&mut self, pos: CursorPosition);

    /// Get the source location that should be assigned to new instructions.
    fn srcloc(&self) -> SourceLoc;

    /// Set the source location that should be assigned to new instructions.
    fn set_srcloc(&mut self, srcloc: SourceLoc);

    /// Borrow a reference to the function layout that this cursor is navigating.
    fn layout(&self) -> &Layout;

    /// Borrow a mutable reference to the function layout that this cursor is navigating.
    fn layout_mut(&mut self) -> &mut Layout;

    /// Exchange this cursor for one with a set source location.
    ///
    /// This is intended to be used as a builder method:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block, SourceLoc};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_func(func: &mut Function, srcloc: SourceLoc) {
    ///     let mut pos = FuncCursor::new(func).with_srcloc(srcloc);
    ///
    ///     // Use `pos`...
    /// }
    /// ```
    fn with_srcloc(mut self, srcloc: SourceLoc) -> Self
    where
        Self: Sized,
    {
        self.set_srcloc(srcloc);
        self
    }

    /// Rebuild this cursor positioned at `pos`.
    fn at_position(mut self, pos: CursorPosition) -> Self
    where
        Self: Sized,
    {
        self.set_position(pos);
        self
    }

    /// Rebuild this cursor positioned at `inst`.
    ///
    /// This is intended to be used as a builder method:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block, Inst};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_func(func: &mut Function, inst: Inst) {
    ///     let mut pos = FuncCursor::new(func).at_inst(inst);
    ///
    ///     // Use `pos`...
    /// }
    /// ```
    fn at_inst(mut self, inst: Inst) -> Self
    where
        Self: Sized,
    {
        self.goto_inst(inst);
        self
    }

    /// Rebuild this cursor positioned at the first insertion point for `block`.
    /// This differs from `at_first_inst` in that it doesn't assume that any
    /// instructions have been inserted into `block` yet.
    ///
    /// This is intended to be used as a builder method:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block, Inst};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_func(func: &mut Function, block: Block) {
    ///     let mut pos = FuncCursor::new(func).at_first_insertion_point(block);
    ///
    ///     // Use `pos`...
    /// }
    /// ```
    fn at_first_insertion_point(mut self, block: Block) -> Self
    where
        Self: Sized,
    {
        self.goto_first_insertion_point(block);
        self
    }

    /// Rebuild this cursor positioned at the first instruction in `block`.
    ///
    /// This is intended to be used as a builder method:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block, Inst};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_func(func: &mut Function, block: Block) {
    ///     let mut pos = FuncCursor::new(func).at_first_inst(block);
    ///
    ///     // Use `pos`...
    /// }
    /// ```
    fn at_first_inst(mut self, block: Block) -> Self
    where
        Self: Sized,
    {
        self.goto_first_inst(block);
        self
    }

    /// Rebuild this cursor positioned at the last instruction in `block`.
    fn at_last_inst(mut self, block: Block) -> Self
    where
        Self: Sized,
    {
        self.goto_last_inst(block);
        self
    }

    /// Rebuild this cursor positioned after `inst`.
    fn after_inst(mut self, inst: Inst) -> Self
    where
        Self: Sized,
    {
        self.goto_after_inst(inst);
        self
    }

    /// Rebuild this cursor positioned at the top of `block`.
    ///
    /// This is intended to be used as a builder method:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block, Inst};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_func(func: &mut Function, block: Block) {
    ///     let mut pos = FuncCursor::new(func).at_top(block);
    ///
    ///     // Use `pos`...
    /// }
    /// ```
    fn at_top(mut self, block: Block) -> Self
    where
        Self: Sized,
    {
        self.goto_top(block);
        self
    }

    /// Rebuild this cursor positioned at the bottom of `block`.
    ///
    /// This is intended to be used as a builder method:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block, Inst};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_func(func: &mut Function, block: Block) {
    ///     let mut pos = FuncCursor::new(func).at_bottom(block);
    ///
    ///     // Use `pos`...
    /// }
    /// ```
    fn at_bottom(mut self, block: Block) -> Self
    where
        Self: Sized,
    {
        self.goto_bottom(block);
        self
    }

    /// Rebuild this cursor positioned at the bottom of the last `block`.
    ///
    /// This is intended to be used as a builder method:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block, Inst};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_func(func: &mut Function, block: Block) {
    ///     let mut pos = FuncCursor::new(func).at_bottom(block);
    ///
    ///     // Use `pos`...
    /// }
    /// ```
    fn at_exit(mut self) -> Self
    where
        Self: Sized,
    {
        self.goto_exit();
        self
    }

    /// Go to the bottom of the exit `block`.
    /// At this position, inserted instructions will be appended to `block`.
    fn goto_exit(&mut self) {
        self.goto_bottom(self.layout().last_block().unwrap());
    }

    /// Get the block corresponding to the current position.
    fn current_block(&self) -> Option<Block> {
        use self::CursorPosition::*;
        match self.position() {
            Nowhere => None,
            At(inst) => self.layout().inst_block(inst),
            Before(block) | After(block) => Some(block),
        }
    }

    /// Get the instruction corresponding to the current position, if any.
    fn current_inst(&self) -> Option<Inst> {
        use self::CursorPosition::*;
        match self.position() {
            At(inst) => Some(inst),
            _ => None,
        }
    }

    /// Go to the position after a specific instruction, which must be inserted
    /// in the layout. New instructions will be inserted after `inst`.
    fn goto_after_inst(&mut self, inst: Inst) {
        debug_assert!(self.layout().inst_block(inst).is_some());
        let new_pos = if let Some(next) = self.layout().next_inst(inst) {
            CursorPosition::At(next)
        } else {
            CursorPosition::After(
                self.layout().inst_block(inst).expect("current instruction removed?"),
            )
        };
        self.set_position(new_pos);
    }

    /// Go to a specific instruction which must be inserted in the layout.
    /// New instructions will be inserted before `inst`.
    fn goto_inst(&mut self, inst: Inst) {
        debug_assert!(self.layout().inst_block(inst).is_some());
        self.set_position(CursorPosition::At(inst));
    }

    /// Go to the position for inserting instructions at the beginning of `block`,
    /// which unlike `goto_first_inst` doesn't assume that any instructions have
    /// been inserted into `block` yet.
    fn goto_first_insertion_point(&mut self, block: Block) {
        if let Some(inst) = self.layout().first_inst(block) {
            self.goto_inst(inst);
        } else {
            self.goto_bottom(block);
        }
    }

    /// Go to the first instruction in `block`.
    fn goto_first_inst(&mut self, block: Block) {
        let inst = self.layout().first_inst(block).expect("Empty block");
        self.goto_inst(inst);
    }

    /// Go to the last instruction in `block`.
    fn goto_last_inst(&mut self, block: Block) {
        let inst = self.layout().last_inst(block).expect("Empty block");
        self.goto_inst(inst);
    }

    /// Go to the top of `block` which must be inserted into the layout.
    /// At this position, instructions cannot be inserted, but `next_inst()` will move to the first
    /// instruction in `block`.
    fn goto_top(&mut self, block: Block) {
        debug_assert!(self.layout().is_block_inserted(block));
        self.set_position(CursorPosition::Before(block));
    }

    /// Go to the bottom of `block` which must be inserted into the layout.
    /// At this position, inserted instructions will be appended to `block`.
    fn goto_bottom(&mut self, block: Block) {
        debug_assert!(self.layout().is_block_inserted(block));
        self.set_position(CursorPosition::After(block));
    }

    /// Go to the top of the next block in layout order and return it.
    ///
    /// - If the cursor wasn't pointing at anything, go to the top of the first block in the
    ///   function.
    /// - If there are no more blocks, leave the cursor pointing at nothing and return `None`.
    ///
    /// # Examples
    ///
    /// The `next_block()` method is intended for iterating over the blocks in layout order:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_func(func: &mut Function) {
    ///     let mut cursor = FuncCursor::new(func);
    ///     while let Some(block) = cursor.next_block() {
    ///         // Edit block.
    ///     }
    /// }
    /// ```
    fn next_block(&mut self) -> Option<Block> {
        let next = if let Some(block) = self.current_block() {
            self.layout().next_block(block)
        } else {
            self.layout().entry_block()
        };
        self.set_position(match next {
            Some(block) => CursorPosition::Before(block),
            None => CursorPosition::Nowhere,
        });
        next
    }

    /// Go to the bottom of the previous block in layout order and return it.
    ///
    /// - If the cursor wasn't pointing at anything, go to the bottom of the last block in the
    ///   function.
    /// - If there are no more blocks, leave the cursor pointing at nothing and return `None`.
    ///
    /// # Examples
    ///
    /// The `prev_block()` method is intended for iterating over the blocks in backwards layout order:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_func(func: &mut Function) {
    ///     let mut cursor = FuncCursor::new(func);
    ///     while let Some(block) = cursor.prev_block() {
    ///         // Edit block.
    ///     }
    /// }
    /// ```
    fn prev_block(&mut self) -> Option<Block> {
        let prev = if let Some(block) = self.current_block() {
            self.layout().prev_block(block)
        } else {
            self.layout().last_block()
        };
        self.set_position(match prev {
            Some(block) => CursorPosition::After(block),
            None => CursorPosition::Nowhere,
        });
        prev
    }

    /// Move to the next instruction in the same block and return it.
    ///
    /// - If the cursor was positioned before a block, go to the first instruction in that block.
    /// - If there are no more instructions in the block, go to the `After(block)` position and return
    ///   `None`.
    /// - If the cursor wasn't pointing anywhere, keep doing that.
    ///
    /// This method will never move the cursor to a different block.
    ///
    /// # Examples
    ///
    /// The `next_inst()` method is intended for iterating over the instructions in a block like
    /// this:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_block(func: &mut Function, block: Block) {
    ///     let mut cursor = FuncCursor::new(func).at_top(block);
    ///     while let Some(inst) = cursor.next_inst() {
    ///         // Edit instructions...
    ///     }
    /// }
    /// ```
    /// The loop body can insert and remove instructions via the cursor.
    ///
    /// Iterating over all the instructions in a function looks like this:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_func(func: &mut Function) {
    ///     let mut cursor = FuncCursor::new(func);
    ///     while let Some(block) = cursor.next_block() {
    ///         while let Some(inst) = cursor.next_inst() {
    ///             // Edit instructions...
    ///         }
    ///     }
    /// }
    /// ```
    fn next_inst(&mut self) -> Option<Inst> {
        use self::CursorPosition::*;
        match self.position() {
            Nowhere | After(..) => None,
            At(inst) => {
                if let Some(next) = self.layout().next_inst(inst) {
                    self.set_position(At(next));
                    Some(next)
                } else {
                    let pos = After(
                        self.layout().inst_block(inst).expect("current instruction removed?"),
                    );
                    self.set_position(pos);
                    None
                }
            }
            Before(block) => {
                if let Some(next) = self.layout().first_inst(block) {
                    self.set_position(At(next));
                    Some(next)
                } else {
                    self.set_position(After(block));
                    None
                }
            }
        }
    }

    /// Move to the previous instruction in the same block and return it.
    ///
    /// - If the cursor was positioned after a block, go to the last instruction in that block.
    /// - If there are no more instructions in the block, go to the `Before(block)` position and return
    ///   `None`.
    /// - If the cursor wasn't pointing anywhere, keep doing that.
    ///
    /// This method will never move the cursor to a different block.
    ///
    /// # Examples
    ///
    /// The `prev_inst()` method is intended for iterating backwards over the instructions in an
    /// block like this:
    ///
    /// ```
    /// # use cranelift_codegen::{Function, Block};
    /// # use cranelift_codegen::cursor::{Cursor, FuncCursor};
    /// fn edit_block(func: &mut Function, block: Block) {
    ///     let mut cursor = FuncCursor::new(func).at_bottom(block);
    ///     while let Some(inst) = cursor.prev_inst() {
    ///         // Edit instructions...
    ///     }
    /// }
    /// ```
    fn prev_inst(&mut self) -> Option<Inst> {
        use self::CursorPosition::*;
        match self.position() {
            Nowhere | Before(..) => None,
            At(inst) => {
                if let Some(prev) = self.layout().prev_inst(inst) {
                    self.set_position(At(prev));
                    Some(prev)
                } else {
                    let pos = Before(
                        self.layout().inst_block(inst).expect("current instruction removed?"),
                    );
                    self.set_position(pos);
                    None
                }
            }
            After(block) => {
                if let Some(prev) = self.layout().last_inst(block) {
                    self.set_position(At(prev));
                    Some(prev)
                } else {
                    self.set_position(Before(block));
                    None
                }
            }
        }
    }

    /// Insert an instruction at the current position.
    ///
    /// - If pointing at an instruction, the new instruction is inserted before the current
    ///   instruction.
    /// - If pointing at the bottom of a block, the new instruction is appended to the block.
    /// - Otherwise panic.
    ///
    /// In either case, the cursor is not moved, such that repeated calls to `insert_inst()` causes
    /// instructions to appear in insertion order in the block.
    fn insert_inst(&mut self, inst: Inst) {
        use self::CursorPosition::*;
        match self.position() {
            Nowhere | Before(..) => panic!("Invalid insert_inst position"),
            At(cur) => self.layout_mut().prepend_inst(inst, cur),
            After(block) => self.layout_mut().append_inst_to_bb(inst, block),
        }
    }

    /// Remove the instruction under the cursor.
    ///
    /// The cursor is left pointing at the position following the current instruction.
    ///
    /// Return the instruction that was removed.
    fn remove_inst(&mut self) -> Inst {
        let inst = self.current_inst().expect("No instruction to remove");
        self.next_inst();
        self.layout_mut().remove_inst(inst);
        inst
    }

    /// Remove the instruction under the cursor.
    ///
    /// The cursor is left pointing at the position preceding the current instruction.
    ///
    /// Return the instruction that was removed.
    fn remove_inst_and_step_back(&mut self) -> Inst {
        let inst = self.current_inst().expect("No instruction to remove");
        self.prev_inst();
        self.layout_mut().remove_inst(inst);
        inst
    }
}

/// Function cursor.
///
/// A `FuncCursor` holds a mutable reference to a whole `Function` while keeping a position
/// too. The function can be re-borrowed by accessing the public `cur.func` member.
///
/// This cursor is for use before legalization. The inserted instructions are not given an
/// encoding.
pub struct FuncCursor<'f> {
    pos: CursorPosition,
    srcloc: SourceLoc,

    /// The referenced function.
    pub func: &'f mut Function,
}

impl<'f> FuncCursor<'f> {
    /// Create a new `FuncCursor` pointing nowhere.
    pub fn new(func: &'f mut Function) -> Self {
        Self { pos: CursorPosition::Nowhere, srcloc: Default::default(), func }
    }

    /// Use the source location of `inst` for future instructions.
    pub fn use_srcloc(&mut self, inst: Inst) {
        self.srcloc = self.func.srclocs[inst];
    }

    /// Create an instruction builder that inserts an instruction at the current position.
    pub fn ins(&mut self) -> InsertBuilder<&mut FuncCursor<'f>> {
        InsertBuilder::new(self)
    }

    /// Rebuild this cursor positioned after `inst`.
    /// If `inst` is a phi instruction this function will advance until the following instruction
    /// is not a phi instruction.
    ///
    /// This is intended for situtions where an instruction will be inserted after the definition
    /// of an unknown value.
    pub fn after_inst_no_phi(self, mut inst: Inst) -> Self
    where
        Self: Sized,
    {
        while let Some(next) = self.func.layout.next_inst(inst) {
            if self.func.dfg.insts[next].is_phi() {
                inst = next;
            } else {
                break;
            }
        }
        self.after_inst(inst)
    }
}

impl<'f> Cursor for FuncCursor<'f> {
    fn position(&self) -> CursorPosition {
        self.pos
    }

    fn set_position(&mut self, pos: CursorPosition) {
        self.pos = pos
    }

    fn srcloc(&self) -> SourceLoc {
        self.srcloc
    }

    fn set_srcloc(&mut self, srcloc: SourceLoc) {
        self.srcloc = srcloc;
    }

    fn layout(&self) -> &Layout {
        &self.func.layout
    }

    fn layout_mut(&mut self) -> &mut Layout {
        &mut self.func.layout
    }
}

impl<'c, 'f> InstInserterBase<'c> for &'c mut FuncCursor<'f> {
    fn data_flow_graph(&self) -> &DataFlowGraph {
        &self.func.dfg
    }

    fn data_flow_graph_mut(&mut self) -> &mut DataFlowGraph {
        &mut self.func.dfg
    }

    fn insert_built_inst(self, inst: Inst) -> &'c mut DataFlowGraph {
        self.insert_inst(inst);
        if !self.srcloc.is_default() {
            self.func.srclocs[inst] = self.srcloc;
        }
        &mut self.func.dfg
    }
}
