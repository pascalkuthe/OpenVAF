use cranelift_entity::EntityRef;

use super::Layout;
use crate::cursor::{Cursor, CursorPosition};
use crate::{Block, Inst, SourceLoc};

struct LayoutCursor<'f> {
    /// Borrowed function layout. Public so it can be re-borrowed from this cursor.
    pub layout: &'f mut Layout,
    pos: CursorPosition,
}

impl<'f> Cursor for LayoutCursor<'f> {
    fn position(&self) -> CursorPosition {
        self.pos
    }

    fn set_position(&mut self, pos: CursorPosition) {
        self.pos = pos;
    }

    fn srcloc(&self) -> SourceLoc {
        unimplemented!()
    }

    fn set_srcloc(&mut self, _srcloc: SourceLoc) {
        unimplemented!()
    }

    fn layout(&self) -> &Layout {
        self.layout
    }

    fn layout_mut(&mut self) -> &mut Layout {
        self.layout
    }
}

impl<'f> LayoutCursor<'f> {
    /// Create a new `LayoutCursor` for `layout`.
    /// The cursor holds a mutable reference to `layout` for its entire lifetime.
    pub fn new(layout: &'f mut Layout) -> Self {
        Self { layout, pos: CursorPosition::Nowhere }
    }
}

fn verify(layout: &mut Layout, blocks: &[(Block, &[Inst])]) {
    // Check that blocks are inserted and instructions belong the right places.
    // Check forward linkage with iterators.
    {
        let mut block_iter = layout.blocks();
        for &(block, insts) in blocks {
            assert!(layout.is_block_inserted(block));
            assert_eq!(block_iter.next(), Some(block));

            let mut inst_iter = layout.block_insts(block);
            for &inst in insts {
                assert_eq!(layout.inst_block(inst), Some(block));
                assert_eq!(inst_iter.next(), Some(inst));
            }
            assert_eq!(inst_iter.next(), None);
        }
        assert_eq!(block_iter.next(), None);
    }

    // Check backwards linkage with a cursor.
    let mut cur = LayoutCursor::new(layout);
    for &(block, insts) in blocks.iter().rev() {
        assert_eq!(cur.prev_block(), Some(block));
        for &inst in insts.iter().rev() {
            assert_eq!(cur.prev_inst(), Some(inst));
        }
        assert_eq!(cur.prev_inst(), None);
    }
    assert_eq!(cur.prev_block(), None);
}

#[test]
fn append_block() {
    let mut layout = Layout::new();
    let e0 = Block::new(0);
    let e1 = Block::new(1);
    let e2 = Block::new(2);

    {
        let imm = &layout;
        assert!(!imm.is_block_inserted(e0));
        assert!(!imm.is_block_inserted(e1));
    }
    verify(&mut layout, &[]);

    layout.append_block(e1);
    assert!(!layout.is_block_inserted(e0));
    assert!(layout.is_block_inserted(e1));
    assert!(!layout.is_block_inserted(e2));
    let v: Vec<Block> = layout.blocks().collect();
    assert_eq!(v, [e1]);

    layout.append_block(e2);
    assert!(!layout.is_block_inserted(e0));
    assert!(layout.is_block_inserted(e1));
    assert!(layout.is_block_inserted(e2));
    let v: Vec<Block> = layout.blocks().collect();
    assert_eq!(v, [e1, e2]);

    layout.append_block(e0);
    assert!(layout.is_block_inserted(e0));
    assert!(layout.is_block_inserted(e1));
    assert!(layout.is_block_inserted(e2));
    let v: Vec<Block> = layout.blocks().collect();
    assert_eq!(v, [e1, e2, e0]);

    {
        let imm = &layout;
        let mut v = Vec::new();
        for e in imm {
            v.push(e);
        }
        assert_eq!(v, [e1, e2, e0]);
    }

    // Test cursor positioning.
    let mut cur = LayoutCursor::new(&mut layout);
    assert_eq!(cur.position(), CursorPosition::Nowhere);
    assert_eq!(cur.next_inst(), None);
    assert_eq!(cur.position(), CursorPosition::Nowhere);
    assert_eq!(cur.prev_inst(), None);
    assert_eq!(cur.position(), CursorPosition::Nowhere);

    assert_eq!(cur.next_block(), Some(e1));
    assert_eq!(cur.position(), CursorPosition::Before(e1));
    assert_eq!(cur.next_inst(), None);
    assert_eq!(cur.position(), CursorPosition::After(e1));
    assert_eq!(cur.next_inst(), None);
    assert_eq!(cur.position(), CursorPosition::After(e1));
    assert_eq!(cur.next_block(), Some(e2));
    assert_eq!(cur.prev_inst(), None);
    assert_eq!(cur.position(), CursorPosition::Before(e2));
    assert_eq!(cur.next_block(), Some(e0));
    assert_eq!(cur.next_block(), None);
    assert_eq!(cur.position(), CursorPosition::Nowhere);

    // Backwards through the blocks.
    assert_eq!(cur.prev_block(), Some(e0));
    assert_eq!(cur.position(), CursorPosition::After(e0));
    assert_eq!(cur.prev_block(), Some(e2));
    assert_eq!(cur.prev_block(), Some(e1));
    assert_eq!(cur.prev_block(), None);
    assert_eq!(cur.position(), CursorPosition::Nowhere);
}

#[test]
fn insert_block() {
    let mut layout = Layout::new();
    let e0 = Block::new(0);
    let e1 = Block::new(1);
    let e2 = Block::new(2);

    {
        let imm = &layout;
        assert!(!imm.is_block_inserted(e0));
        assert!(!imm.is_block_inserted(e1));

        let v: Vec<Block> = layout.blocks().collect();
        assert_eq!(v, []);
    }

    layout.append_block(e1);
    assert!(!layout.is_block_inserted(e0));
    assert!(layout.is_block_inserted(e1));
    assert!(!layout.is_block_inserted(e2));
    verify(&mut layout, &[(e1, &[])]);

    layout.insert_block(e2, e1);
    assert!(!layout.is_block_inserted(e0));
    assert!(layout.is_block_inserted(e1));
    assert!(layout.is_block_inserted(e2));
    verify(&mut layout, &[(e2, &[]), (e1, &[])]);

    layout.insert_block(e0, e1);
    assert!(layout.is_block_inserted(e0));
    assert!(layout.is_block_inserted(e1));
    assert!(layout.is_block_inserted(e2));
    verify(&mut layout, &[(e2, &[]), (e0, &[]), (e1, &[])]);
}

#[test]
fn insert_block_after() {
    let mut layout = Layout::new();
    let e0 = Block::new(0);
    let e1 = Block::new(1);
    let e2 = Block::new(2);

    layout.append_block(e1);
    layout.insert_block_after(e2, e1);
    verify(&mut layout, &[(e1, &[]), (e2, &[])]);

    layout.insert_block_after(e0, e1);
    verify(&mut layout, &[(e1, &[]), (e0, &[]), (e2, &[])]);
}

#[test]
fn append_inst() {
    let mut layout = Layout::new();
    let e1 = Block::new(1);

    layout.append_block(e1);
    let v: Vec<Inst> = layout.block_insts(e1).collect();
    assert_eq!(v, []);

    let i0 = Inst::new(0);
    let i1 = Inst::new(1);
    let i2 = Inst::new(2);

    assert_eq!(layout.inst_block(i0), None);
    assert_eq!(layout.inst_block(i1), None);
    assert_eq!(layout.inst_block(i2), None);

    layout.append_inst(i1, e1);
    assert_eq!(layout.inst_block(i0), None);
    assert_eq!(layout.inst_block(i1), Some(e1));
    assert_eq!(layout.inst_block(i2), None);
    let v: Vec<Inst> = layout.block_insts(e1).collect();
    assert_eq!(v, [i1]);

    layout.append_inst(i2, e1);
    assert_eq!(layout.inst_block(i0), None);
    assert_eq!(layout.inst_block(i1), Some(e1));
    assert_eq!(layout.inst_block(i2), Some(e1));
    let v: Vec<Inst> = layout.block_insts(e1).collect();
    assert_eq!(v, [i1, i2]);

    // Test double-ended instruction iterator.
    let v: Vec<Inst> = layout.block_insts(e1).rev().collect();
    assert_eq!(v, [i2, i1]);

    layout.append_inst(i0, e1);
    verify(&mut layout, &[(e1, &[i1, i2, i0])]);

    // Test cursor positioning.
    let mut cur = LayoutCursor::new(&mut layout).at_top(e1);
    assert_eq!(cur.position(), CursorPosition::Before(e1));
    assert_eq!(cur.prev_inst(), None);
    assert_eq!(cur.position(), CursorPosition::Before(e1));
    assert_eq!(cur.next_inst(), Some(i1));
    assert_eq!(cur.position(), CursorPosition::At(i1));
    assert_eq!(cur.next_inst(), Some(i2));
    assert_eq!(cur.next_inst(), Some(i0));
    assert_eq!(cur.prev_inst(), Some(i2));
    assert_eq!(cur.position(), CursorPosition::At(i2));
    assert_eq!(cur.next_inst(), Some(i0));
    assert_eq!(cur.position(), CursorPosition::At(i0));
    assert_eq!(cur.next_inst(), None);
    assert_eq!(cur.position(), CursorPosition::After(e1));
    assert_eq!(cur.next_inst(), None);
    assert_eq!(cur.position(), CursorPosition::After(e1));
    assert_eq!(cur.prev_inst(), Some(i0));
    assert_eq!(cur.prev_inst(), Some(i2));
    assert_eq!(cur.prev_inst(), Some(i1));
    assert_eq!(cur.prev_inst(), None);
    assert_eq!(cur.position(), CursorPosition::Before(e1));

    // Test remove_inst.
    cur.goto_inst(i2);
    assert_eq!(cur.remove_inst(), i2);
    verify(cur.layout, &[(e1, &[i1, i0])]);
    assert_eq!(cur.layout.inst_block(i2), None);
    assert_eq!(cur.remove_inst(), i0);
    verify(cur.layout, &[(e1, &[i1])]);
    assert_eq!(cur.layout.inst_block(i0), None);
    assert_eq!(cur.position(), CursorPosition::After(e1));
    cur.layout.remove_inst(i1);
    verify(cur.layout, &[(e1, &[])]);
    assert_eq!(cur.layout.inst_block(i1), None);
}

#[test]
fn insert_inst() {
    let mut layout = Layout::new();
    let e1 = Block::new(1);

    layout.append_block(e1);
    let v: Vec<Inst> = layout.block_insts(e1).collect();
    assert_eq!(v, []);

    let i0 = Inst::new(0);
    let i1 = Inst::new(1);
    let i2 = Inst::new(2);

    assert_eq!(layout.inst_block(i0), None);
    assert_eq!(layout.inst_block(i1), None);
    assert_eq!(layout.inst_block(i2), None);

    layout.append_inst(i1, e1);
    assert_eq!(layout.inst_block(i0), None);
    assert_eq!(layout.inst_block(i1), Some(e1));
    assert_eq!(layout.inst_block(i2), None);
    let v: Vec<Inst> = layout.block_insts(e1).collect();
    assert_eq!(v, [i1]);

    layout.insert_inst(i2, i1);
    assert_eq!(layout.inst_block(i0), None);
    assert_eq!(layout.inst_block(i1), Some(e1));
    assert_eq!(layout.inst_block(i2), Some(e1));
    let v: Vec<Inst> = layout.block_insts(e1).collect();
    assert_eq!(v, [i2, i1]);

    layout.insert_inst(i0, i1);
    verify(&mut layout, &[(e1, &[i2, i0, i1])]);
}

#[test]
fn multiple_blocks() {
    let mut layout = Layout::new();

    let e0 = Block::new(0);
    let e1 = Block::new(1);

    assert_eq!(layout.entry_block(), None);
    layout.append_block(e0);
    assert_eq!(layout.entry_block(), Some(e0));
    layout.append_block(e1);
    assert_eq!(layout.entry_block(), Some(e0));

    let i0 = Inst::new(0);
    let i1 = Inst::new(1);
    let i2 = Inst::new(2);
    let i3 = Inst::new(3);

    layout.append_inst(i0, e0);
    layout.append_inst(i1, e0);
    layout.append_inst(i2, e1);
    layout.append_inst(i3, e1);

    let v0: Vec<Inst> = layout.block_insts(e0).collect();
    let v1: Vec<Inst> = layout.block_insts(e1).collect();
    assert_eq!(v0, [i0, i1]);
    assert_eq!(v1, [i2, i3]);
}

#[test]
fn split_block() {
    let mut layout = Layout::new();

    let e0 = Block::new(0);
    let e1 = Block::new(1);
    let e2 = Block::new(2);

    let i0 = Inst::new(0);
    let i1 = Inst::new(1);
    let i2 = Inst::new(2);
    let i3 = Inst::new(3);

    layout.append_block(e0);
    layout.append_inst(i0, e0);
    assert_eq!(layout.inst_block(i0), Some(e0));
    layout.split_block(e1, i0);
    assert_eq!(layout.inst_block(i0), Some(e1));

    {
        let mut cur = LayoutCursor::new(&mut layout);
        assert_eq!(cur.next_block(), Some(e0));
        assert_eq!(cur.next_inst(), None);
        assert_eq!(cur.next_block(), Some(e1));
        assert_eq!(cur.next_inst(), Some(i0));
        assert_eq!(cur.next_inst(), None);
        assert_eq!(cur.next_block(), None);

        // Check backwards links.
        assert_eq!(cur.prev_block(), Some(e1));
        assert_eq!(cur.prev_inst(), Some(i0));
        assert_eq!(cur.prev_inst(), None);
        assert_eq!(cur.prev_block(), Some(e0));
        assert_eq!(cur.prev_inst(), None);
        assert_eq!(cur.prev_block(), None);
    }

    layout.append_inst(i1, e0);
    layout.append_inst(i2, e0);
    layout.append_inst(i3, e0);
    layout.split_block(e2, i2);

    assert_eq!(layout.inst_block(i0), Some(e1));
    assert_eq!(layout.inst_block(i1), Some(e0));
    assert_eq!(layout.inst_block(i2), Some(e2));
    assert_eq!(layout.inst_block(i3), Some(e2));

    {
        let mut cur = LayoutCursor::new(&mut layout);
        assert_eq!(cur.next_block(), Some(e0));
        assert_eq!(cur.next_inst(), Some(i1));
        assert_eq!(cur.next_inst(), None);
        assert_eq!(cur.next_block(), Some(e2));
        assert_eq!(cur.next_inst(), Some(i2));
        assert_eq!(cur.next_inst(), Some(i3));
        assert_eq!(cur.next_inst(), None);
        assert_eq!(cur.next_block(), Some(e1));
        assert_eq!(cur.next_inst(), Some(i0));
        assert_eq!(cur.next_inst(), None);
        assert_eq!(cur.next_block(), None);

        assert_eq!(cur.prev_block(), Some(e1));
        assert_eq!(cur.prev_inst(), Some(i0));
        assert_eq!(cur.prev_inst(), None);
        assert_eq!(cur.prev_block(), Some(e2));
        assert_eq!(cur.prev_inst(), Some(i3));
        assert_eq!(cur.prev_inst(), Some(i2));
        assert_eq!(cur.prev_inst(), None);
        assert_eq!(cur.prev_block(), Some(e0));
        assert_eq!(cur.prev_inst(), Some(i1));
        assert_eq!(cur.prev_inst(), None);
        assert_eq!(cur.prev_block(), None);
    }
}
