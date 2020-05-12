//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::compact_arena::{Idx16, InvariantLifetime, Step, TinyHeapArena};
use crate::ir::{IntegerExpressionId, StatementId};
use bitflags::_core::mem::swap;
use bitflags::_core::ops::{Index, IndexMut};
use fixedbitset::FixedBitSet as BitSet;
use log::debug;
use log::trace;
use rustc_hash::FxHashMap;

#[macro_export]
macro_rules! simplify {
    ($cfg:ident) => {
        let tag = $crate::compact_arena::invariant_lifetime();
        let _guard;
        let mut $cfg = unsafe {
            // this is not per-se unsafe but we need it to be public and
            // calling it with a non-unique `tag` would allow arena mixups,
            // which may introduce UB in `Index`/`IndexMut`
            $cfg.simplify(tag)
        };
        // this doesn't make it to MIR, but ensures that borrowck will not
        // unify the lifetimes of two macro calls by binding the lifetime to
        // drop scope
        if false {
            struct Guard<'tag>(&'tag $crate::compact_arena::InvariantLifetime<'tag>);
            impl<'tag> ::core::ops::Drop for Guard<'tag> {
                fn drop(&mut self) {}
            }
            _guard = Guard(&tag);
        }
    };
}

pub type BasicBlockId<'tag> = Idx16<'tag>;
pub struct SimplifiedControlFlowGraph<'tag, 'mir> {
    pub blocks: TinyHeapArena<'tag, BasicBlock<'tag, 'mir>>,
}

impl<'tag, 'mir> SimplifiedControlFlowGraph<'tag, 'mir> {
    #[inline(always)]
    pub fn start(&self) -> BasicBlockId<'tag> {
        unsafe { self.blocks.end() } //This is save since we treat this as an inclusive range
    }

    #[inline(always)]
    pub fn end(&self) -> BasicBlockId<'tag> {
        unsafe { self.blocks.start() } //This is save since we treat this as an inclusive range
    }

    pub fn block_count(&self) -> u16 {
        self.blocks.len()
    }

    pub fn visit_in_execution_order(&self, mut f: impl FnMut(BasicBlockId<'tag>)) {
        self.partial_visit_in_execution_order(self.start(), None, &mut f)
    }

    pub fn partial_visit_in_execution_order(
        &self,
        start: BasicBlockId<'tag>,
        end: Option<BasicBlockId<'tag>>,
        f: &mut impl FnMut(BasicBlockId<'tag>),
    ) {
        let mut current = start;
        loop {
            f(current);
            current = match self.blocks[current].terminator {
                Terminator::End => return,

                Terminator::Goto(next) | Terminator::Merge(next) if Some(next) == end => return,

                Terminator::Goto(next) | Terminator::Merge(next) => next,

                Terminator::Split {
                    condition: _,
                    true_block,
                    false_block,
                    merge,
                } => {
                    self.partial_visit_in_execution_order(true_block, Some(merge), f);
                    if merge == current {
                        //loops
                        false_block
                    } else {
                        self.partial_visit_in_execution_order(false_block, Some(merge), f);
                        merge
                    }
                }
            };
        }
    }
}
#[derive(Debug)]
pub struct ControlFlowGraph<'tag, 'mir> {
    pub blocks: TinyHeapArena<'tag, BasicBlock<'tag, 'mir>>,
    pub(crate) dead_blocks: BitSet,
}

impl<'tag, 'mir> ControlFlowGraph<'tag, 'mir> {
    #[inline(always)]
    pub fn start(&self) -> BasicBlockId<'tag> {
        unsafe { self.blocks.end() } //This is save since we treat this as an inclusive range
    }

    #[inline(always)]
    pub fn end(&self) -> BasicBlockId<'tag> {
        unsafe { self.blocks.start() } //This is save since we treat this as an inclusive range
    }

    pub fn block_count(&self) -> u16 {
        self.blocks.len()
    }

    pub unsafe fn simplify<'newtag>(
        mut self,
        new_tag: InvariantLifetime<'newtag>,
    ) -> SimplifiedControlFlowGraph<'newtag, 'mir> {
        let mut new_replacements: FxHashMap<BasicBlockId, BasicBlockId> = FxHashMap::default();
        let mut replacements = new_replacements.clone();

        loop {
            let mut replacement_targets = BitSet::with_capacity(self.block_count() as usize);
            let mut changed = false;

            for block in self.blocks.full_range() {
                if self.dead_blocks.contains(block.index() as usize) {
                    continue;
                }

                match self.blocks[block].terminator {
                    Terminator::End => (),
                    Terminator::Merge(ref mut next) | Terminator::Goto(ref mut next) => {
                        if let Some(&new_next) = replacements.get(&next) {
                            *next = new_next
                        }

                        let next = *next;
                        if self.blocks[block].statements.is_empty()
                            && !replacement_targets.contains(block.index() as usize)
                            && !new_replacements.contains_key(&next)
                        {
                            new_replacements.insert(block, next);
                            self.dead_blocks.insert(block.index() as usize);
                            replacement_targets.insert(next.index() as usize);
                        }
                    }

                    Terminator::Split {
                        condition,
                        mut true_block,
                        mut false_block,
                        mut merge,
                    } => {
                        if let Some(&new_true_block) = replacements.get(&true_block) {
                            true_block = new_true_block
                        }

                        if let Some(&new_false_block) = replacements.get(&false_block) {
                            false_block = new_false_block
                        }

                        if let Some(&new_merge) = replacements.get(&merge) {
                            merge = new_merge
                        }

                        self.blocks[block].terminator = if true_block == false_block {
                            //empty condition
                            changed = true;
                            Terminator::Merge(merge)
                        } else if true_block == block {
                            //empty loop
                            changed = true;
                            Terminator::Merge(false_block)
                        } else {
                            Terminator::Split {
                                condition,
                                true_block,
                                false_block,
                                merge,
                            }
                        };
                    }
                };
            }

            if new_replacements.is_empty() && !changed {
                break;
            }

            trace!(
                "running because changed: {} or replacements: {:#?}",
                changed,
                new_replacements
            );

            swap(&mut new_replacements, &mut replacements);
            new_replacements.clear();
        }
        let res = self.remove_dead_code(new_tag);

        SimplifiedControlFlowGraph { blocks: res.blocks }
    }

    pub unsafe fn remove_dead_code<'newtag>(
        mut self,
        _tag: InvariantLifetime<'newtag>,
    ) -> ControlFlowGraph<'newtag, 'mir> {
        let mut replacements = FxHashMap::default();
        let dead_blocks = &self.dead_blocks;
        self.blocks.retain(
            |block| !dead_blocks.contains(block.index() as usize),
            |old_id, new_id| {
                replacements.insert(old_id, new_id);
            },
        );

        for block in self.blocks.full_range() {
            match self.blocks[block].terminator {
                Terminator::End => (),

                Terminator::Merge(ref mut next) | Terminator::Goto(ref mut next) => {
                    //split into two loops
                    if let Some(&new_next) = replacements.get(&next) {
                        *next = new_next
                    }
                }
                Terminator::Split {
                    condition,
                    ref mut true_block,
                    ref mut false_block,
                    ref mut merge,
                } => {
                    if let Some(&new_true_block) = replacements.get(true_block) {
                        *true_block = new_true_block
                    }

                    if let Some(&new_false_block) = replacements.get(false_block) {
                        *false_block = new_false_block
                    }

                    if let Some(&new_merge) = replacements.get(merge) {
                        *merge = new_merge
                    }
                }
            }
        }

        self.dead_blocks.clear();
        std::mem::transmute(self)
    }

    pub fn clone_into<'newtag>(
        &self,
        allocator: TinyHeapArena<'newtag, BasicBlock<'newtag, 'mir>>,
    ) -> ControlFlowGraph<'newtag, 'mir> {
        let mut allocator: TinyHeapArena<'tag, BasicBlock<'tag, 'mir>> =
            unsafe { std::mem::transmute(allocator) };
        self.blocks.clone_into(&mut allocator);
        ControlFlowGraph {
            blocks: unsafe { std::mem::transmute(allocator) },
            dead_blocks: self.dead_blocks.clone(),
        }
    }

    pub fn for_all_blocks(&self, mut f: impl FnMut(BasicBlockId<'tag>)) {
        self.blocks.full_range().for_each(|block| f(block));
    }

    pub fn for_all_blocks_mut(&mut self, mut f: impl FnMut(&mut Self, BasicBlockId<'tag>)) {
        self.blocks.full_range().for_each(|block| f(self, block));
    }

    pub fn visit_mut_in_execution_order(
        &mut self,
        mut f: impl FnMut(&mut Self, BasicBlockId<'tag>, Option<BasicBlockId>),
    ) {
        self.partial_visit_mut_in_execution_order(self.start(), None, &mut f)
    }
    pub fn partial_visit_mut_in_execution_order(
        &mut self,
        start: BasicBlockId<'tag>,
        end: Option<BasicBlockId<'tag>>,
        f: &mut impl FnMut(&mut Self, BasicBlockId<'tag>, Option<BasicBlockId<'tag>>),
    ) {
        let mut current = start;
        loop {
            f(self, current, end);
            current = match self.blocks[current].terminator {
                Terminator::End => return,
                Terminator::Merge(next) if Some(next) == end => return,

                Terminator::Goto(next) | Terminator::Merge(next) => next,

                Terminator::Split {
                    condition: _,
                    true_block,
                    false_block,
                    merge,
                } => {
                    self.partial_visit_mut_in_execution_order(true_block, Some(merge), f);
                    if merge == current {
                        //loops
                        false_block
                    } else {
                        self.partial_visit_mut_in_execution_order(false_block, Some(merge), f);
                        merge
                    }
                }
            };
        }
    }
    pub fn mark_block_dead(&mut self, block: BasicBlockId<'tag>) {
        self.dead_blocks.insert(block.index() as usize)
    }

    pub fn visit_in_execution_order(&self, mut f: impl FnMut(BasicBlockId<'tag>)) {
        self.partial_visit_in_execution_order(self.start(), None, &mut f)
    }

    pub fn partial_visit_in_execution_order(
        &self,
        start: BasicBlockId<'tag>,
        end: Option<BasicBlockId<'tag>>,
        f: &mut impl FnMut(BasicBlockId<'tag>),
    ) {
        let mut current = start;
        loop {
            f(current);
            current = match self.blocks[current].terminator {
                Terminator::End => return,
                Terminator::Merge(next) if Some(next) == end => return,

                Terminator::Goto(next) | Terminator::Merge(next) => next,

                Terminator::Split {
                    condition: _,
                    true_block,
                    false_block,
                    merge,
                } => {
                    self.partial_visit_in_execution_order(true_block, Some(merge), f);
                    if merge == current {
                        //loops
                        false_block
                    } else {
                        self.partial_visit_in_execution_order(false_block, Some(merge), f);
                        merge
                    }
                }
            };
        }
    }
}

impl<'cfg, 'mir> Index<BasicBlockId<'cfg>> for ControlFlowGraph<'cfg, 'mir> {
    type Output = BasicBlock<'cfg, 'mir>;

    fn index(&self, index: BasicBlockId<'cfg>) -> &Self::Output {
        &self.blocks[index]
    }
}

impl<'cfg, 'mir> IndexMut<BasicBlockId<'cfg>> for ControlFlowGraph<'cfg, 'mir> {
    fn index_mut(&mut self, index: BasicBlockId<'cfg>) -> &mut Self::Output {
        &mut self.blocks[index]
    }
}

#[derive(Clone, Debug)]
pub struct BasicBlock<'tag, 'mir> {
    pub statements: Vec<StatementId<'mir>>,
    pub terminator: Terminator<'tag, 'mir>,
}

#[derive(Copy, Clone, Debug)]
pub enum Terminator<'tag, 'mir> {
    Goto(BasicBlockId<'tag>),
    Merge(BasicBlockId<'tag>),
    Split {
        condition: IntegerExpressionId<'mir>,
        true_block: BasicBlockId<'tag>,
        false_block: BasicBlockId<'tag>,
        merge: BasicBlockId<'tag>,
    },
    End,
}
impl<'tag, 'mir> Terminator<'tag, 'mir> {
    unsafe fn offset<'newtag>(
        mut self,
        offset: <BasicBlockId as Step>::I,
    ) -> Terminator<'newtag, 'mir> {
        match self {
            Self::Merge(mut next) => {
                next.step(offset);
                Terminator::Merge(next.transmute())
            }
            Self::Goto(mut next) => {
                next.step(offset);
                Terminator::Goto(next.transmute())
            }
            Self::Split {
                condition,
                mut true_block,
                mut false_block,
                mut merge,
            } => {
                true_block.step(offset);
                false_block.step(offset);
                merge.step(offset);
                Terminator::Split {
                    condition,
                    true_block: true_block.transmute(),
                    false_block: false_block.transmute(),
                    merge: merge.transmute(),
                }
            }
            Self::End => Terminator::End,
        }
    }
    unsafe fn neg_offset<'newtag>(
        mut self,
        offset: <BasicBlockId as Step>::I,
    ) -> Terminator<'newtag, 'mir> {
        match self {
            Self::Goto(mut next) => {
                next.step_back(offset);
                Terminator::Goto(next.transmute())
            }
            Self::Merge(mut next) => {
                next.step_back(offset);
                Terminator::Merge(next.transmute())
            }
            Self::Split {
                condition,
                mut true_block,
                mut false_block,
                mut merge,
            } => {
                true_block.step_back(offset);
                false_block.step_back(offset);
                merge.step_back(offset);
                Terminator::Split {
                    condition,
                    true_block: true_block.transmute(),
                    false_block: false_block.transmute(),
                    merge: merge.transmute(),
                }
            }
            Self::End => Terminator::End,
        }
    }
}

#[cfg(feature = "graph_debug")]
mod debug {
    use super::*;
    use rustc_ap_graphviz as dot;
    use rustc_ap_graphviz::LabelText::{EscStr, LabelStr};
    use rustc_ap_graphviz::{Edges, GraphWalk, Id, LabelText, Labeller, Nodes};
    use std::borrow::Cow;
    use std::io::Write;

    impl<'tag, 'mir> SimplifiedControlFlowGraph<'tag, 'mir> {
        pub fn render_to<W: Write>(&self, write: &mut W) {
            dot::render(self, write).expect("Rendering failed")
        }
    }

    impl<'tag, 'mir> ControlFlowGraph<'tag, 'mir> {
        pub fn render_to<W: Write>(&self, write: &mut W) {
            dot::render(self, write).expect("Rendering failed")
        }

        pub fn render_dominators_to<W: Write>(&self, write: &mut W) {
            dot::render(&DominatorTreeControlFlowRender { cfg: self }, write)
                .expect("Rendering failed")
        }
    }

    impl<'a, 'tag, 'mir> dot::Labeller<'a> for ControlFlowGraph<'tag, 'mir> {
        type Node = BasicBlockId<'tag>;
        type Edge = (BasicBlockId<'tag>, BasicBlockId<'tag>);

        fn graph_id(&'a self) -> Id<'a> {
            dot::Id::new("ControlFlowGraph").unwrap()
        }

        fn node_id(&'a self, n: &Self::Node) -> Id<'a> {
            dot::Id::new(format!("BB_{}", n.index())).unwrap()
        }

        fn node_label(&'a self, &n: &Self::Node) -> LabelText<'a> {
            let dead = self.dead_blocks.contains(n.index() as usize);
            match self.blocks[n].terminator {
                Terminator::End => EscStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\n END{}",
                    n.index(),
                    self.blocks[n].statements.len(),
                    if dead { "\nDEAD" } else { "" }
                ))),
                Terminator::Merge(_) | Terminator::Goto(_) => LabelStr(Cow::Owned(format!(
                    "BB_{}: {} Statements{}",
                    n.index(),
                    self.blocks[n].statements.len(),
                    if dead { "\nDEAD" } else { "" }
                ))),
                Terminator::Split {
                    condition, merge, ..
                } => LabelStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\nSplit at {:?}\nMerge at BB_{}{}",
                    n.index(),
                    self.blocks[n].statements.len(),
                    condition,
                    merge.index(),
                    if dead { "\nDEAD" } else { "" }
                ))),
            }
        }
        fn edge_label(
            &'a self,
            &(start, dst): &(BasicBlockId<'tag>, BasicBlockId<'tag>),
        ) -> LabelText<'a> {
            match self.blocks[start].terminator {
                Terminator::Merge(_) => LabelStr(Cow::Borrowed("MERGE")),
                Terminator::Goto(_) => LabelStr(Cow::Borrowed("GOTO")),
                Terminator::End => LabelStr(Cow::Borrowed("ILLEGAL")),
                Terminator::Split {
                    condition,
                    true_block,
                    false_block,
                    merge,
                } => {
                    let true_or_false = if true_block == dst {
                        "TRUE"
                    } else if false_block == dst {
                        "FALSE"
                    } else {
                        "ILLEGAL"
                    };
                    LabelStr(Cow::Borrowed(true_or_false))
                }
            }
        }
    }

    impl<'a, 'tag, 'mir> dot::GraphWalk<'a> for ControlFlowGraph<'tag, 'mir> {
        type Node = BasicBlockId<'tag>;
        type Edge = (BasicBlockId<'tag>, BasicBlockId<'tag>);

        fn nodes(&'a self) -> Nodes<'a, Self::Node> {
            Cow::Owned(self.blocks.full_range().collect())
        }

        fn edges(&'a self) -> Edges<'a, Self::Edge> {
            let mut edges = Vec::new();
            for block in self.blocks.full_range() {
                match self.blocks[block].terminator {
                    Terminator::Merge(dst) | Terminator::Goto(dst) => edges.push((block, dst)),
                    Terminator::Split {
                        condition,
                        true_block,
                        false_block,
                        merge,
                    } => {
                        edges.push((block, false_block));
                        edges.push((block, true_block));
                    }
                    Terminator::End => (),
                }
            }
            Cow::Owned(edges)
        }

        fn source(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.0
        }

        fn target(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.1
        }
    }

    struct DominatorTreeControlFlowRender<'lt, 'tag, 'mir> {
        cfg: &'lt ControlFlowGraph<'tag, 'mir>,
    }
    impl<'a, 'tag, 'mir> dot::Labeller<'a> for DominatorTreeControlFlowRender<'a, 'tag, 'mir> {
        type Node = BasicBlockId<'tag>;
        type Edge = (BasicBlockId<'tag>, BasicBlockId<'tag>);

        fn graph_id(&'a self) -> Id<'a> {
            dot::Id::new("ControlFlowGraph").unwrap()
        }

        fn node_id(&'a self, n: &Self::Node) -> Id<'a> {
            dot::Id::new(format!("BB_{}", n.index())).unwrap()
        }

        fn node_label(&'a self, &n: &Self::Node) -> LabelText<'a> {
            match self.cfg.blocks[n].terminator {
                Terminator::End => EscStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\n END",
                    n.index(),
                    self.cfg.blocks[n].statements.len()
                ))),
                Terminator::Merge(nxt) | Terminator::Goto(nxt) => LabelStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\n GOTO {}",
                    n.index(),
                    self.cfg.blocks[n].statements.len(),
                    nxt.index()
                ))),
                Terminator::Split { condition, .. } => LabelStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\n Split {:?}",
                    n.index(),
                    self.cfg.blocks[n].statements.len(),
                    condition,
                ))),
            }
        }

        fn edge_label(
            &'a self,
            &(start, dst): &(BasicBlockId<'tag>, BasicBlockId<'tag>),
        ) -> LabelText<'a> {
            match self.cfg.blocks[start].terminator {
                Terminator::Merge(_) | Terminator::Goto(_) => LabelStr(Cow::Borrowed("ILLEGAL")),
                Terminator::End => LabelStr(Cow::Borrowed("ILLEGAL")),
                Terminator::Split {
                    condition,
                    true_block,
                    false_block,
                    merge,
                } => {
                    let true_or_false = if true_block == dst {
                        "TRUE"
                    } else if false_block == dst {
                        "FALSE"
                    } else if merge == dst {
                        "MERGE"
                    } else {
                        "ILLEGAL"
                    };
                    LabelStr(Cow::Borrowed(true_or_false))
                }
            }
        }
    }

    impl<'a, 'tag, 'mir> dot::GraphWalk<'a> for DominatorTreeControlFlowRender<'a, 'tag, 'mir> {
        type Node = BasicBlockId<'tag>;
        type Edge = (BasicBlockId<'tag>, BasicBlockId<'tag>);

        fn nodes(&'a self) -> Nodes<'a, Self::Node> {
            Cow::Owned(self.cfg.blocks.full_range().collect())
        }

        fn edges(&'a self) -> Edges<'a, Self::Edge> {
            let mut edges = Vec::new();
            for block in self.cfg.blocks.full_range() {
                match self.cfg.blocks[block].terminator {
                    Terminator::Merge(dst) | Terminator::Goto(dst) => (),
                    Terminator::Split {
                        condition,
                        true_block,
                        false_block,
                        merge,
                    } => {
                        edges.push((block, false_block));
                        edges.push((block, true_block));
                        edges.push((block, merge));
                    }
                    Terminator::End => (),
                }
            }
            Cow::Owned(edges)
        }

        fn source(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.0
        }

        fn target(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.1
        }
    }

    impl<'a, 'tag, 'mir> dot::Labeller<'a> for SimplifiedControlFlowGraph<'tag, 'mir> {
        type Node = BasicBlockId<'tag>;
        type Edge = (BasicBlockId<'tag>, BasicBlockId<'tag>);

        fn graph_id(&'a self) -> Id<'a> {
            dot::Id::new("ControlFlowGraph").unwrap()
        }

        fn node_id(&'a self, n: &Self::Node) -> Id<'a> {
            dot::Id::new(format!("BB_{}", n.index())).unwrap()
        }

        fn node_label(&'a self, &n: &Self::Node) -> LabelText<'a> {
            match self.blocks[n].terminator {
                Terminator::End => EscStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\n END",
                    n.index(),
                    self.blocks[n].statements.len()
                ))),
                Terminator::Merge(_) | Terminator::Goto(_) => LabelStr(Cow::Owned(format!(
                    "BB_{}: {} Statements",
                    n.index(),
                    self.blocks[n].statements.len()
                ))),
                Terminator::Split {
                    condition, merge, ..
                } => LabelStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\nSplit at {:?}\nMerge at BB_{}",
                    n.index(),
                    self.blocks[n].statements.len(),
                    condition,
                    merge.index()
                ))),
            }
        }
        fn edge_label(
            &'a self,
            &(start, dst): &(BasicBlockId<'tag>, BasicBlockId<'tag>),
        ) -> LabelText<'a> {
            match self.blocks[start].terminator {
                Terminator::Merge(_) | Terminator::Goto(_) => LabelStr(Cow::Borrowed("GOTO")),
                Terminator::End => LabelStr(Cow::Borrowed("ILLEGAL")),
                Terminator::Split {
                    condition,
                    true_block,
                    false_block,
                    merge,
                } => {
                    let true_or_false = if true_block == dst {
                        "TRUE"
                    } else if false_block == dst {
                        "FALSE"
                    } else {
                        "ILLEGAL"
                    };
                    LabelStr(Cow::Borrowed(true_or_false))
                }
            }
        }
    }

    impl<'a, 'tag, 'mir> dot::GraphWalk<'a> for SimplifiedControlFlowGraph<'tag, 'mir> {
        type Node = BasicBlockId<'tag>;
        type Edge = (BasicBlockId<'tag>, BasicBlockId<'tag>);

        fn nodes(&'a self) -> Nodes<'a, Self::Node> {
            Cow::Owned(self.blocks.full_range().collect())
        }

        fn edges(&'a self) -> Edges<'a, Self::Edge> {
            let mut edges = Vec::new();
            for block in self.blocks.full_range() {
                match self.blocks[block].terminator {
                    Terminator::Merge(dst) | Terminator::Goto(dst) => edges.push((block, dst)),
                    Terminator::Split {
                        condition,
                        true_block,
                        false_block,
                        merge,
                    } => {
                        edges.push((block, false_block));
                        edges.push((block, true_block));
                    }
                    Terminator::End => (),
                }
            }
            Cow::Owned(edges)
        }

        fn source(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.0
        }

        fn target(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.1
        }
    }
}
