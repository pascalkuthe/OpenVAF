//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::compact_arena::{Idx16, InvariantLifetime, Step, TinyHeapArena};
use crate::ir::{IntegerExpressionId, StatementId};
use log::trace;
use rustc_hash::{FxHashMap, FxHashSet};

pub type BasicBlockId<'tag> = Idx16<'tag>;

#[derive(Debug)]
pub struct ControlFlowGraph<'tag, 'mir> {
    pub blocks: TinyHeapArena<'tag, BasicBlock<'tag, 'mir>>,
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

    // TODO: Use only a single allocator and vector truncation
    pub fn simplify<'newtag>(
        mut self,
        new_allocator: TinyHeapArena<'newtag, BasicBlock<'newtag, 'mir>>,
    ) -> ControlFlowGraph<'newtag, 'mir> {
        let mut new_replacements: FxHashMap<BasicBlockId, BasicBlockId> = FxHashMap::default();
        let mut removals: FxHashSet<BasicBlockId> = FxHashSet::default();
        let mut res = ControlFlowGraph {
            blocks: new_allocator,
        };
        // Transmute so we can switch buffers
        let mut old: ControlFlowGraph<'newtag, 'mir> = unsafe { std::mem::transmute(self) };

        loop {
            //TODO more efficient algorithm?
            let replacements = new_replacements;
            new_replacements = FxHashMap::default();
            let mut replacement_targets = FxHashSet::default();
            let mut changed = false;
            for block in old.blocks.full_range() {
                if removals.contains(&block) {
                    continue;
                }
                match old.blocks[block].terminator {
                    Terminator::End => (),
                    Terminator::Merge(ref mut next) | Terminator::Goto(ref mut next) => {
                        if let Some(&new_next) = replacements.get(&next) {
                            *next = new_next
                        }

                        let next = *next;

                        if old.blocks[block].statements.is_empty()
                            && !replacement_targets.contains(&block)
                            && !new_replacements.contains_key(&next)
                        {
                            new_replacements.insert(block, next);
                            removals.insert(block);
                            replacement_targets.insert(next);
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

                        old.blocks[block].terminator = if true_block == false_block {
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
            trace!(
                "running because changed: {} or replacements: {:#?}",
                changed,
                new_replacements
            );
            if new_replacements.is_empty() && !changed {
                break;
            }
        }

        let mut offset = 0u16;
        for block in old.blocks.full_range() {
            if removals.contains(&block) {
                offset += 1;
                continue;
            }
            let mut new_block = block;
            unsafe { new_block.sub(offset) };
            new_replacements.insert(block, new_block);
            res.blocks.add(BasicBlock {
                statements: std::mem::take(&mut old.blocks[block].statements),
                terminator: old.blocks[block].terminator,
            });
        }
        unsafe {
            old.blocks.clear();
        }

        for block in res.blocks.full_range() {
            match res.blocks[block].terminator {
                Terminator::End => (),

                Terminator::Merge(ref mut next) | Terminator::Goto(ref mut next) => {
                    //split into two loops
                    if let Some(&new_next) = new_replacements.get(&next) {
                        *next = new_next
                    }
                }
                Terminator::Split {
                    condition,
                    ref mut true_block,
                    ref mut false_block,
                    ref mut merge,
                } => {
                    if let Some(&new_true_block) = new_replacements.get(true_block) {
                        *true_block = new_true_block
                    }

                    if let Some(&new_false_block) = new_replacements.get(false_block) {
                        *false_block = new_false_block
                    }

                    if let Some(&new_merge) = new_replacements.get(merge) {
                        *merge = new_merge
                    }
                }
            }
        }
        res
    }

    pub unsafe fn retain<'newtag>(
        mut self,
        tag: InvariantLifetime<'newtag>,
        mut predicate: impl FnMut(BasicBlockId<'tag>) -> bool,
    ) -> ControlFlowGraph<'newtag, 'mir> {
        let mut replacements = FxHashMap::default();
        self.blocks.retain(
            |block| predicate(block),
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
        }
    }

    pub fn for_all_blocks(&self, mut f: impl FnMut(BasicBlockId<'tag>)) {
        self.blocks.full_range().for_each(|block| f(block));
    }

    pub fn visit_mut_in_execution_order(
        &mut self,
        mut f: impl FnMut(&mut Self, BasicBlockId<'tag>),
    ) {
        self.partial_visit_mut_in_execution_order(self.start(), None, &mut f)
    }
    pub fn partial_visit_mut_in_execution_order(
        &mut self,
        start: BasicBlockId<'tag>,
        end: Option<BasicBlockId<'tag>>,
        f: &mut impl FnMut(&mut Self, BasicBlockId<'tag>),
    ) {
        let mut current = start;
        loop {
            f(self, current);
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
}
