//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::cfg::predecessors::PredecessorCache;
use crate::cfg::statement_owner::StatementOwnerCache;
use crate::cfg::transversal::ReversePostorderIterMut;
use crate::cfg::transversal::{
    Postorder, PostorderIter, PostorderIterMut, ReversePostorder, ReversePostorderIter,
};
use crate::Mir;
use core::mem::swap;
use core::mem::MaybeUninit;
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_data_structures::BitSet;
use openvaf_ir::ids::{IntegerExpressionId, StatementId};
use openvaf_ir::{id_type, impl_id_type};

use crate::HashMap;

pub mod predecessors;
#[cfg(feature = "graph_debug")]
mod print;
pub mod statement_owner;
pub mod transversal;

// Macros that call functions that produce a cfg with more or less blocks (and therefore a new tag)

id_type!(BasicBlockId(u16));

impl_id_type!(BasicBlockId in ControlFlowGraph::blocks -> BasicBlock);

pub struct Successors {
    data: [MaybeUninit<BasicBlockId>; 2],
    len: u8,
}

impl Successors {
    #[inline]
    #[must_use]
    pub const fn new_empty() -> Self {
        Self {
            data: [MaybeUninit::uninit(), MaybeUninit::uninit()],
            len: 0,
        }
    }

    #[inline]
    #[must_use]
    pub const fn new_single(block: BasicBlockId) -> Self {
        Self {
            data: [MaybeUninit::new(block), MaybeUninit::uninit()],
            len: 1,
        }
    }

    #[inline]
    #[must_use]
    pub const fn new_double(block1: BasicBlockId, block2: BasicBlockId) -> Self {
        Self {
            data: [MaybeUninit::new(block1), MaybeUninit::new(block2)],
            len: 2,
        }
    }
}

impl Iterator for Successors {
    type Item = BasicBlockId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.len > 0 {
            self.len -= 1;
            Some(unsafe { self.data[self.len as usize].assume_init() })
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len as usize, Some(self.len as usize))
    }
}

impl ExactSizeIterator for Successors {
    fn len(&self) -> usize {
        self.len as usize
    }
}

#[derive(Clone, Debug, Default)]
pub struct ControlFlowGraph {
    pub blocks: IndexVec<BasicBlockId, BasicBlock>,
    pub predecessor_cache: PredecessorCache,
    pub statement_owner_cache: StatementOwnerCache,
}

impl ControlFlowGraph {
    #[inline]
    #[must_use]
    pub fn new(blocks: IndexVec<BasicBlockId, BasicBlock>, mir: &Mir) -> Self {
        Self {
            blocks,
            predecessor_cache: PredecessorCache::new(),
            statement_owner_cache: StatementOwnerCache::new(mir.statements().len()),
        }
    }

    //inline always because this is so simple it literally takes less instructions to execute this than
    #[inline]
    pub fn start(&self) -> BasicBlockId {
        // The last element
        self.blocks.len_idx() - 1
    }

    //This could be a constant
    //but I dont want to expose that implimentation detail in a public API
    #[allow(clippy::inline_always)]
    #[allow(clippy::unused_self)]
    #[inline(always)]
    pub const fn end(&self) -> BasicBlockId {
        BasicBlockId::from_raw_unchecked(0)
    }

    // transversals

    pub fn postorder(&self) -> Postorder {
        Postorder::new(self, self.start())
    }

    pub fn postorder_iter(&self) -> PostorderIter {
        PostorderIter::new(self, self.start())
    }

    pub fn postorder_itermut(&mut self) -> PostorderIterMut {
        PostorderIterMut::new(self, self.start())
    }

    pub fn reverse_postorder(&self) -> ReversePostorder {
        ReversePostorder::new(self, self.start())
    }

    pub fn reverse_postorder_iter(&self) -> ReversePostorderIter<'_> {
        ReversePostorderIter::new(self, self.start())
    }

    pub fn reverse_postorder_itermut(&mut self) -> ReversePostorderIterMut {
        ReversePostorderIterMut::new(self, self.start())
    }

    // Relations

    pub fn predecessors(&self, bb: BasicBlockId) -> &[BasicBlockId] {
        &self.predecessor_cache.compute(&self.blocks)[bb]
    }

    pub fn containing_block(&self, stmt: StatementId) -> Option<BasicBlockId> {
        self.statement_owner_cache
            .compute(self)
            .get(stmt)
            .copied()
            .flatten()
    }

    pub fn successors(&self, bb: BasicBlockId) -> Successors {
        self[bb].successors()
    }

    // Transformations

    pub fn simplify(&mut self) {
        let mut new_replacements: HashMap<BasicBlockId, BasicBlockId> = HashMap::default();
        let mut replacements = new_replacements.clone();
        let mut dead_blocks = BitSet::new_empty(self.blocks.len_idx());
        dead_blocks.extend(self.postorder_iter().map(|(id, _)| id));
        dead_blocks.toggle_range(..);
        loop {
            let mut replacement_targets = BitSet::new_empty(self.blocks.len_idx());
            let mut changed = false;

            for (id, block) in self.blocks.iter_mut_enumerated() {
                if dead_blocks.contains(id) {
                    continue;
                }

                match block.terminator {
                    Terminator::End => (),
                    Terminator::Goto(ref mut next) => {
                        if let Some(&new_next) = replacements.get(next) {
                            *next = new_next
                        }

                        let next = *next;
                        if block.statements.is_empty()
                            && !replacement_targets.contains(id)
                            && !new_replacements.contains_key(&next)
                        {
                            new_replacements.insert(id, next);
                            dead_blocks.insert(id);
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

                        block.terminator = if true_block == false_block {
                            //empty condition
                            changed = true;
                            Terminator::Goto(merge)
                        } else if true_block == id {
                            //empty loop
                            changed = true;
                            Terminator::Goto(false_block)
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

            /*trace!(
                "running because changed: {} or replacements: {:#?}",
                changed,
                new_replacements
            );*/

            swap(&mut new_replacements, &mut replacements);
            new_replacements.clear();
        }
        self.remove_dead_code(&dead_blocks);
    }

    pub fn remove_dead_code(&mut self, dead_blocks: &BitSet<BasicBlockId>) {
        let mut replacements = HashMap::new();

        // adopted from std::vec::Vec::retain to fit the needs here
        let len = self.blocks.len();
        let mut del = 0;

        for id in self.blocks.indices() {
            if dead_blocks.contains(id) {
                del += 1;
            } else if del > 0 {
                self.blocks.swap(id - del, id);
                replacements.insert(id, id - del);
            }
        }

        if del > 0 {
            self.blocks.truncate(len - del);
        }

        for block in self.blocks.indices() {
            match self.blocks[block].terminator {
                Terminator::End => (),

                Terminator::Goto(ref mut next) => {
                    if let Some(&new_next) = replacements.get(next) {
                        *next = new_next
                    }
                }
                Terminator::Split {
                    ref mut true_block,
                    ref mut false_block,
                    ref mut merge,
                    ..
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
        self.statement_owner_cache.invalidate_internal();
        self.predecessor_cache.invalidate();
    }
}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub statements: Vec<StatementId>,
    pub terminator: Terminator,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Terminator {
    Goto(BasicBlockId),
    Split {
        condition: IntegerExpressionId,
        true_block: BasicBlockId,
        false_block: BasicBlockId,
        merge: BasicBlockId,
    },
    End,
}

impl Terminator {
    #[must_use]
    pub fn successors(&self) -> Successors {
        match self {
            Self::End => Successors::new_empty(),
            Self::Goto(dst) => Successors::new_single(*dst),
            Self::Split {
                true_block,
                false_block,
                ..
            } => Successors::new_double(*true_block, *false_block),
        }
    }
}

impl BasicBlock {
    #[must_use]
    pub fn successors(&self) -> Successors {
        self.terminator.successors()
    }
}
