/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::cfg::{BasicBlock, BasicBlockData, ControlFlowGraph, Successors, START_BLOCK};
use crate::CallType;
use openvaf_data_structures::BitSet;
use std::mem::transmute;

/// Postorder traversal of a graph.
///
/// Postorder traversal is when each node is visited after all of its
/// successors, except when the successor is only reachable by a back-edge
///
///
/// ```text
///
///         A
///        / \
///       /   \
///      B     C
///       \   /
///        \ /
///         D
/// ```
///
/// A Postorder traversal of this graph is `D B C A` or `D C B A`
///
pub struct Postorder {
    visited: BitSet<BasicBlock>,
    visit_stack: Vec<(BasicBlock, Successors)>,
    root_is_start_block: bool,
}

impl<'lt> Postorder {
    pub fn new<C: CallType>(cfg: &ControlFlowGraph<C>, root: BasicBlock) -> Postorder {
        let mut po = Postorder {
            visited: BitSet::new_empty(cfg.blocks.len_idx()),
            visit_stack: Vec::new(),
            root_is_start_block: root == START_BLOCK,
        };

        po.visited.insert(root);
        po.visit_stack.push((root, cfg.successors(root)));
        po.traverse_successor(cfg);

        po
    }

    fn traverse_successor<C: CallType>(&mut self, cfg: &ControlFlowGraph<C>) {
        // This is quite a complex loop due to 1. the borrow checker not liking it much
        // and 2. what exactly is going on is not clear
        //
        // It does the actual traversal of the graph, while the `next` method on the iterator
        // just pops off of the stack. `visit_stack` is a stack containing pairs of nodes and
        // iterators over the successors of those nodes. Each iteration attempts to get the next
        // node from the top of the stack, then pushes that node and an iterator over the
        // successors to the top of the stack. This loop only grows `visit_stack`, stopping when
        // we reach a child that has no children that we haven't already visited.
        //
        // For a graph that looks like this:
        //
        //         A
        //        / \
        //       /   \
        //      B     C
        //      |     |
        //      |     |
        //      D     |
        //       \   /
        //        \ /
        //         E
        //
        // The state of the stack starts out with just the root node (`A` in this case);
        //     [(A, [B, C])]
        //
        // When the first call to `traverse_successor` happens, the following happens:
        //
        //     [(B, [D]),  // `B` taken from the successors of `A`, pushed to the
        //                 // top of the stack along with the successors of `B`
        //      (A, [C])]
        //
        //     [(D, [E]),  // `D` taken from successors of `B`, pushed to stack
        //      (B, []),
        //      (A, [C])]
        //
        //     [(E, []),   // `E` taken from successors of `D`, pushed to stack
        //      (D, []),
        //      (B, []),
        //      (A, [C])]
        //
        // Now that the top of the stack has no successors we can traverse, each item will
        // be popped off during iteration until we get back to `A`. This yields [E, D, B].
        //
        // When we yield `B` and call `traverse_successor`, we push `C` to the stack, but
        // since we've already visited `E`, that child isn't added to the stack. The last
        // two iterations yield `C` and finally `A` for a final traversal of [E, D, B, C, A]
        while let Some(bb) = self
            .visit_stack
            .last_mut()
            .and_then(|(_, iter)| iter.next())
        {
            if !self.visited.put(bb) {
                self.visit_stack.push((bb, cfg.successors(bb)));
            }
        }
    }
}

pub struct PostorderIter<'lt, C: CallType> {
    pub base: Postorder,
    pub cfg: &'lt ControlFlowGraph<C>,
}

impl<'lt, C: CallType> PostorderIter<'lt, C> {
    pub fn new(cfg: &'lt ControlFlowGraph<C>, root: BasicBlock) -> Self {
        Self {
            base: Postorder::new(cfg, root),
            cfg,
        }
    }
}

impl<'lt, C: CallType> Iterator for PostorderIter<'lt, C> {
    type Item = (BasicBlock, &'lt BasicBlockData<C>);

    fn next(&mut self) -> Option<(BasicBlock, &'lt BasicBlockData<C>)> {
        let next = self.base.visit_stack.pop();
        if next.is_some() {
            self.base.traverse_successor(self.cfg);
        }

        next.map(|(bb, _)| (bb, &self.cfg[bb]))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // All the blocks, minus the number of blocks we've visited.
        let upper = self.cfg.blocks.len() - self.base.visited.ones().count();

        let lower = if self.base.root_is_start_block {
            // We will visit all remaining blocks exactly once.
            upper
        } else {
            self.base.visit_stack.len()
        };

        (lower, Some(upper))
    }
}

pub struct PostorderIterMut<'lt, C: CallType> {
    base: Postorder,
    cfg: &'lt mut ControlFlowGraph<C>,
}

impl<'lt, C: CallType> PostorderIterMut<'lt, C> {
    pub fn new(cfg: &'lt mut ControlFlowGraph<C>, root: BasicBlock) -> Self {
        Self {
            base: Postorder::new(cfg, root),
            cfg,
        }
    }
}

impl<'lt, C: CallType> Iterator for PostorderIterMut<'lt, C> {
    type Item = (BasicBlock, &'lt mut BasicBlockData<C>);

    fn next(&mut self) -> Option<(BasicBlock, &'lt mut BasicBlockData<C>)> {
        let next = self.base.visit_stack.pop();
        if next.is_some() {
            self.base.traverse_successor(self.cfg);
        }

        /*
        This transmute  is here to transmut `&'_ mut BasicBlock` to `&'lt mut BasicBlock`
        This is save since the iterator already has an exclusive borrow of the ControlFlow Graph/Basic Blocks for 'lt
        and only yields elements exactly once so not two mutable refences to the same block can be handed out.
        Furthermore the mutable cfg reference is private so it can not be used from elsewhere to access the cfg.
        Internally this iterator does read from the cfg. However it only ever reads from blocks that have not been visited yet.
        As such this is also save since after hadning out a &'lt mut to a block it is never read internally either
        */

        next.map(|(bb, _)| (bb, unsafe { transmute(&mut self.cfg[bb]) }))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // All the blocks, minus the number of blocks we've visited.
        let upper = self.cfg.blocks.len() - self.base.visited.ones().count();

        let lower = if self.base.root_is_start_block {
            // We will visit all remaining blocks exactly once.
            upper
        } else {
            self.base.visit_stack.len()
        };

        (lower, Some(upper))
    }
}

/// Reverse postorder traversal of a graph
///
/// Reverse postorder is the reverse order of a postorder traversal.
/// This is different to a preorder traversal and represents a natural
/// linearization of control-flow.
///
/// ```text
///
///         A
///        / \
///       /   \
///      B     C
///       \   /
///        \ /
///         D
/// ```
///
/// A reverse postorder traversal of this graph is either `A B C D` or `A C B D`
/// Note that for a graph containing no loops (i.e., A DAG), this is equivalent to
/// a topological sort.
///
/// Construction of a `ReversePostorder` traversal requires doing a full
/// postorder traversal of the graph, therefore this traversal should be
/// constructed as few times as possible. Use the `reset` method to be able
/// to re-use the traversal
#[derive(Clone, Debug)]
pub struct ReversePostorder {
    blocks: Vec<BasicBlock>,
    idx: usize,
}

impl ReversePostorder {
    pub fn new<C: CallType>(cfg: &ControlFlowGraph<C>, root: BasicBlock) -> Self {
        let blocks: Vec<_> = PostorderIter::new(cfg, root).map(|(bb, _)| bb).collect();

        let len = blocks.len();

        ReversePostorder { blocks, idx: len }
    }

    pub fn reset(&mut self) {
        self.idx = self.blocks.len();
    }
}

impl Iterator for ReversePostorder {
    type Item = BasicBlock;

    fn next(&mut self) -> Option<BasicBlock> {
        if self.idx == 0 {
            return None;
        }
        self.idx -= 1;

        self.blocks.get(self.idx).copied()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.idx, Some(self.idx))
    }
}

impl ExactSizeIterator for ReversePostorder {
    fn len(&self) -> usize {
        self.idx
    }
}

pub struct ReversePostorderIter<'lt, C: CallType> {
    cfg: &'lt ControlFlowGraph<C>,
    base: ReversePostorder,
}

impl<'lt, C: CallType> ReversePostorderIter<'lt, C> {
    pub fn new(cfg: &'lt ControlFlowGraph<C>, root: BasicBlock) -> Self {
        Self {
            base: ReversePostorder::new(cfg, root),
            cfg,
        }
    }
}

impl<'lt, C: CallType> ExactSizeIterator for ReversePostorderIter<'lt, C> {
    fn len(&self) -> usize {
        self.base.len()
    }
}

impl<'lt, C: CallType> Iterator for ReversePostorderIter<'lt, C> {
    type Item = (BasicBlock, &'lt BasicBlockData<C>);

    fn next(&mut self) -> Option<(BasicBlock, &'lt BasicBlockData<C>)> {
        self.base.next().map(|bb| (bb, &self.cfg[bb]))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.base.size_hint()
    }
}

pub struct ReversePostorderIterMut<'lt, C: CallType> {
    cfg: &'lt mut ControlFlowGraph<C>,
    base: ReversePostorder,
}

impl<'lt, C: CallType> ReversePostorderIterMut<'lt, C> {
    pub fn new(cfg: &'lt mut ControlFlowGraph<C>, root: BasicBlock) -> Self {
        Self {
            base: ReversePostorder::new(cfg, root),
            cfg,
        }
    }
}

impl<'lt, C: CallType> Iterator for ReversePostorderIterMut<'lt, C> {
    type Item = (BasicBlock, &'lt mut BasicBlockData<C>);

    fn next(&mut self) -> Option<(BasicBlock, &'lt mut BasicBlockData<C>)> {
        /*
        This transmute  is here to transmut &'_ mut BasicBlock to &'lt mut BasicBlock
        This is save since the iterator already has an exclusive borrow of the ControlFlow Graph/Basic Blocks for 'lt
        and only yields elements exactly once so not two mutable refences to the same block can be handed out.
        Furthermore the mutable cfg reference is private so it can not be used from elsewhere to access the cfg.
        Internally this iterator never accesses the cfg (this is just a conviniance struct) so this is fine here
        */
        self.base
            .next()
            .map(|bb| (bb, unsafe { transmute(&mut self.cfg[bb]) }))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.base.size_hint()
    }
}

impl<'lt, C: CallType> ExactSizeIterator for ReversePostorderIterMut<'lt, C> {
    fn len(&self) -> usize {
        self.base.len()
    }
}
