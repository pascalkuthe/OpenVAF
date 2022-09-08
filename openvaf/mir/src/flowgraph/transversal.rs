use crate::Block;
use bitset::BitSet;

use crate::flowgraph::Successors;
use crate::ControlFlowGraph;

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
pub struct Postorder<'a> {
    cfg: &'a ControlFlowGraph,
    visited: BitSet<Block>,
    visit_stack: Vec<(Block, Successors)>,
}

impl<'a> Postorder<'a> {
    pub fn new(cfg: &'a ControlFlowGraph, root: Block) -> Postorder<'a> {
        let mut po =
            Postorder { cfg, visited: BitSet::new_empty(cfg.data.len()), visit_stack: Vec::new() };

        po.visited.insert(root);
        po.visit_stack.push((root, cfg.successors(root)));
        po.traverse_successor(cfg);

        po
    }

    fn traverse_successor(&mut self, cfg: &ControlFlowGraph) {
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
        //     [(A, [B])]
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
        // two iterations yield `C` and finally `A` for a final traversal of [E, D, B, A]
        while let Some(bb) = self.visit_stack.last_mut().and_then(|(_, iter)| iter.pop()) {
            if self.visited.insert(bb) {
                self.visit_stack.push((bb, cfg.successors(bb)));
            }
        }
    }
}

impl<'lt> Iterator for Postorder<'lt> {
    type Item = Block;

    fn next(&mut self) -> Option<Block> {
        let next = self.visit_stack.pop().map(|(bb, _)| bb);
        if next.is_some() {
            self.traverse_successor(self.cfg);
        }

        next
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // All the blocks, minus the number of blocks we've visited.
        let upper = self.cfg.data.len() - self.visited.count();

        let lower = self.visit_stack.len();

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
    blocks: Vec<Block>,
    idx: usize,
}

impl ReversePostorder {
    pub fn new(cfg: &ControlFlowGraph, root: Block) -> Self {
        let blocks: Vec<_> = Postorder::new(cfg, root).collect();

        let len = blocks.len();

        ReversePostorder { blocks, idx: len }
    }

    pub fn reset(&mut self) {
        self.idx = self.blocks.len();
    }
}

impl Iterator for ReversePostorder {
    type Item = Block;

    fn next(&mut self) -> Option<Block> {
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
