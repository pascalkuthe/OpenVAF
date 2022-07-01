use bitset::BitSet;
use mir::{DataFlowGraph, Inst, InstUseIter, Use, Value};

use crate::zero_derivative;

/// Postorder traversal of a data flow graph
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
    dfg: &'a DataFlowGraph,
    pub visited: BitSet<Inst>,
    visit_stack: Vec<(Inst, InstUseIter<'a>)>,
}

impl<'a> Postorder<'a> {
    pub fn new(dfg: &'a DataFlowGraph) -> Postorder<'a> {
        Postorder { dfg, visited: BitSet::new_empty(dfg.num_insts()), visit_stack: Vec::new() }
    }

    pub fn populate(&mut self, val: Value) {
        for use_ in self.dfg.uses(val) {
            self.transverse_use(use_)
        }
    }

    pub fn clear(&mut self) {
        self.visited.clear();
    }

    pub fn traverse_successor(&mut self) {
        while let Some(use_) = self.visit_stack.last_mut().and_then(|(_, iter)| iter.next()) {
            self.transverse_use(use_);
        }
    }

    fn transverse_use(&mut self, use_: Use) {
        let inst = self.dfg.use_to_operand(use_).0;
        if !zero_derivative(self.dfg, inst) && self.visited.insert(inst) {
            self.visit_stack.push((inst, self.dfg.inst_uses(inst)));
        }
    }
}

impl<'lt> Iterator for Postorder<'lt> {
    type Item = Inst;

    fn next(&mut self) -> Option<Inst> {
        let next = self.visit_stack.pop().map(|(inst, _)| inst);
        if next.is_some() {
            self.traverse_successor();
        }

        next
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // All the blocks, minus the number of blocks we've visited.
        let upper = self.dfg.num_insts() - self.visited.count();

        let lower = self.visit_stack.len();

        (lower, Some(upper))
    }
}
