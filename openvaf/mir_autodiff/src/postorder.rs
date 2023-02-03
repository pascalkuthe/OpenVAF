use bitset::BitSet;
use mir::{DataFlowGraph, Inst, InstUseIter, Use, Value};

use crate::intern::DerivativeIntern;
use crate::{is_zero_call, zero_derivative};

pub type PostorderParts<'a> = (BitSet<Inst>, Vec<(Inst, InstUseIter<'a>)>);

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
pub struct Postorder<'a, 'b, 'c> {
    dfg: &'a DataFlowGraph,
    pub visited: BitSet<Inst>,
    pub visit_stack: Vec<(Inst, InstUseIter<'a>)>,
    intern: &'b DerivativeIntern<'c>,
}

impl<'a, 'b, 'c> Postorder<'a, 'b, 'c> {
    // pub fn new(dfg: &'a DataFlowGraph, intern: &'b DerivativeIntern<'c>) -> Postorder<'a, 'b, 'c> {
    //     Postorder {
    //         dfg,
    //         visited: BitSet::new_empty(dfg.num_insts()),
    //         visit_stack: Vec::new(),
    //         intern,
    //     }
    // }

    pub fn into_parts(self) -> PostorderParts<'a> {
        (self.visited, self.visit_stack)
    }

    pub fn from_parts(
        dfg: &'a DataFlowGraph,
        parts: PostorderParts<'a>,
        intern: &'b DerivativeIntern<'c>,
    ) -> Self {
        Postorder { dfg, visited: parts.0, visit_stack: parts.1, intern }
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
        self.transverse_inst(inst);
    }

    pub fn transverse_inst(&mut self, inst: Inst) {
        if !zero_derivative(self.dfg, inst)
            && !is_zero_call(self.dfg, inst, self.intern)
            && self.visited.insert(inst)
        {
            self.visit_stack.push((inst, self.dfg.inst_uses(inst)));
        }
    }
}

impl Iterator for Postorder<'_, '_, '_> {
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
