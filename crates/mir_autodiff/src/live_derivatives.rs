use bitset::{BitSet, HybridBitSet, SparseBitMatrix};
use mir::{
    DataFlowGraph, FuncRef, Function, Inst, InstUseIter, InstructionData, Use, Value, ValueDef,
};

use crate::unkowns::{Unkown, Unkowns};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct LiveDerivatives {
    pub derivatives: SparseBitMatrix<Inst, Unkown>,
    pub post_order: Vec<Inst>,
}

impl LiveDerivatives {
    pub fn build(
        func: &Function,
        unkowns: &mut Unkowns,
        extra_derivatives: impl IntoIterator<Item = (Value, FuncRef)>,
    ) -> LiveDerivatives {
        let mut derivative_params = BitSet::new_empty(func.dfg.num_values());

        for (_, params) in unkowns.first_order_unkowns.raw.iter() {
            for (param, _) in params.iter() {
                derivative_params.insert(*param);
            }
        }

        let mut derivatives = SparseBitMatrix::new(func.dfg.num_values(), unkowns.len());

        for (val, func_ref) in extra_derivatives {
            if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
                let unkown = unkowns.first_order_unkowns.index(&func_ref).unwrap();
                derivatives.insert(inst, unkown.into());
            }
        }

        // finding the live derivatives is a simple post order visit
        // the union of the derivatives of the uses then form the live derivatives
        // However for cycels this doesn't work because not all uses can be visited before
        //
        // for now simply running a fixpoint iteration is the only valid solution here
        // Even for complex models we at most end up with 5 iterations which is fine (this loop
        // isn't very performance intensive)
        //
        // If it becomes a problem we might look into only reprocessing users of cycels

        let mut post_order: Vec<_> = Postorder::new(&func.dfg, &derivative_params).collect();
        loop {
            let mut changed = false;

            for inst in post_order.iter().copied() {
                let mut dst = HybridBitSet::new_empty();
                for val in func.dfg.inst_results(inst) {
                    for use_ in func.dfg.uses(*val) {
                        let user = func.dfg.use_to_operand(use_).0;
                        if let Some(row) = derivatives.row(user) {
                            dst.union(row, unkowns.len());
                        }
                    }
                }

                if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                    if let Some(ddx_unkown) = unkowns.callback_unkown(func_ref) {
                        let old = dst.clone();
                        for unkown in old.iter() {
                            let higher_order = unkowns.raise_order(unkown, ddx_unkown);
                            dst.insert_growable(higher_order, unkowns.len());
                        }

                        dst.insert(ddx_unkown.into(), unkowns.len());
                    }
                }

                if !dst.is_empty() {
                    let old = derivatives.ensure_row(inst);
                    if old != &dst {
                        changed = true;
                        *old = dst;
                    }
                }
            }

            if !changed {
                break;
            }
        }

        post_order.retain(|inst| derivatives.row(*inst).is_some());

        LiveDerivatives { derivatives, post_order }
    }

    pub fn of_inst(&self, inst: Inst) -> Option<&HybridBitSet<Unkown>> {
        self.derivatives.row(inst)
    }
}

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
    dfg: &'a DataFlowGraph,
    visited: BitSet<Inst>,
    visit_stack: Vec<(Inst, InstUseIter<'a>)>,
}

impl<'a> Postorder<'a> {
    pub fn new(dfg: &'a DataFlowGraph, entries: &BitSet<Value>) -> Postorder<'a> {
        let mut po =
            Postorder { dfg, visited: BitSet::new_empty(dfg.num_insts()), visit_stack: Vec::new() };

        for val in entries.iter() {
            for use_ in dfg.uses(val) {
                po.transverse_use(use_)
            }
        }

        po.traverse_successor();

        po
    }

    fn traverse_successor(&mut self) {
        while let Some(use_) = self.visit_stack.last_mut().and_then(|(_, iter)| iter.next()) {
            self.transverse_use(use_);
        }
    }

    fn transverse_use(&mut self, use_: Use) {
        let inst = self.dfg.use_to_operand(use_).0;
        if self.visited.insert(inst) {
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
