use bitset::{BitSet, HybridBitSet, SparseBitMatrix};
use mir::{
    DataFlowGraph, Function, Inst, InstUseIter, InstructionData, Opcode, Use, Value, ValueDef,
};

use crate::unkowns::{Unkown, Unkowns};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct LiveDerivatives {
    pub mat: SparseBitMatrix<Inst, Unkown>,
}

impl LiveDerivatives {
    pub fn build(
        func: &Function,
        unkowns: &mut Unkowns,
        extra_derivatives: &[(Value, mir::Unkown)],
    ) -> LiveDerivatives {
        let mut derivative_params = BitSet::new_empty(func.dfg.num_values());

        for param in unkowns.first_order_unkowns.raw.iter() {
            derivative_params.insert(*param);
        }

        let mut mat = SparseBitMatrix::new(func.dfg.num_values(), unkowns.len());

        for (val, unkown) in extra_derivatives {
            if let ValueDef::Result(inst, _) = func.dfg.value_def(*val) {
                mat.insert(inst, (*unkown).into());
            }
        }

        // finding the live derivatives is a simple post order visit
        // the union of the derivatives of the uses then form the live derivatives
        // However for cycels this doesn't work because not all uses can be visited before.
        //
        // For now simply running a fixpoint iteration is the only valid solution here.
        // Even for complex models we required 5 iterations at most which is fine (this loop
        // isn't very performance intensive)
        //
        // If it becomes a problem we might look into only reprocessing users of cycels

        let post_order: Vec<_> = Postorder::new(&func.dfg, &derivative_params).collect();
        let mut live_derivatives = LiveDerivatives { mat };

        loop {
            let mut changed = false;

            for inst in post_order.iter().copied() {
                let opcode = func.dfg.insts[inst].opcode();
                let no_derivative = matches!(
                    opcode,
                    Opcode::Ineg
                        | Opcode::Iadd
                        | Opcode::Isub
                        | Opcode::Imul
                        | Opcode::Idiv
                        | Opcode::Ishl
                        | Opcode::Ishr
                        | Opcode::IFcast
                        | Opcode::BIcast
                        | Opcode::IBcast
                        | Opcode::FBcast
                        | Opcode::BFcast
                        | Opcode::FIcast
                        | Opcode::Irem
                        | Opcode::Inot
                        | Opcode::Ixor
                        | Opcode::Iand
                        | Opcode::Ior
                        | Opcode::Clog2
                        | Opcode::Frem
                        | Opcode::Floor
                        | Opcode::Ceil
                        | Opcode::Bnot
                        | Opcode::Ilt
                        | Opcode::Igt
                        | Opcode::Flt
                        | Opcode::Fgt
                        | Opcode::Ile
                        | Opcode::Ige
                        | Opcode::Fle
                        | Opcode::Fge
                        | Opcode::Ieq
                        | Opcode::Feq
                        | Opcode::Seq
                        | Opcode::Beq
                        | Opcode::Ine
                        | Opcode::Fne
                        | Opcode::Sne
                        | Opcode::Bne
                        | Opcode::Br
                        | Opcode::Jmp
                );

                if no_derivative {
                    continue;
                }
                // zero no need to store the derivative
                let mut dst = live_derivatives.compute_inst(inst, func, unkowns);
                if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                    if unkowns.ddx_calls.contains_key(&func_ref) {
                        let old = dst.clone();
                        let (pos_unkowns, neg_unkowns) = &unkowns.ddx_calls[&func_ref];
                        for ddx_unkown in pos_unkowns.iter().chain(neg_unkowns.iter()) {
                            for unkown in old.iter() {
                                let higher_order = unkowns.raise_order(unkown, ddx_unkown);
                                dst.insert_growable(higher_order, unkowns.len());
                            }

                            dst.insert(ddx_unkown.into(), unkowns.len());
                        }
                    } else {
                        continue; // TODO call derivatives
                    }
                }

                if !dst.is_empty() {
                    let old = live_derivatives.mat.ensure_row(inst);
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

        // post_order.retain(|inst| live_derivatives.mat.row(*inst).is_some());
        live_derivatives
    }

    pub fn of_inst(&self, inst: Inst) -> Option<&HybridBitSet<Unkown>> {
        self.mat.row(inst)
    }

    pub fn compute_inst(
        &self,
        inst: Inst,
        func: &Function,
        unkowns: &Unkowns,
    ) -> HybridBitSet<Unkown> {
        let mut dst = HybridBitSet::new_empty();
        for val in func.dfg.inst_results(inst) {
            for use_ in func.dfg.uses(*val) {
                let user = func.dfg.use_to_operand(use_).0;
                if let Some(row) = self.mat.row(user) {
                    dst.union(row, unkowns.len());
                }
            }
        }
        dst
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
