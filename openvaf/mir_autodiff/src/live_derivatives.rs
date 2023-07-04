use std::mem::take;

use ahash::AHashMap;
use bitset::{BitSet, HybridBitSet, SparseBitMatrix};
use mir::{DominatorTree, Function, Inst, InstructionData, Opcode, Value, ValueDef};
use workqueue::WorkQueue;

use crate::intern::{Derivative, DerivativeIntern};
use crate::postorder::{Postorder, PostorderParts};
use crate::ChainRule;

#[cfg(test)]
mod tests;

struct LiveDerivativeBuilder<'a, 'b> {
    live_derivatives: LiveDerivatives,
    func: &'a Function,
    intern: &'a mut DerivativeIntern<'b>,
    reachable_derivatives: SparseBitMatrix<Inst, Derivative>,
    post_order_parts: PostorderParts<'a>,
    visited: BitSet<Inst>,
}

impl<'a, 'b> LiveDerivativeBuilder<'a, 'b> {
    pub fn new(
        func: &'a Function,
        intern: &'a mut DerivativeIntern<'b>,
    ) -> LiveDerivativeBuilder<'a, 'b> {
        let mat = SparseBitMatrix::new(func.dfg.num_insts(), intern.num_derivatives());
        let post_order_parts = (BitSet::new_empty(func.dfg.num_insts()), Vec::with_capacity(64));
        LiveDerivativeBuilder {
            live_derivatives: LiveDerivatives {
                mat: mat.clone(),
                conversions: AHashMap::default(),
            },
            func,
            intern,
            reachable_derivatives: mat,
            post_order_parts,
            visited: BitSet::default(),
        }
    }

    fn depends_on(&self, val: Value, derivative: Derivative) -> bool {
        let LiveDerivativeBuilder { intern, reachable_derivatives, func, .. } = self;

        if intern.previous_order(derivative).is_none() {
            let unknown = intern.get_unknown(derivative);
            if intern.unknowns[unknown] == val {
                return true;
            }
        }

        if let Some(inst) = func.dfg.value_def(val).inst() {
            reachable_derivatives.contains(inst, derivative)
        } else {
            false
        }
    }

    /// This function deterimes which unknowns are reachable where in the DFG.
    /// If a derivative is not reachable at a certain instruction it can be assumed that its value is 0 here.
    /// The results are stored in `self.reachable_derivatives`.
    fn populate_reachable_unknowns(&mut self) {
        let mut post_order =
            Postorder::from_parts(&self.func.dfg, take(&mut self.post_order_parts), self.intern);

        for (unknown, param) in self.intern.unknowns.iter_enumerated() {
            post_order.populate(*param);
            post_order.traverse_successor();
            for inst in &mut post_order {
                self.reachable_derivatives.insert(inst, self.intern.to_derivative(unknown));
            }
            post_order.clear();
        }

        self.post_order_parts = post_order.into_parts();
    }

    /// This function deterimes where a derivative (of higher order) is reachable in the DFG.
    /// If a derivative is not reachable at a certain instruction it can be assumed that its value is 0 here.
    /// The results are stored in `self.reachable_derivatives`.
    ///
    /// This function must be called whenever a new higher order derivative is created
    fn populate_reachable(&mut self, derivative: Derivative) {
        let mut post_order =
            Postorder::from_parts(&self.func.dfg, take(&mut self.post_order_parts), self.intern);
        post_order.clear();
        for inst in self.reachable_derivatives.rows() {
            let base = self.intern.get_base_derivative(derivative);
            let prev_order = self.intern.previous_order(derivative).unwrap();
            let row = self.reachable_derivatives.row(inst).unwrap();
            if !(row.contains(prev_order) && row.contains(base)) {
                continue;
            }

            // only some instructions spawn higher order derivatives
            // eg a + b never creates a "new" higher order derivative but a*b may
            let (args, is_div) = match self.func.dfg.insts[inst] {
                InstructionData::Binary { opcode: Opcode::Fmul, args } => (args, false),
                InstructionData::Binary { opcode: Opcode::Fdiv, args } => (args, true),
                InstructionData::Unary { opcode: Opcode::OptBarrier | Opcode::Fneg, .. }
                | InstructionData::Binary { opcode: Opcode::Fadd | Opcode::Fsub, .. } => continue,
                InstructionData::Unary { .. } | InstructionData::Binary { .. } => {
                    post_order.transverse_inst(inst);
                    continue;
                }
                _ => continue,
            };

            let arg0_prev = self.depends_on(args[0], prev_order);
            let arg0_base = self.depends_on(args[0], base);
            let arg1_prev = self.depends_on(args[1], prev_order);
            let arg1_base = self.depends_on(args[1], base);

            // a*b (and a*1/b) spawn a higher order derivative if a depends on x and b
            // depends on y (or the other way around
            // a/b also spawns a higher order derivative if b depends on both
            if (is_div && arg1_base && arg1_prev)
                || arg0_base && arg1_prev
                || arg0_prev && arg1_base
            {
                post_order.transverse_inst(inst)
            }
        }

        post_order.traverse_successor();
        for inst in &mut post_order {
            self.reachable_derivatives
                .ensure_row(inst)
                .insert_growable(derivative, self.intern.num_derivatives());
            if let Some(row) = self.reachable_derivatives.row(inst) {
                let skip = if let HybridBitSet::Dense(dense) = row {
                    dense.domain_size() < 65
                } else {
                    false
                };
                assert!(
                    skip || !(row.contains(64u32.into())
                        && self.func.dfg.first_result(inst) == 54342u32.into())
                )
            }
        }
        self.post_order_parts = post_order.into_parts();
    }

    fn initial_live_derivative_workque(&mut self) -> WorkQueue<Inst> {
        let mut post_order =
            Postorder::from_parts(&self.func.dfg, take(&mut self.post_order_parts), self.intern);
        for param in self.intern.unknowns.iter() {
            post_order.populate(*param)
        }
        post_order.traverse_successor();
        let mut workqueue: WorkQueue<_> = WorkQueue::with_none(self.func.dfg.num_insts());
        workqueue.extend(&mut post_order);
        self.visited = post_order.visited.clone();

        self.post_order_parts = post_order.into_parts();
        workqueue
    }

    /// Runs a fixpoint iteration to find the live derivatives at any instruction: That is any derivative that we
    /// care about at a certain instruction regardless [^1]  of whether this derivative is
    /// non-zero here. Most importantly this pass finds any higher order derivatives that might be
    /// required.
    ///
    /// Finding the live derivatives is a reverse data flow problem on the SSA.
    /// The live derivatives at any instruction is the union of the derivatives of the uses.
    ///
    ///
    /// [^1]: We use a postorder visit of the dataflow graph starting from the first order
    /// derivative values. Therefore we do in fact ignore any derivatives which are trivially
    /// zero. However this is just a performance optimization and still produces a lot of unneeded
    /// derivatives. Furthermore we only generate higher order derivatives if their first order
    /// derivative is reachable at this point
    fn live_derivative_fixpoint(&mut self, workqueue: &mut WorkQueue<Inst>) {
        let func = self.func;
        while let Some(inst) = workqueue.pop() {
            let mut dst = self.live_derivatives.compute_inst(inst, func, self.intern);
            if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                if self.intern.ddx_calls.contains_key(&func_ref) {
                    let old = dst.clone();
                    let (pos_unknowns, neg_unknowns) = &self.intern.ddx_calls[&func_ref];
                    for ddx_unknown in pos_unknowns.iter().chain(neg_unknowns.iter()) {
                        for unknown in old.iter() {
                            let res =
                                self.intern.raise_order_with(unknown, ddx_unknown, |unknown| {
                                    self.reachable_derivatives.contains(inst, unknown)
                                });
                            if let Some((derivative, created)) = res {
                                dst.insert_growable(derivative, self.intern.num_derivatives());
                                if created {
                                    self.populate_reachable(derivative)
                                }
                            }
                        }

                        dst.insert(
                            self.intern.to_derivative(ddx_unknown),
                            self.intern.num_derivatives(),
                        );
                    }
                } else {
                    continue; // TODO call derivatives
                }
            }

            if !dst.is_empty() {
                let old = self.live_derivatives.mat.ensure_row(inst);
                if old != &dst {
                    for val in func.dfg.instr_args(inst) {
                        if let Some(inst) = func.dfg.value_def(*val).inst() {
                            if self.visited.contains(inst) {
                                workqueue.insert(inst);
                            }
                        }
                    }
                    *old = dst;
                }
            }
        }
    }

    /// This function strip unneeded live derivatives by taking an intersection with the reachable
    /// derivatives
    fn strip_unneeded_derivatives(&mut self) {
        let mut reachable_derivatives = take(&mut self.reachable_derivatives);
        self.live_derivatives.mat.ensure_columns(self.intern.num_derivatives());
        reachable_derivatives.ensure_columns(self.intern.num_derivatives());
        self.live_derivatives.mat.intersect(&reachable_derivatives);
    }

    fn insert_extra_derivative(&mut self, extra_derivatives: &[(Value, mir::Unknown)]) {
        for (val, unknown) in extra_derivatives {
            if let ValueDef::Result(inst, _) = self.func.dfg.value_def(*val) {
                self.live_derivatives.mat.insert(inst, self.intern.to_derivative(*unknown));
            }
        }
    }

    pub fn finish(mut self) -> (LiveDerivatives, BitSet<Inst>) {
        self.visited.clear();
        (self.live_derivatives, self.visited)
    }
}

#[derive(Debug, Clone)]
pub struct LiveDerivatives {
    pub mat: SparseBitMatrix<Inst, Derivative>,
    pub(crate) conversions: AHashMap<Inst, Vec<ChainRule>>,
}

impl LiveDerivatives {
    pub fn build(
        func: &Function,
        intern: &mut DerivativeIntern,
        extra_derivatives: &[(Value, mir::Unknown)],
        dom_tree: &DominatorTree,
    ) -> LiveDerivatives {
        let mut builder = LiveDerivativeBuilder::new(func, intern);
        builder.populate_reachable_unknowns();
        builder.insert_extra_derivative(extra_derivatives);
        let mut workqueue = builder.initial_live_derivative_workque();
        builder.live_derivative_fixpoint(&mut workqueue);
        builder.strip_unneeded_derivatives();
        let (mut res, buf) = builder.finish();

        res.run_subgraph_opt(func, intern, extra_derivatives, dom_tree, buf);

        res
    }

    pub fn of_inst(&self, inst: Inst) -> Option<&HybridBitSet<Derivative>> {
        self.mat.row(inst)
    }

    pub fn compute_inst(
        &self,
        inst: Inst,
        func: &Function,
        unknowns: &DerivativeIntern,
    ) -> HybridBitSet<Derivative> {
        let mut dst = HybridBitSet::new_empty();
        for val in func.dfg.inst_results(inst) {
            for use_ in func.dfg.uses(*val) {
                let user = func.dfg.use_to_operand(use_).0;
                if let Some(row) = self.mat.row(user) {
                    dst.union(row, unknowns.num_derivatives());
                }
            }
        }
        dst
    }
}
