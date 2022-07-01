use ahash::AHashMap;
use bitset::{HybridBitSet, SparseBitMatrix};
use mir::{DominatorTree, Function, Inst, InstructionData, Value, ValueDef};
use workqueue::WorkQueue;

use crate::postorder::Postorder;
use crate::subgraph::SubGraphExplorer;
use crate::unkowns::{NthOrderUnkownInfo, Unkown, Unkowns};
use crate::ChainRule;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct LiveDerivatives {
    pub mat: SparseBitMatrix<Inst, Unkown>,
    pub(crate) conversions: AHashMap<Inst, Vec<ChainRule>>,
    pub(crate) completed_subgraphs: Vec<(Value, HybridBitSet<Unkown>)>,
}

impl LiveDerivatives {
    pub fn build(
        func: &Function,
        unkowns: &mut Unkowns,
        extra_derivatives: &[(Value, mir::Unkown)],
        dom_tree: &DominatorTree,
    ) -> LiveDerivatives {
        let mut post_order = Postorder::new(&func.dfg);

        // First check which first-order derivatives are actually reachable in the DFG.
        // If a derivative is not reachable at a certain
        // instruction it can be assumed that its value is 0 here.
        let mut reachable_derivatives: SparseBitMatrix<Inst, Unkown> =
            SparseBitMatrix::new(func.dfg.num_values(), unkowns.len());

        for (unkown, param) in unkowns.first_order_unkowns.iter_enumerated() {
            post_order.populate(*param);
            post_order.traverse_successor();
            for inst in &mut post_order {
                reachable_derivatives.insert(inst, unkown.into());
            }
            post_order.clear();
        }

        // If higher older derivatives are added, we insert where this higer order derivative is
        // rechable. A higher order derivative is rechable when its new base unkown or its previous
        // order are rechable (both known at this point). A higher order derivative is only
        // generated if all its unkowns are rechable at the ddx call
        let mut insert_higher_order_unkown =
            |inst: Inst, unkown, info: Option<NthOrderUnkownInfo>| {
                if let Some(info) = info {
                    for row in reachable_derivatives.row_data_mut() {
                        if row.contains(info.previous_order) && row.contains(info.base.into()) {
                            row.insert_growable(unkown, usize::from(unkown) + 1);
                        }
                    }
                    true
                } else {
                    reachable_derivatives.row(inst).map_or(false, |row| row.contains(unkown))
                }
            };

        // First we find the live derivatives at any instruction: That is any derivative that we
        // care about at a certain instruction regardless [^1]  of whether this derivative is
        // non-zero here. Most importantly this pass finds any higher order derivatives that might be
        // required.
        //
        // finding the live derivatives is a reverse data flow problem on the SSA.
        // The live derivatives at any instruction is the union of the derivatives of the uses.
        //
        //
        // [^1]: We use a postorder visit of the dataflow graph starting from the first order
        // derivative values. Therefore we do in fact ignore any derivatives which are trivally
        // zero. However this is just a performance optimzation and still produces a lot of unneded
        // derivatives. Furthermore we only generate higher order derivatives if their first order
        // derivative is reachable at this point
        let mut live_derivatives = SparseBitMatrix::new(func.dfg.num_values(), unkowns.len());

        for param in unkowns.first_order_unkowns.raw.iter() {
            post_order.populate(*param)
        }

        post_order.traverse_successor();

        for (val, unkown) in extra_derivatives {
            if let ValueDef::Result(inst, _) = func.dfg.value_def(*val) {
                live_derivatives.insert(inst, (*unkown).into());
            }
        }

        let mut workqueue: WorkQueue<_> = WorkQueue::with_none(func.dfg.num_insts());
        workqueue.extend(&mut post_order);
        let mut live_derivatives = LiveDerivatives {
            mat: live_derivatives,
            conversions: AHashMap::default(),
            completed_subgraphs: Vec::new(),
        };

        while let Some(inst) = workqueue.pop() {
            let mut dst = live_derivatives.compute_inst(inst, func, unkowns);
            if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                if unkowns.ddx_calls.contains_key(&func_ref) {
                    let old = dst.clone();
                    let (pos_unkowns, neg_unkowns) = &unkowns.ddx_calls[&func_ref];
                    for ddx_unkown in pos_unkowns.iter().chain(neg_unkowns.iter()) {
                        for unkown in old.iter() {
                            let higher_order =
                                unkowns.raise_order_with(unkown, ddx_unkown, |unkown, info| {
                                    insert_higher_order_unkown(inst, unkown, info)
                                });
                            if let Some(higher_order) = higher_order {
                                dst.insert_growable(higher_order, unkowns.len());
                            }
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
                    for val in func.dfg.instr_args(inst) {
                        if let Some(inst) = func.dfg.value_def(*val).inst() {
                            if post_order.visited.contains(inst) {
                                workqueue.insert(inst);
                            }
                        }
                    }
                    *old = dst;
                }
            }
        }

        // only keep those live derivative that are actually rechable
        live_derivatives.mat.intersect(&reachable_derivatives);
        drop(reachable_derivatives);
        let mut outputs = post_order.visited;
        outputs.clear();
        for (val, _) in extra_derivatives {
            if let Some(inst) = func.dfg.value_def(*val).inst() {
                outputs.insert(inst);
            }
        }

        let mut subgraph_opt =
            SubGraphExplorer::new(&mut live_derivatives, func, unkowns, dom_tree, &outputs);
        subgraph_opt.run();

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
