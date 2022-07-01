use std::mem::replace;

use bitset::{BitSet, HybridBitSet, SparseBitSet};
use mir::{Block, DominatorTree, Function, Inst, Opcode, Value, ValueDef};

use crate::unkowns::{Unkown, Unkowns};
use crate::{zero_derivative, ChainRule, LiveDerivatives};

pub struct SubGraphExplorer<'a, 'b> {
    derivatives: &'a mut LiveDerivatives,
    func: &'a Function,
    unkowns: &'a Unkowns<'b>,
    dom_tree: &'a DominatorTree,
    outputs: &'a BitSet<Inst>,

    workqueue: Vec<Inst>,
    curr_subgraph: BitSet<Inst>,
    curr_subgraph_dominator: Block,

    curr_subgraph_unkowns: HybridBitSet<Unkown>,
    off: u32,

    saved_insts: i32,
}

impl<'a, 'b> SubGraphExplorer<'a, 'b> {
    pub fn new(
        derivatives: &'a mut LiveDerivatives,
        func: &'a Function,
        unkowns: &'a Unkowns<'b>,
        dom_tree: &'a DominatorTree,
        outputs: &'a BitSet<Inst>,
    ) -> SubGraphExplorer<'a, 'b> {
        let off =
            unkowns.first_order_unkowns.len() as u32 + unkowns.higher_order_unkowns.len() as u32;

        SubGraphExplorer {
            derivatives,
            func,
            unkowns,
            dom_tree,
            outputs,
            workqueue: Vec::new(),
            curr_subgraph: BitSet::new_empty(func.dfg.num_insts()),
            curr_subgraph_unkowns: HybridBitSet::new_empty(),
            off,
            saved_insts: 0,
            curr_subgraph_dominator: 0u32.into(),
        }
    }

    fn as_subgraph_entry(&mut self, inst: Inst) -> Option<(SparseBitSet<Unkown>, Value)> {
        self.curr_subgraph_dominator = self.func.layout.inst_block(inst).unwrap();

        let unkowns = if let Some(HybridBitSet::Sparse(unkowns)) = self.derivatives.mat.row(inst) {
            unkowns.clone()
        } else {
            return None;
        };

        let args = self.func.dfg.instr_args(inst);
        let results = self.func.dfg.inst_results(inst);
        if unkowns.len() < 2 || args.len() < 2 || results.len() != 1 {
            return None;
        }

        if self.func.dfg.uses_head_cursor(self.func.dfg.first_result(inst))
            == self.func.dfg.uses_tail_cursor(self.func.dfg.first_result(inst))
        {
            // just a single use... no need ;)
            return None;
        }

        // for arg in args {
        //     if let Some(inst) = self.func.dfg.value_def(*arg).inst() {
        //         if let Some(derivatives) = self.derivatives.mat.row(inst) {
        //             if unkowns.iter().all(|unkown| derivatives.contains(*unkown)) {
        //                 return None;
        //             }
        //         }
        //     }
        // }

        self.curr_subgraph_unkowns = HybridBitSet::Sparse(unkowns.clone());

        for unkown in unkowns.iter().copied() {
            let idx = u32::from(unkown);
            if idx >= self.off {
                self.curr_subgraph_unkowns.union(
                    &self.derivatives.completed_subgraphs[(idx - self.off) as usize].1,
                    self.off as usize + self.derivatives.completed_subgraphs.len(),
                );
            }
        }

        Some((unkowns, results[0]))
    }

    pub fn run(&mut self) {
        for bb in self.dom_tree.cfg_postorder().iter().rev() {
            for inst in self.func.layout.block_insts(*bb) {
                self.explore_subgraph(inst)
            }
        }
    }

    fn explore_subgraph(&mut self, inst: Inst) {
        let entry = if let Some(entry) = self.as_subgraph_entry(inst) {
            entry
        } else {
            return;
        };

        self.workqueue
            .extend(self.func.dfg.inst_uses(inst).map(|use_| self.func.dfg.use_to_operand(use_).0));

        self.curr_subgraph.insert(inst);

        while let Some(inst) = self.workqueue.pop() {
            if zero_derivative(&self.func.dfg, inst)
                || self.func.dfg.insts[inst].opcode() == Opcode::Call
            {
                continue;
            }

            if let Some(row) = self.derivatives.mat.row(inst) {
                if self.curr_subgraph_unkowns.iter().all(|unkown| !row.contains(unkown)) {
                    continue;
                }
            } else {
                continue;
            }

            if !self.inside_curr_subgraph(inst) {
                continue;
            }
            self.curr_subgraph.insert(inst);
            for use_ in self.func.dfg.inst_uses(inst) {
                let (inst, _) = self.func.dfg.use_to_operand(use_);
                if !self.curr_subgraph.contains(inst) {
                    self.workqueue.push(inst)
                }
            }
        }

        self.curr_subgraph.remove(inst);

        // reusing allocation of the workqueue
        let edges = &mut self.workqueue;
        let mut num_insts = 0;

        for inst in self.curr_subgraph.iter() {
            num_insts += 1;
            if self.outputs.contains(inst) {
                edges.push(inst);
                continue;
            }

            for use_ in self.func.dfg.inst_uses(inst) {
                let (user, _) = self.func.dfg.use_to_operand(use_);
                if !self.curr_subgraph.contains(user) {
                    edges.push(inst);
                    break;
                }
            }
        }

        let num_unkowns = entry.0.len() as u32;
        let saved_insts = num_insts * (num_unkowns - 1);
        let extra_inst_approx = edges.len() as u32 * num_unkowns;

        if saved_insts > extra_inst_approx + 4
            && (saved_insts - extra_inst_approx) * 100 / num_insts > 15
        {
            let mut extra_inst = 0;

            let mut num_unkowns = self.off + self.derivatives.completed_subgraphs.len() as u32;
            let new_unkown = Unkown::from(num_unkowns);
            num_unkowns += 1;

            let unkowns = replace(&mut self.curr_subgraph_unkowns, HybridBitSet::new_empty());
            let val = self.func.dfg.first_result(inst);
            // this is actually worth it
            self.derivatives.completed_subgraphs.push((val, unkowns));

            for mut inst in edges.iter().copied() {
                let val = if let Some(val) = self.func.dfg.inst_results(inst).get(0) {
                    *val
                } else {
                    continue;
                };

                let row = self.derivatives.mat.row(inst).unwrap();
                while let Some(next) = self.func.layout.next_inst(inst) {
                    if self.func.dfg.insts[next].is_phi() {
                        inst = next;
                    } else {
                        break;
                    }
                }

                let dst = self.derivatives.conversions.entry(inst).or_default();

                for unkown in entry.0.iter() {
                    if row.contains(*unkown) {
                        dst.push(ChainRule {
                            unkown: *unkown,
                            out_derivative_unkown: u32::from(new_unkown).into(),
                            outer_val: val,
                        });
                        extra_inst += 1;
                    }
                }
            }

            debug_assert_ne!(extra_inst, 0);

            for inst in self.curr_subgraph.iter() {
                let row = self.derivatives.mat.row_mut(inst).unwrap();
                let changed = row.subtract(&entry.0);
                debug_assert!(changed);
                row.insert_growable(new_unkown, num_unkowns as usize);
            }

            self.saved_insts += saved_insts as i32 - extra_inst as i32;
        }

        edges.clear();
        self.curr_subgraph.clear();
    }

    fn inside_curr_subgraph(&self, inst: Inst) -> bool {
        for arg in self.func.dfg.instr_args(inst) {
            match self.func.dfg.value_def(*arg) {
                ValueDef::Result(inst, _) if !self.curr_subgraph.contains(inst) => {
                    match self.derivatives.mat.row(inst) {
                        Some(HybridBitSet::Sparse(row))
                            if row.iter().any(|unkown| {
                                if u32::from(*unkown) >= self.off {
                                    let set = &self.derivatives.completed_subgraphs
                                        [usize::from(*unkown) - self.off as usize]
                                        .1;
                                    self.curr_subgraph_unkowns
                                        .iter()
                                        .any(|unkown| set.contains(unkown))
                                } else {
                                    let unkown = self.unkowns.to_first_order(*unkown);
                                    self.curr_subgraph_unkowns.contains(unkown.into())
                                }
                            }) =>
                        {
                            return false;
                        }
                        Some(HybridBitSet::Dense(_)) => return false,

                        _ => (),
                    }
                }
                ValueDef::Param(_) => {
                    if let Some(unkown) = self.unkowns.first_order_unkowns.index(arg) {
                        return !self.curr_subgraph_unkowns.contains(unkown.into());
                    }
                }
                _ => (),
            }
        }

        if self.func.dfg.insts[inst].is_phi() {
            let bb = self.func.layout.inst_block(inst).unwrap();
            self.dom_tree.dominates(bb, self.curr_subgraph_dominator)
        } else {
            true
        }
    }
}
