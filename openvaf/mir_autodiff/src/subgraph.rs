use std::mem::{replace, take};

use bitset::{BitSet, HybridBitSet, SparseBitSet};
use indexmap::IndexMap;
use mir::{Block, DominatorTree, Function, Inst, Opcode, Unknown, Value, ValueDef};

use crate::intern::{Derivative, DerivativeInfo, DerivativeIntern};
use crate::{zero_derivative, ChainRule, LiveDerivatives};

struct SubGraphExplorer<'a, 'b> {
    derivatives: &'a mut LiveDerivatives,
    func: &'a Function,
    intern: &'a mut DerivativeIntern<'b>,
    dom_tree: &'a DominatorTree,
    outputs: &'a BitSet<Inst>,

    workqueue: Vec<Inst>,
    curr_subgraph: BitSet<Inst>,
    curr_subgraph_dominator: Block,

    curr_subgraph_unknowns: HybridBitSet<Unknown>,
    off: u32,

    saved_insts: i32,
    completed_subgraphs: Vec<HybridBitSet<Unknown>>,

    derivative_map: IndexMap<Derivative, (Derivative, Derivative), ahash::RandomState>,
}

impl<'a, 'b> SubGraphExplorer<'a, 'b> {
    pub fn new(
        derivatives: &'a mut LiveDerivatives,
        func: &'a Function,
        intern: &'a mut DerivativeIntern<'b>,
        dom_tree: &'a DominatorTree,
        outputs: &'a BitSet<Inst>,
    ) -> SubGraphExplorer<'a, 'b> {
        let off = intern.unknowns.len() as u32;
        SubGraphExplorer {
            derivatives,
            func,
            intern,
            dom_tree,
            outputs,
            workqueue: Vec::new(),
            curr_subgraph: BitSet::new_empty(func.dfg.num_insts()),
            curr_subgraph_unknowns: HybridBitSet::new_empty(),
            off,
            saved_insts: 0,
            curr_subgraph_dominator: 0u32.into(),
            completed_subgraphs: Vec::new(),
            derivative_map: IndexMap::default(),
        }
    }

    fn as_subgraph_entry(&mut self, inst: Inst) -> Option<(SparseBitSet<Derivative>, Value)> {
        self.curr_subgraph_dominator = self.func.layout.inst_block(inst).unwrap();

        let derivatives =
            if let Some(HybridBitSet::Sparse(derivatives)) = self.derivatives.mat.row(inst) {
                derivatives.clone()
            } else {
                return None;
            };

        let args = self.func.dfg.instr_args(inst);
        let results = self.func.dfg.inst_results(inst);
        if derivatives.len() < 2 || args.len() < 2 || results.len() != 1 {
            return None;
        }

        self.curr_subgraph_unknowns = HybridBitSet::new_empty();

        for derivative in derivatives.iter().copied() {
            let idx: u32 = self.intern.get_unknown(derivative).into();
            if idx >= self.off {
                self.curr_subgraph_unknowns.union(
                    &self.completed_subgraphs[(idx - self.off) as usize],
                    self.intern.num_unknowns(),
                );
            } else {
                let unknown = self.intern.get_unknown(derivative);
                self.curr_subgraph_unknowns.insert(unknown, self.intern.num_unknowns());
            }
        }

        Some((derivatives, results[0]))
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
                if self
                    .curr_subgraph_unknowns
                    .iter()
                    .all(|unknown| !row.contains(self.intern.to_derivative(unknown)))
                {
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
        let mut edges = take(&mut self.workqueue);
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

        let num_unknowns = entry.0.len() as u32;
        let saved_insts = num_insts * (num_unknowns - 1);
        let extra_inst_approx = edges.len() as u32 * num_unknowns;

        if saved_insts > extra_inst_approx + 4
            && (saved_insts - extra_inst_approx) * 100 / num_insts > 15
        {
            // this is actually worth it
            let new_unknown = self.intern.ensure_unknown(entry.1);
            let new_derivative = self.intern.to_derivative(new_unknown);
            let unknowns = replace(&mut self.curr_subgraph_unknowns, HybridBitSet::new_empty());
            self.completed_subgraphs.push(unknowns);

            let mut extra_inst = 0;
            let mut i = 0;

            let curr_subgraph = take(&mut self.curr_subgraph);
            for inst in curr_subgraph.iter() {
                let row = self.derivatives.mat.row(inst).unwrap();
                self.derivative_map
                    .extend(entry.0.iter().map(|deriv| (*deriv, (new_derivative, *deriv))));

                for derivative in row.iter() {
                    if let Some(prev_order) = self.intern.previous_order(derivative) {
                        if let Some(&(new_prev_order, inner_derivative)) =
                            self.derivative_map.get(&prev_order)
                        {
                            let base = self.intern.get_unknown(derivative);
                            let new_derivative = self
                                .intern
                                .intern(DerivativeInfo {
                                    base,
                                    previous_order: Some(new_prev_order),
                                })
                                .0;
                            self.derivative_map
                                .insert(derivative, (new_derivative, inner_derivative));
                        }
                    }
                }

                if edges[i] == inst {
                    i += 1;
                    self.insert_edge_conversions(inst, entry.1, &mut extra_inst);
                }

                let row = self.derivatives.mat.row_mut(inst).unwrap();
                for (old_deriv, (new_deriv, _)) in self.derivative_map.drain(..) {
                    row.remove(old_deriv);
                    row.insert_growable(new_deriv, self.intern.num_derivatives());
                }
            }

            self.curr_subgraph = curr_subgraph;

            debug_assert_ne!(extra_inst, 0);
            self.saved_insts += saved_insts as i32 - extra_inst as i32;
        }

        edges.clear();
        self.workqueue = edges;
        self.curr_subgraph.clear();
    }

    fn insert_edge_conversions(
        &mut self,
        mut inst: Inst,
        inner_derivative_val: Value,
        extra_inst: &mut u32,
    ) {
        let val = if let Some(val) = self.func.dfg.inst_results(inst).get(0) {
            *val
        } else {
            return;
        };

        while let Some(next) = self.func.layout.next_inst(inst) {
            if self.func.dfg.insts[next].is_phi() {
                inst = next;
            } else {
                break;
            }
        }

        let dst = self.derivatives.conversions.entry(inst).or_default();

        // chain rules are interated in rev order to ensure that they cancel
        // however derivatives must be iterated in forward order to ensure they work correctly
        // therefore reverse again here so the reversals cancel
        for (&old_deriv, &(new_deriv, inner_deriv)) in self.derivative_map.iter().rev() {
            dst.push(ChainRule {
                inner_derivative: (inner_derivative_val, inner_deriv),
                outer_derivative: new_deriv,
                dst_derivative: old_deriv,
                val,
            });
            *extra_inst += 1;
        }
    }

    fn inside_curr_subgraph(&self, inst: Inst) -> bool {
        for arg in self.func.dfg.instr_args(inst) {
            match self.func.dfg.value_def(*arg) {
                ValueDef::Result(inst, _) if !self.curr_subgraph.contains(inst) => {
                    match self.derivatives.mat.row(inst) {
                        Some(HybridBitSet::Sparse(row))
                            if row.iter().any(|&derivative| {
                                let unknown = self.intern.get_unknown(derivative);
                                if u32::from(unknown) >= self.off {
                                    let set = &self.completed_subgraphs
                                        [usize::from(unknown) - self.off as usize];
                                    self.curr_subgraph_unknowns
                                        .iter()
                                        .any(|unknown| set.contains(unknown))
                                } else {
                                    self.curr_subgraph_unknowns.contains(unknown)
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
                    if let Some(unknown) = self.intern.unknowns.index(arg) {
                        return !self.curr_subgraph_unknowns.contains(unknown);
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

impl LiveDerivatives {
    pub fn run_subgraph_opt(
        &mut self,
        func: &Function,
        intern: &mut DerivativeIntern,
        extra_derivatives: &[(Value, mir::Unknown)],
        dom_tree: &DominatorTree,
        buf: BitSet<Inst>,
    ) {
        let mut outputs = buf;
        for (val, _) in extra_derivatives {
            if let Some(inst) = func.dfg.value_def(*val).inst() {
                outputs.insert(inst);
            }
        }

        let mut subgraph_opt = SubGraphExplorer::new(self, func, intern, dom_tree, &outputs);
        subgraph_opt.run();
    }
}
