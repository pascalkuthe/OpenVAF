use bitset::BitSet;
use mir::flowgraph::Successors;
use mir::{
    Block, ControlFlowGraph, Function, Inst, InstructionData, Opcode, PhiNode, Value, ValueDef,
    FALSE, TRUE,
};
use typed_index_collections::TiVec;

use crate::simplify::SimplifyCtx;

#[cfg(test)]
mod tests;

pub fn sparse_conditional_constant_propagation(func: &mut Function, cfg: &ControlFlowGraph) {
    let vals = (0..func.dfg.num_values())
        .map(|val| match func.dfg.value_def(val.into()) {
            ValueDef::Const(_) => FlatSet::Elem(val.into()),
            ValueDef::Param(_) | ValueDef::Invalid => FlatSet::Top,
            ValueDef::Result(_, _) => FlatSet::Bottom,
        })
        .collect();

    let feasible_edges = vec![Successors::default(); func.layout.num_blocks()].into();
    let executable_blocks = BitSet::new_empty(func.layout.num_blocks());

    let mut solver = ConstSolver {
        vals,
        func,
        cfg,
        overdef_work_list: Vec::with_capacity(64),
        inst_work_list: Vec::with_capacity(64),
        block_work_list: Vec::with_capacity(64),
        feasible_edges,
        executable_blocks,
    };

    solver.solve();

    let vals = solver.vals;
    let executable_blocks = solver.executable_blocks;

    for (val, lattice) in vals.iter_enumerated() {
        if let FlatSet::Elem(const_) = lattice {
            if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
                func.dfg.replace_uses(val, *const_);
                if func.dfg.inst_results(inst).len() == 1 {
                    func.dfg.zap_inst(inst);
                    func.layout.remove_inst(inst)
                }
            }
        } else if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
            if let Some(bb) = func.layout.inst_block(inst) {
                if !executable_blocks.contains(bb) {
                    func.dfg.zap_inst(inst);
                    func.layout.remove_inst(inst)
                }
            }
        }
    }
    for bb in func.layout.blocks() {
        if !executable_blocks.contains(bb) {
            if let Some(last_inst) = func.layout.last_inst(bb) {
                // break loops so bb simplify has an easier time
                if let InstructionData::Branch { cond, .. } = &mut func.dfg.insts[last_inst] {
                    *cond = FALSE;
                }
            }
        }
    }
}

/// Extends a type `T` with top and bottom elements to make it a partially ordered set in which no
/// value of `T` is comparable with any other. A flat set has the following [Hasse diagram]:
///
/// ```text
///         top
///       / /  \ \
/// all possible values of `T`
///       \ \  / /
///        bottom
/// ```
///
/// [Hasse diagram]: https://en.wikipedia.org/wiki/Hasse_diagram
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FlatSet {
    Top,
    Elem(Value),
    Bottom,
}

pub struct ConstSolver<'a> {
    vals: TiVec<Value, FlatSet>,
    func: &'a mut Function,
    cfg: &'a ControlFlowGraph,
    overdef_work_list: Vec<Inst>,
    inst_work_list: Vec<Inst>,
    block_work_list: Vec<Block>,
    feasible_edges: TiVec<Block, Successors>,
    executable_blocks: BitSet<Block>,
}

impl ConstSolver<'_> {
    pub fn solve(&mut self) {
        let entry = if let Some(entry) = self.func.layout.entry_block() {
            entry
        } else {
            return;
        };

        self.executable_blocks.insert(entry);
        self.block_work_list.push(entry);

        while !self.block_work_list.is_empty()
            || !self.inst_work_list.is_empty()
            || !self.overdef_work_list.is_empty()
        {
            // separate overdef worklist to drive the solver to termination faster
            while let Some(inst) = self.overdef_work_list.pop() {
                if let Some(bb) = self.func.layout.inst_block(inst) {
                    if self.executable_blocks.contains(bb) {
                        self.eval::<true>(bb, inst, false);
                    }
                }
            }

            while let Some(inst) = self.inst_work_list.pop() {
                if let Some(bb) = self.func.layout.inst_block(inst) {
                    if self.executable_blocks.contains(bb) {
                        self.eval::<true>(bb, inst, false);
                    }
                }
            }
            while let Some(bb) = self.block_work_list.pop() {
                let eval_phis = self.should_eval_phis(bb);
                let mut cursor = self.func.layout.block_inst_cursor(bb);
                while let Some(inst) = cursor.next(&self.func.layout) {
                    self.eval::<false>(bb, inst, eval_phis);
                }
            }
        }
    }

    pub fn eval<const DETERMINE_EVAL_PHI: bool>(
        &mut self,
        bb: Block,
        inst: Inst,
        mut eval_phi: bool,
    ) {
        match self.func.dfg.insts[inst].clone() {
            InstructionData::Unary { opcode, arg } => self.eval_unary(opcode, arg, inst),
            InstructionData::Binary { opcode, args } => {
                self.eval_binary(opcode, args[0], args[1], inst)
            }
            InstructionData::Branch { cond, then_dst, else_dst, .. } => match self.vals[cond] {
                FlatSet::Elem(TRUE) => self.mark_edge_feasible(bb, then_dst),
                FlatSet::Elem(FALSE) => self.mark_edge_feasible(bb, else_dst),
                _ => {
                    self.mark_edge_feasible(bb, else_dst);
                    self.mark_edge_feasible(bb, then_dst);
                }
            },
            InstructionData::PhiNode(phi) => {
                if DETERMINE_EVAL_PHI {
                    eval_phi = self.should_eval_phis(bb)
                }
                self.eval_phi(phi, bb, inst, eval_phi)
            }
            InstructionData::Jump { destination } => {
                self.mark_edge_feasible(bb, destination);
            }
            InstructionData::Call { .. } => {
                let mut i = 0;
                loop {
                    let res = self.func.dfg.inst_results(inst);
                    if i >= res.len() {
                        break;
                    }
                    let res = res[i];
                    self.mark_overdefinied(res);
                    i += 1;
                }
            }
        }
    }

    fn eval_phi(&mut self, phi: PhiNode, bb: Block, inst: Inst, eval_phi: bool) {
        let res = self.func.dfg.first_result(inst);
        let mut lattice = self.vals[res];

        if lattice == FlatSet::Top {
            return;
        } else if !eval_phi {
            self.mark_overdefinied(res);
            return;
        }

        for (pred, val) in self.func.dfg.phi_edges(&phi) {
            if !self.feasible_edges[pred].contains(bb) {
                continue;
            }

            let incoming_lattice = self.vals[val];
            match (incoming_lattice, lattice) {
                (FlatSet::Top, _) => {
                    self.mark_overdefinied(res);
                    return;
                }

                (_, FlatSet::Top) => {
                    debug_assert!(false, "loop should have breaked earlier");
                    return;
                }

                (FlatSet::Elem(old), FlatSet::Elem(new)) if new != old => {
                    self.mark_overdefinied(res);
                    return;
                }

                _ => {
                    lattice = incoming_lattice;
                }
            }
        }

        if let FlatSet::Elem(val) = lattice {
            self.mark_const(res, val)
        }
    }

    fn eval_unary(&mut self, op: Opcode, arg: Value, inst: Inst) {
        let lattice = self.vals[arg];
        match lattice {
            FlatSet::Bottom => (),
            FlatSet::Elem(arg) => {
                let mut simplify = SimplifyCtx::<f64, _>::new(self.func, |val, _| {
                    if let FlatSet::Elem(const_val) = self.vals[val] {
                        const_val
                    } else {
                        val
                    }
                });
                if let Some(val) = simplify.simplify_unary_op(op, arg) {
                    self.mark_inst_const(inst, val)
                } else {
                    self.mark_inst_overdefinied(inst)
                }
            }
            FlatSet::Top => self.mark_inst_overdefinied(inst),
        }
    }

    fn eval_binary(&mut self, op: Opcode, lhs: Value, rhs: Value, inst: Inst) {
        let lhs_lattice = self.vals[lhs];
        let rhs_lattice = self.vals[rhs];
        let (lhs, rhs, overdef) = match (lhs_lattice, rhs_lattice) {
            (FlatSet::Top, FlatSet::Elem(rhs)) => (lhs, rhs, true),
            (FlatSet::Elem(lhs), FlatSet::Top) => (lhs, rhs, true),
            (FlatSet::Bottom, FlatSet::Top) | (FlatSet::Top, FlatSet::Bottom | FlatSet::Top) => {
                (lhs, rhs, true)
            }
            (FlatSet::Elem(lhs), FlatSet::Elem(rhs)) => (lhs, rhs, false),
            (FlatSet::Elem(lhs), FlatSet::Bottom) => (lhs, rhs, false),
            (FlatSet::Bottom, FlatSet::Elem(rhs)) => (lhs, rhs, false),
            (FlatSet::Bottom, FlatSet::Bottom) => (lhs, rhs, false),
        };

        let mut simplify = SimplifyCtx::<f64, _>::new(self.func, |val, _| {
            if let FlatSet::Elem(const_val) = self.vals[val] {
                const_val
            } else {
                val
            }
        });

        if let Some(val) = simplify.simplify_binop(op, lhs, rhs) {
            if self.func.dfg.value_def(val).as_const().is_some() {
                self.mark_inst_const(inst, val);
                return;
            }
        }

        if overdef {
            self.mark_inst_overdefinied(inst)
        }
    }

    fn mark_inst_const(&mut self, inst: Inst, val: Value) {
        let res = self.func.dfg.first_result(inst);
        self.mark_const(res, val);
    }

    fn mark_inst_overdefinied(&mut self, inst: Inst) {
        let res = self.func.dfg.first_result(inst);
        self.mark_overdefinied(res);
    }

    fn mark_overdefinied(&mut self, val: Value) {
        if self.vals[val] != FlatSet::Top {
            self.vals[val] = FlatSet::Top;
            for use_ in self.func.dfg.uses(val) {
                let inst = self.func.dfg.use_to_operand(use_).0;
                self.overdef_work_list.push(inst);
            }
        }
    }

    fn mark_const(&mut self, dst: Value, val: Value) {
        if !matches!(self.vals[dst], FlatSet::Elem(_)) {
            self.vals[dst] = FlatSet::Elem(val);
            for use_ in self.func.dfg.uses(dst) {
                let inst = self.func.dfg.use_to_operand(use_).0;
                self.inst_work_list.push(inst);
            }
        }
    }

    fn mark_edge_feasible(&mut self, src: Block, dst: Block) {
        if !self.feasible_edges[src].insert(dst) {
            return;
        }

        if self.executable_blocks.insert(dst) {
            self.block_work_list.push(dst)
        } else {
            // block is already executable but we added a new edge (predecessor) so we need to
            // revisit phis (and dependent values)
            let eval_phis = self.should_eval_phis(dst);
            let mut cursor = self.func.layout.block_inst_cursor(dst);
            while let Some(inst) = cursor.next(&self.func.layout) {
                if let InstructionData::PhiNode(phi) = self.func.dfg.insts[inst].clone() {
                    self.eval_phi(phi, dst, inst, eval_phis)
                } else {
                    break;
                }
            }
        }
    }

    fn should_eval_phis(&self, bb: Block) -> bool {
        // Super-extra-high-degree PHI nodes are unlikely to ever be marked constant,
        // and slow us down a lot.  Just mark them overdefined.
        self.cfg.pred_iter(bb).count() <= 64
    }
}
