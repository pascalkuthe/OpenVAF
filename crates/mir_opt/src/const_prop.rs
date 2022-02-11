use std::mem::size_of_val;

use bitset::BitSet;
use mir::flowgraph::Successors;
use mir::{
    Block, Const, ControlFlowGraph, Function, Inst, InstructionData, Opcode, PhiNode, Value,
    ValueDef, FALSE, F_ONE, F_ZERO, ONE, TRUE, ZERO,
};
use typed_index_collections::TiVec;

#[cfg(test)]
mod tests;

pub fn sparse_conditional_constant_propagation(func: &mut Function, cfg: &ControlFlowGraph) {
    let vals = (0..func.dfg.num_values())
        .map(|val| match func.dfg.value_def(val.into()) {
            ValueDef::Const(_) => FlatSet::Elem(val.into()),
            ValueDef::Param(_) => FlatSet::Top,
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
                if !executable_blocks.contains(bb) && Some(inst) != func.layout.last_inst(bb) {
                    func.dfg.zap_inst(inst);
                    func.layout.remove_inst(inst)
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
            // seperate overdef worklist to drive the solver to termination faster
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
        match self.func.dfg.insts[inst] {
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

        for (pred, val) in self.func.dfg.phi_edges(phi) {
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
            FlatSet::Elem(val) => {
                let val = self.func.dfg.value_def(val).unwrap_const();
                let val = match val {
                    mir::Const::Float(val) => {
                        let val: f64 = val.into();
                        match op {
                            Opcode::Sqrt => self.func.dfg.f64const(val.sqrt()),
                            Opcode::Exp => self.func.dfg.f64const(val.exp()),
                            Opcode::Ln => self.func.dfg.f64const(val.ln()),
                            Opcode::Log => self.func.dfg.f64const(val.log10()),
                            Opcode::Floor => self.func.dfg.f64const(val.floor()),
                            Opcode::Ceil => self.func.dfg.f64const(val.ceil()),
                            Opcode::Sin => self.func.dfg.f64const(val.sin()),
                            Opcode::Cos => self.func.dfg.f64const(val.cos()),
                            Opcode::Tan => self.func.dfg.f64const(val.tan()),
                            Opcode::Asin => self.func.dfg.f64const(val.asin()),
                            Opcode::Acos => self.func.dfg.f64const(val.acos()),
                            Opcode::Atan => self.func.dfg.f64const(val.atan()),
                            Opcode::Sinh => self.func.dfg.f64const(val.sinh()),
                            Opcode::Cosh => self.func.dfg.f64const(val.cosh()),
                            Opcode::Tanh => self.func.dfg.f64const(val.tanh()),
                            Opcode::Asinh => self.func.dfg.f64const(val.asinh()),
                            Opcode::Acosh => self.func.dfg.f64const(val.acosh()),
                            Opcode::Atanh => self.func.dfg.f64const(val.atanh()),
                            Opcode::FIcast => self.func.dfg.iconst(val.round() as i32),
                            Opcode::FBcast => (val.abs() != 0.0).into(),
                            Opcode::Fneg => self.func.dfg.f64const(-val),
                            Opcode::OptBarrier => return self.mark_inst_overdefinied(inst),
                            _ => unreachable!(
                                "invalid real operation {} {}",
                                op,
                                self.func.dfg.display_inst(inst)
                            ),
                        }
                    }
                    mir::Const::Int(val) => match op {
                        Opcode::Inot => self.func.dfg.iconst(!val),
                        Opcode::Ineg => self.func.dfg.iconst(-val),
                        Opcode::IFcast => self.func.dfg.f64const(val as f64),
                        Opcode::IBcast => (val != 0).into(),
                        Opcode::Clog2 => {
                            let val = 8 * size_of_val(&val) as i32 - val.leading_zeros() as i32;
                            self.func.dfg.iconst(val)
                        }
                        Opcode::OptBarrier => return self.mark_inst_overdefinied(inst),
                        _ => unreachable!(
                            "invalid int operation {} {}",
                            op,
                            self.func.dfg.display_inst(inst)
                        ),
                    },
                    mir::Const::Str(_) => unreachable!(),
                    mir::Const::Bool(true) => match op {
                        Opcode::Bnot => FALSE,
                        Opcode::BIcast => ONE,
                        Opcode::BFcast => F_ONE,
                        Opcode::OptBarrier => return self.mark_inst_overdefinied(inst),
                        _ => unreachable!(),
                    },
                    mir::Const::Bool(false) => match op {
                        Opcode::Bnot => TRUE,
                        Opcode::BIcast => ZERO,
                        Opcode::BFcast => F_ZERO,
                        Opcode::OptBarrier => return self.mark_inst_overdefinied(inst),
                        _ => unreachable!(),
                    },
                };

                self.mark_inst_const(inst, val);
            }
            FlatSet::Top => self.mark_inst_overdefinied(inst),
        }
    }
    fn eval_binary(&mut self, op: Opcode, lhs: Value, rhs: Value, inst: Inst) {
        let lhs_lattice = self.vals[lhs];
        let rhs_lattice = self.vals[rhs];

        if let (FlatSet::Elem(lhs), FlatSet::Elem(rhs)) = (lhs_lattice, rhs_lattice) {
            // simple case everything is a constant so we just eval
            let lhs_ = self.func.dfg.value_def(lhs).unwrap_const();
            let rhs_ = self.func.dfg.value_def(rhs).unwrap_const();
            let val = match (lhs_, rhs_) {
                (Const::Int(lhs), Const::Int(rhs)) => match op {
                    Opcode::Iadd => self.func.dfg.iconst(lhs + rhs),
                    Opcode::Isub => self.func.dfg.iconst(lhs - rhs),
                    Opcode::Imul => self.func.dfg.iconst(lhs * rhs),
                    Opcode::Idiv => self.func.dfg.iconst(lhs / rhs),
                    Opcode::Irem => self.func.dfg.iconst(lhs % rhs),

                    Opcode::Ishl => self.func.dfg.iconst(lhs << rhs),
                    Opcode::Ishr => self.func.dfg.iconst(lhs >> rhs),
                    Opcode::Ixor => self.func.dfg.iconst(lhs ^ rhs),
                    Opcode::Iand => self.func.dfg.iconst(lhs & rhs),
                    Opcode::Ior => self.func.dfg.iconst(lhs | rhs),

                    Opcode::Ilt => (lhs < rhs).into(),
                    Opcode::Igt => (lhs > rhs).into(),
                    Opcode::Ige => (lhs >= rhs).into(),
                    Opcode::Ile => (lhs <= rhs).into(),
                    Opcode::Ieq => (lhs == rhs).into(),
                    Opcode::Ine => (lhs != rhs).into(),

                    _ => unreachable!(
                        "invalid int operation {} {}",
                        op,
                        self.func.dfg.display_inst(inst)
                    ),
                },

                (Const::Float(lhs), Const::Float(rhs)) => {
                    let lhs: f64 = lhs.into();
                    let rhs: f64 = rhs.into();
                    match op {
                        Opcode::Fadd => self.func.dfg.f64const(lhs + rhs),
                        Opcode::Fsub => self.func.dfg.f64const(lhs - rhs),
                        Opcode::Fmul => self.func.dfg.f64const(lhs * rhs),
                        Opcode::Fdiv => self.func.dfg.f64const(lhs / rhs),
                        Opcode::Frem => self.func.dfg.f64const(lhs % rhs),

                        Opcode::Flt => (lhs < rhs).into(),
                        Opcode::Fgt => (lhs > rhs).into(),
                        Opcode::Fge => (lhs >= rhs).into(),
                        Opcode::Fle => (lhs <= rhs).into(),
                        Opcode::Feq => (lhs == rhs).into(),
                        Opcode::Fne => (lhs != rhs).into(),

                        Opcode::Hypot => self.func.dfg.f64const(lhs.hypot(rhs)),
                        Opcode::Atan2 => self.func.dfg.f64const(lhs.atan2(rhs)),
                        Opcode::Pow => self.func.dfg.f64const(lhs.powf(rhs)),
                        _ => unreachable!(
                            "invalid real operation  {} {}",
                            op,
                            self.func.dfg.display_inst(inst)
                        ),
                    }
                }
                _ => match op {
                    Opcode::Ieq | Opcode::Feq | Opcode::Seq | Opcode::Beq => (lhs == rhs).into(),

                    Opcode::Ine | Opcode::Fne | Opcode::Sne | Opcode::Bne => (lhs != rhs).into(),
                    _ => unreachable!(
                        "invalid operation {} {:?} {:?} {}",
                        op,
                        lhs_,
                        rhs_,
                        self.func.dfg.display_inst(inst)
                    ),
                },
            };

            self.mark_inst_const(inst, val);
        } else {
            // exploit identities to also copy and const propagate some additional instructions
            if lhs == rhs {
                match op {
                    Opcode::Ieq
                    | Opcode::Feq
                    | Opcode::Seq
                    | Opcode::Beq
                    | Opcode::Ile
                    | Opcode::Ige
                    | Opcode::Fge
                    | Opcode::Fle => return self.mark_inst_const(inst, TRUE),
                    Opcode::Ine
                    | Opcode::Fne
                    | Opcode::Sne
                    | Opcode::Bne
                    | Opcode::Ilt
                    | Opcode::Igt
                    | Opcode::Fgt
                    | Opcode::Flt => return self.mark_inst_const(inst, FALSE),
                    Opcode::Isub | Opcode::Irem | Opcode::Ixor => {
                        return self.mark_inst_const(inst, ZERO)
                    }
                    Opcode::Idiv => return self.mark_inst_const(inst, ONE),
                    Opcode::Fsub | Opcode::Frem => return self.mark_inst_const(inst, F_ZERO),
                    Opcode::Fdiv => return self.mark_inst_const(inst, F_ONE),

                    // Opcode::Ior | Opcode::Iand | Opcode::Inxor => {
                    //     return self.mark_inst_copy(inst, lhs_lattice.val)
                    // }
                    _ => (),
                }
            }

            match op {
                Opcode::Pow | Opcode::Fmul | Opcode::Fdiv | Opcode::Frem
                    if matches!(lhs_lattice, FlatSet::Elem(F_ZERO)) =>
                {
                    self.mark_inst_const(inst, F_ZERO)
                }
                Opcode::Fmul if matches!(rhs_lattice, FlatSet::Elem(F_ZERO)) => {
                    self.mark_inst_const(inst, F_ZERO)
                }
                Opcode::Pow if matches!(rhs_lattice, FlatSet::Elem(F_ZERO)) => {
                    self.mark_inst_const(inst, F_ONE)
                }

                Opcode::Imul | Opcode::Idiv | Opcode::Irem
                    if matches!(lhs_lattice, FlatSet::Elem(ZERO)) =>
                {
                    self.mark_inst_const(inst, ZERO)
                }
                Opcode::Imul if matches!(rhs_lattice, FlatSet::Elem(ZERO)) => {
                    self.mark_inst_const(inst, ZERO)
                }
                // Opcode::Iadd if lhs_lattice.val == ZERO => {
                //     self.mark_inst_copy(inst, rhs_lattice.val)
                // }
                // Opcode::Fadd if lhs_lattice.val == F_ZERO => {
                //     self.mark_inst_copy(inst, rhs_lattice.val)
                // }
                // Opcode::Iadd | Opcode::Isub if rhs_lattice.val == ZERO => {
                //     self.mark_inst_copy(inst, lhs_lattice.val)
                // }
                // Opcode::Fadd | Opcode::Fsub if rhs_lattice.val == F_ZERO => {
                //     self.mark_inst_copy(inst, lhs_lattice.val)
                // }
                _ if lhs_lattice == FlatSet::Top || rhs_lattice == FlatSet::Top => {
                    self.mark_inst_overdefinied(inst)
                }

                _ => (),
            }
        }
    }

    fn mark_inst_const(&mut self, inst: Inst, val: Value) {
        let res = self.func.dfg.first_result(inst);
        self.mark_const(res, val);
    }

    // fn mark_inst_copy(&mut self, inst: Inst, val: Value) {
    //     let res = self.func.dfg.first_result(inst);
    //     self.mark_copy(res, val);
    // }

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
                if let InstructionData::PhiNode(phi) = self.func.dfg.insts[inst] {
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
