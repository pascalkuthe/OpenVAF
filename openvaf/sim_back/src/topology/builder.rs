use ahash::AHashMap;
use bitset::BitSet;
use hir::CompilationDB;
use mir::builder::InstBuilder;
use mir::cursor::{Cursor, FuncCursor};
use mir::{Block, ControlFlowGraph, Function, Inst, InstructionData, Opcode, Value, F_ZERO};

use crate::topology::Topology;

pub(super) struct Builder<'a> {
    pub(super) topology: &'a mut Topology,
    pub(super) db: &'a CompilationDB,
    pub(super) func: &'a mut Function,
    pub(super) cfg: &'a ControlFlowGraph,
    pub(super) output_values: &'a BitSet<Value>,
    pub(super) scratch_buf: BitSet<Inst>,
    pub(super) postorder: Vec<Inst>,
    pub(super) val_map: AHashMap<Value, Value>,
    pub(super) edges: Vec<(Block, Value)>,
    pub(super) phis: Vec<Inst>,
    pub(super) op_dependent_insts: &'a BitSet<Inst>,
    pub(super) op_dependent_vals: &'a [Value],
}

impl<'a> Builder<'a> {
    /// Turns one (or multiple) linear contributions into a separate dimension.
    /// That means that `val` gets replaced with 0 (altough not handeled in this function yet)
    /// and all dependent calculations will use `dim_val` multiplied with the same value
    /// as `val`.
    pub(super) fn create_dimension(&mut self, dim_val: Value, val: Value) {
        self.val_map.clear();
        self.val_map.insert(val, dim_val);
        for &inst in self.postorder.iter().rev() {
            macro_rules! ins {
                () => {
                    FuncCursor::new(self.func).after_inst(inst).ins()
                };
            }

            let res = match self.func.dfg.insts[inst] {
                InstructionData::Binary { opcode: Opcode::Fadd, args } => {
                    match (&self.val_map.get(&args[0]), &self.val_map.get(&args[1])) {
                        (None, None) => continue,
                        (None, Some(&arg)) | (Some(&arg), None) => arg,
                        (Some(&lhs), Some(&rhs)) => ins!().fadd(lhs, rhs),
                    }
                }
                InstructionData::Binary { opcode: Opcode::Fsub, args } => {
                    match (&self.val_map.get(&args[0]), &self.val_map.get(&args[1])) {
                        (None, None) => continue,
                        (None, Some(&arg)) => ins!().fneg(arg),
                        (Some(&arg), None) => arg,
                        (Some(&lhs), Some(&rhs)) => ins!().fsub(lhs, rhs),
                    }
                }
                InstructionData::Unary { opcode: Opcode::Fneg, arg } => {
                    if let Some(&arg) = self.val_map.get(&arg) {
                        ins!().fneg(arg)
                    } else {
                        continue;
                    }
                }
                InstructionData::Binary { opcode: Opcode::Fmul, args: [lhs, rhs] } => {
                    match (&self.val_map.get(&lhs), &self.val_map.get(&rhs)) {
                        (None, None) | (Some(_), Some(_)) => continue,
                        (None, Some(&rhs)) => ins!().fmul(lhs, rhs),
                        (Some(&lhs), None) => ins!().fmul(lhs, rhs),
                    }
                }
                InstructionData::Binary { opcode: Opcode::Fdiv, args: [num, denom] } => {
                    if let Some(&num) = self.val_map.get(&num) {
                        ins!().fdiv(num, denom)
                    } else {
                        continue;
                    }
                }
                InstructionData::PhiNode(_) => {
                    self.phis.push(inst);
                    // delay phi construction as there could be loops in the DFG
                    self.func.dfg.make_invalid_value()
                }
                InstructionData::Unary { opcode: Opcode::OptBarrier, arg } => {
                    if let Some(&arg) = self.val_map.get(&arg) {
                        arg
                    } else {
                        continue;
                    }
                }
                _ => {
                    continue;
                }
            };
            self.val_map.insert(self.func.dfg.first_result(inst), res);
        }
        // now that all values have been built we can popluate the phis
        for inst in self.phis.drain(..) {
            let res = self.val_map[&self.func.dfg.first_result(inst)];
            let phi = self.func.dfg.insts[inst].unwrap_phi();
            self.edges.clear();
            for (bb, mut val) in
                phi.edges(&self.func.dfg.insts.value_lists, &self.func.dfg.phi_forest)
            {
                val = self.val_map.get(&val).copied().unwrap_or(F_ZERO);
                self.edges.push((bb, val));
            }
            FuncCursor::new(self.func).after_inst(inst).ins().with_result(res).phi(&self.edges);
        }
        self.func.dfg.replace_uses(val, F_ZERO);
    }
}
