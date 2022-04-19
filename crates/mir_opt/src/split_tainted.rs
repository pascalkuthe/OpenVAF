use bitset::{BitSet, SparseBitMatrix};
use mir::{Block, Function, Inst, InstructionData, Value};

use crate::post_dominators::PostDominanceFrontiers;

pub fn propagate_taint(
    func: &Function,
    control_dep: &PostDominanceFrontiers,
    tainted: &[Value],
) -> BitSet<Inst> {
    let inv_control_dep = control_dep.inverse();

    let mut solver = TaintSolver {
        control_dependents: &inv_control_dep,
        func,
        inst_queue: Vec::new(),
        tainted_insts: BitSet::new_empty(func.dfg.num_insts()),
        tainted_blocks: BitSet::new_empty(func.layout.num_blocks()),
    };

    for val in tainted {
        for use_ in func.dfg.uses(*val) {
            let inst = func.dfg.use_to_operand(use_).0;
            solver.taint_inst(inst)
        }
    }

    solver.solve();
    solver.tainted_insts
}

struct TaintSolver<'a> {
    control_dependents: &'a SparseBitMatrix<Block, Block>,
    func: &'a Function,

    inst_queue: Vec<Inst>,
    tainted_insts: BitSet<Inst>,
    tainted_blocks: BitSet<Block>,
}

impl TaintSolver<'_> {
    fn taint_inst(&mut self, inst: Inst) {
        if self.tainted_insts.insert(inst) {
            self.inst_queue.push(inst);
        }
    }

    fn taint_block(&mut self, block: Block) {
        if self.tainted_blocks.insert(block) {
            for inst in self.func.layout.block_insts(block) {
                self.taint_inst(inst)
            }
        }
    }

    fn solve(&mut self) {
        while let Some(inst) = self.inst_queue.pop() {
            match self.func.dfg.insts[inst] {
                InstructionData::Branch { .. } => {
                    let bb = self.func.layout.inst_block(inst).unwrap();
                    if let Some(depenents) = self.control_dependents.row(bb) {
                        for dep in depenents.iter() {
                            self.taint_block(dep)
                        }
                    }
                    continue;
                }
                InstructionData::Jump { destination } => {
                    for inst in self.func.layout.block_insts(destination) {
                        if self.func.dfg.insts[inst].is_phi() {
                            self.taint_inst(inst)
                        } else {
                            break;
                        }
                    }
                }
                _ => (),
            }

            for use_ in self.func.dfg.inst_uses(inst) {
                let user = self.func.dfg.use_to_operand(use_).0;
                self.taint_inst(user);
            }
        }
    }
}
