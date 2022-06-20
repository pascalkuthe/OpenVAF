use bitset::BitSet;
use mir::{Block, Function, Inst, InstructionData, Value};

use crate::DominatorTree;

pub fn propagate_taint(
    func: &Function,
    dom_tree: &DominatorTree,
    tainted: &[Value],
) -> BitSet<Inst> {
    let mut solver = TaintSolver {
        dom_tree,
        func,
        inst_queue: Vec::new(),
        tainted_blocks: BitSet::new_empty(func.layout.num_blocks()),
        tainted_insts: BitSet::new_empty(func.dfg.num_insts()),
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
    dom_tree: &'a DominatorTree,
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

    fn taint_block(&mut self, mut block: Block, end: Option<Block>) {
        loop {
            if Some(block) == end {
                return;
            }

            if self.tainted_blocks.insert(block) {
                for inst in self.func.layout.block_insts(block) {
                    self.taint_inst(inst);
                }
            }

            if let Some(next) = self.dom_tree.ipdom(block) {
                block = next;
            }
        }
    }

    fn solve(&mut self) {
        while let Some(inst) = self.inst_queue.pop() {
            match self.func.dfg.insts[inst] {
                InstructionData::Branch { then_dst, else_dst, .. } => {
                    let bb = self.func.layout.inst_block(inst).unwrap();
                    let end = self.dom_tree.ipdom(bb);
                    self.taint_block(then_dst, end);
                    self.taint_block(else_dst, end);
                    // self.taint_dom_frontier_phis(bb, then_dst);
                    // self.taint_dom_frontier_phis(bb, else_dst);
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
                    continue;
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
