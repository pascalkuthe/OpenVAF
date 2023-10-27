use bitset::{BitSet, HybridBitSet, SparseBitMatrix};
use mir::{Block, ControlFlowGraph, Function, Inst, InstructionData, Value};

use mir::DominatorTree;

pub fn propagate_taint(
    func: &Function,
    dom_tree: &DominatorTree,
    cfg: &ControlFlowGraph,
    tainted: impl Iterator<Item = Value>,
    tainted_insts: &mut BitSet<Inst>,
) {
    tainted_insts.ensure(func.dfg.num_insts());
    let mut solver = TaintSolver {
        dom_tree,
        func,
        inst_queue: Vec::new(),
        tainted_blocks: BitSet::new_empty(func.layout.num_blocks()),
        tainted_insts,
        cfg,
        bb_queue: Vec::new(),
    };

    for val in tainted {
        for use_ in func.dfg.uses(val) {
            let inst = func.dfg.use_to_operand(use_).0;
            solver.taint_inst(inst)
        }
    }

    solver.solve();
}

struct TaintSolver<'a> {
    dom_tree: &'a DominatorTree,
    cfg: &'a ControlFlowGraph,
    func: &'a Function,
    tainted_insts: &'a mut BitSet<Inst>,

    inst_queue: Vec<Inst>,
    bb_queue: Vec<Block>,
    tainted_blocks: BitSet<Block>,
}

impl TaintSolver<'_> {
    fn taint_inst(&mut self, inst: Inst) {
        if self.tainted_insts.insert(inst) {
            self.inst_queue.push(inst);
        }
    }

    fn taint_block(&mut self, mut bb: Block, end: Option<Block>) {
        // TODO: benchmark whether permanent hashmap is faster?
        let mut visited = HybridBitSet::new_empty();
        loop {
            loop {
                if Some(bb) == end {
                    break;
                }
                if self.tainted_blocks.insert(bb) {
                    for inst in self.func.layout.block_insts(bb) {
                        self.taint_inst(inst);
                    }
                }
                let mut successors = self.cfg.succ_iter(bb);
                // enumlate tail recursion
                if let Some(succ) =
                    successors.find(|&bb| visited.insert(bb, self.func.layout.num_blocks()))
                {
                    bb = succ;
                } else {
                    break;
                }
                for succ in successors {
                    if visited.insert(succ, self.func.layout.num_blocks()) {
                        self.bb_queue.push(succ);
                    }
                }
            }
            if let Some(next) = self.bb_queue.pop() {
                bb = next
            } else {
                break;
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

pub fn propagate_direct_taint(
    func: &Function,
    dom_frontiers: &SparseBitMatrix<Block, Block>,
    tainted: impl Iterator<Item = Value>,
    tainted_insts: &mut BitSet<Inst>,
) {
    tainted_insts.ensure(func.dfg.num_insts());
    let mut solver =
        DirectTaintSolver { func, inst_queue: Vec::new(), tainted_insts, dom_frontiers };

    for val in tainted {
        for use_ in func.dfg.uses(val) {
            let inst = func.dfg.use_to_operand(use_).0;
            solver.taint_inst(inst)
        }
    }

    solver.solve();
}

struct DirectTaintSolver<'a> {
    dom_frontiers: &'a SparseBitMatrix<Block, Block>,
    func: &'a Function,
    tainted_insts: &'a mut BitSet<Inst>,
    inst_queue: Vec<Inst>,
}

impl DirectTaintSolver<'_> {
    fn taint_inst(&mut self, inst: Inst) {
        if self.tainted_insts.insert(inst) {
            self.inst_queue.push(inst);
        }
    }
    fn taint_dom_frontier_phis(&mut self, frontiers: impl Iterator<Item = Block>) {
        for dom_frontier in frontiers {
            for inst in self.func.layout.block_insts(dom_frontier) {
                if self.func.dfg.insts[inst].is_phi() {
                    self.taint_inst(inst)
                } else {
                    break;
                }
            }
        }
    }

    fn solve(&mut self) {
        while let Some(inst) = self.inst_queue.pop() {
            if let InstructionData::Branch { then_dst, else_dst, .. } = self.func.dfg.insts[inst] {
                if let Some(frontiers) = self.dom_frontiers.row(then_dst) {
                    self.taint_dom_frontier_phis(frontiers.iter());
                }
                if let Some(frontiers) = self.dom_frontiers.row(else_dst) {
                    self.taint_dom_frontier_phis(frontiers.iter());
                }
            } else {
                for use_ in self.func.dfg.inst_uses(inst) {
                    let user = self.func.dfg.use_to_operand(use_).0;
                    self.taint_inst(user);
                }
            }
        }
    }
}
