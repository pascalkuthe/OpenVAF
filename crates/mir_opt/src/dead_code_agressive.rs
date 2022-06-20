use bitset::{BitSet, SparseBitMatrix};
use mir::{Block, ControlFlowGraph, Function, Inst, InstructionData, Value, ValueDef};

pub type PostDominanceFrontiers = SparseBitMatrix<Block, Block>;

pub fn agressive_dead_code_elimination(
    func: &mut Function,
    cfg: &mut ControlFlowGraph,
    is_live: &dyn Fn(Value, &Function) -> bool,
    pdom_frontiers: &PostDominanceFrontiers,
) {
    let mut live_blocks = BitSet::new_empty(func.layout.num_blocks());
    live_blocks.insert(func.layout.entry_block().unwrap());
    let mut adce = AgressiveDeadCode {
        pdom_frontiers,
        live_insts: BitSet::new_empty(func.dfg.num_insts()),
        live_blocks,
        live_predecessors: BitSet::new_empty(func.layout.num_blocks()),
        live_control_flow: BitSet::new_empty(func.layout.num_blocks()),
        inst_work_list: Vec::new(),
        bb_work_list: Vec::with_capacity(64),
        func,
        cfg,
    };

    for inst in func.dfg.insts.iter() {
        if func.layout.inst_block(inst).is_some()
            && (func.dfg.has_sideeffects(inst, false)
                || func.dfg.inst_results(inst).iter().any(|val| is_live(*val, func)))
        {
            adce.mark_inst_live(inst);
        }
    }

    adce.solve();

    let dead_instructions = {
        adce.live_insts.inverse();
        adce.live_insts
    };

    let dead_blocks = {
        adce.live_blocks.inverse();
        adce.live_blocks
    };

    for inst in dead_instructions.iter() {
        func.dfg.zap_inst(inst);
        if func.layout.inst_block(inst).is_some() && !func.dfg.insts[inst].is_terminator() {
            func.layout.remove_inst(inst);
        }
    }

    for bb in dead_blocks.iter() {
        if let Some(term) = func.layout.last_inst(bb) {
            if let InstructionData::Branch { else_dst, .. } = func.dfg.insts[term] {
                func.dfg.insts[term] = InstructionData::Jump { destination: else_dst };
                cfg.recompute_block(func, bb);
            }
        }
    }
}

struct AgressiveDeadCode<'a> {
    pdom_frontiers: &'a PostDominanceFrontiers,
    live_insts: BitSet<Inst>,
    live_blocks: BitSet<Block>,
    live_predecessors: BitSet<Block>,
    live_control_flow: BitSet<Block>,
    inst_work_list: Vec<Inst>,
    bb_work_list: Vec<Block>,
    func: &'a Function,
    cfg: &'a ControlFlowGraph,
}

impl AgressiveDeadCode<'_> {
    pub fn solve(&mut self) {
        loop {
            while let Some(inst) = self.inst_work_list.pop() {
                for arg in self.func.dfg.instr_args(inst) {
                    if let ValueDef::Result(def, _) = self.func.dfg.value_def(*arg) {
                        self.mark_inst_live(def);
                    }
                }
            }

            while let Some(bb) = self.bb_work_list.pop() {
                if let Some(row) = self.pdom_frontiers.row(bb) {
                    for dep in row.iter() {
                        self.mark_term_live(dep)
                    }
                }
            }

            if self.inst_work_list.is_empty() {
                break;
            }
        }
    }

    fn mark_inst_live(&mut self, inst: Inst) -> bool {
        if self.live_insts.insert(inst) {
            self.inst_work_list.push(inst);
            let bb = self.func.layout.inst_block(inst).unwrap();
            self.mark_bb_live(bb);

            if matches!(self.func.dfg.insts[inst], InstructionData::PhiNode(_)) {
                let bb = self.func.layout.inst_block(inst).unwrap();
                if self.live_predecessors.insert(bb) {
                    for pred in self.cfg.pred_iter(bb) {
                        if self.live_control_flow.insert(pred) {
                            self.bb_work_list.push(pred)
                        }
                    }
                }
            }

            true
        } else {
            false
        }
    }

    fn mark_bb_live(&mut self, bb: Block) {
        if self.live_blocks.insert(bb) {
            if self.live_control_flow.insert(bb) {
                self.bb_work_list.push(bb);
            }

            if let Some(term) = self.func.layout.last_inst(bb) {
                if matches!(self.func.dfg.insts[term], InstructionData::Jump { .. }) {
                    self.live_insts.insert(term);
                    // no need to insert into the work list etc. for trivial jumps
                }
            }
        }
    }

    fn mark_term_live(&mut self, bb: Block) {
        if let Some(term) = self.func.layout.last_inst(bb) {
            if let InstructionData::Branch { then_dst, else_dst, .. } = self.func.dfg.insts[term] {
                if self.mark_inst_live(term) {
                    self.mark_bb_live(then_dst);
                    self.mark_bb_live(else_dst);
                }
            }
        }
    }
}
