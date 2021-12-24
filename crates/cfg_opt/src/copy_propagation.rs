use ahash::AHashMap;
use cfg::{ControlFlowGraph, InstrDst, Op, Operand, Terminator};

pub fn copy_propagation(cfg: &mut ControlFlowGraph) {
    let mut copys = AHashMap::new();
    for (_, bb) in cfg.reverse_postorder_itermut() {
        for phi in &mut bb.phis {
            for local in phi.sources.values_mut() {
                if let Some(copy) = copys.get(local) {
                    *local = *copy
                }
            }
        }
        for instr in &mut bb.instructions {
            if instr.op == Op::Copy {
                if let Operand::Local(local) = &mut instr.args[0] {
                    if let Some(copy) = copys.get(local) {
                        *local = *copy
                    }
                    if let InstrDst::Local(dst) = instr.dst {
                        copys.insert(dst, *local);
                    }
                }
            } else {
                instr.visit_operands_mut(|op| {
                    if let Operand::Local(local) = op {
                        if let Some(copy) = copys.get(local) {
                            *local = *copy
                        }
                    }
                })
            }
        }

        if let Some(Terminator::Split { condition: Operand::Local(local), .. }) = &mut bb.terminator
        {
            if let Some(copy) = copys.get(local) {
                *local = *copy
            }
        }
    }
}
