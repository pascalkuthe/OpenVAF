use cfg::{Const, ControlFlowGraph, Operand, Terminator};

pub fn simplify_branches(cfg: &mut ControlFlowGraph) {
    for bb in cfg.blocks.iter_mut() {
        let terminator = bb.terminator_mut();
        match terminator {
            Terminator::Split {
                condition: Operand::Const(Const::Bool(true)),
                true_block: dst,
                ..
            }
            | Terminator::Split {
                condition: Operand::Const(Const::Bool(false)),
                false_block: dst,
                ..
            } => {
                *terminator = Terminator::Goto(*dst)
                // TODO error for infinite loop
            }

            _ => (),
        }
    }
    cfg.predecessor_cache.invalidate();
    cfg.is_cyclic.invalidate();
}
