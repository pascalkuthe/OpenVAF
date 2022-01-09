use bitset::SparseBitMatrix;
use cfg::{BasicBlock, ControlFlowGraph, Terminator};

use crate::post_dominators::PostDominators;

pub type ControlDependenceGraph = SparseBitMatrix<BasicBlock, BasicBlock>;

/// Calculates the Control Dependence Graph of a CFG.
/// A basic block is control dependent on a statement if this statements decides whether the block is executed (for example an if statement).
/// Statements that cause control flow are not represented as statements but as basic block terminators in the cfg.
/// As such the control dependence Graph simply maps basic blocks to all basics block whose terminator affects whether a statements is executed
///
///# Note
/// This analysis pass allows to manually pass the `PostDominators` so that the post dominators can be reused
/// If you dont want to reuse `ipdom` you should can simply pass `BuildPostDominators`
pub fn control_dependence(
    ipdom: &PostDominators,
    cfg: &ControlFlowGraph,
) -> ControlDependenceGraph {
    let mut cdg = SparseBitMatrix::new(cfg.blocks.len(), cfg.blocks.len());

    for (id, bb) in cfg.blocks.iter_enumerated() {
        // we only care about control dependencies on branches since end and goto are unconditional jumps
        if let Terminator::Split { true_block, false_block, loop_head, .. } = bb.terminator() {
            propagate_control_dependence(&mut cdg, ipdom, *true_block, id);
            // There is no control flow control in Verilog-A and loops must always terminate.
            // Tt is therefore fair to assume that the blocks after the loop are not control dependent on the loop condition
            if !loop_head {
                propagate_control_dependence(&mut cdg, ipdom, *false_block, id);
            }
        }
    }

    cdg
}

fn propagate_control_dependence(
    dst: &mut ControlDependenceGraph,
    ipdom: &PostDominators,
    mut from: BasicBlock,
    to: BasicBlock,
) {
    loop {
        if from == ipdom[to] {
            break;
        }
        dst.insert(from, to);

        if from == ipdom[from] {
            unreachable!("unclosed condition or circular post dominance (hoping to reach: {:?} ({:?}), but stuck at : {:?})", ipdom[to], to, from)
        }
        from = ipdom[from];
    }
}
