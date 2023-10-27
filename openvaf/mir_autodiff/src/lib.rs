mod builder;
mod intern;
mod live_derivatives;
mod postorder;
mod subgraph;

use ahash::AHashMap;
pub use builder::build_derivatives;
pub use live_derivatives::LiveDerivatives;
use mir::{
    DataFlowGraph, DominatorTree, Function, Inst, InstructionData, KnownDerivatives, Opcode, Value,
};

use crate::intern::{Derivative, DerivativeIntern};

pub fn auto_diff(
    mut func: impl AsMut<Function>,
    dom_tree: &DominatorTree,
    derivatives: &KnownDerivatives,
    extra_derivatives: &[(Value, mir::Unknown)],
) -> AHashMap<(Value, mir::Unknown), Value> {
    let func = func.as_mut();
    let mut intern = DerivativeIntern::new(derivatives);
    let live_derivative = LiveDerivatives::build(func, &mut intern, extra_derivatives, dom_tree);
    build_derivatives(func, &mut intern, &live_derivative, dom_tree.cfg_postorder())
}

fn is_zero_call(dfg: &DataFlowGraph, inst: Inst, intern: &DerivativeIntern) -> bool {
    if let InstructionData::Call { func_ref, .. } = dfg.insts[inst] {
        !intern.ddx_calls.contains_key(&func_ref)
    } else {
        false
    }
}
fn zero_derivative(dfg: &DataFlowGraph, inst: Inst) -> bool {
    let opcode = dfg.insts[inst].opcode();
    matches!(
        opcode,
        Opcode::Ineg
            | Opcode::Iadd
            | Opcode::Isub
            | Opcode::Imul
            | Opcode::Idiv
            | Opcode::Ishl
            | Opcode::Ishr
            | Opcode::IFcast
            | Opcode::BIcast
            | Opcode::IBcast
            | Opcode::FBcast
            | Opcode::BFcast
            | Opcode::FIcast
            | Opcode::Irem
            | Opcode::Inot
            | Opcode::Ixor
            | Opcode::Iand
            | Opcode::Ior
            | Opcode::Clog2
            | Opcode::Frem
            | Opcode::Floor
            | Opcode::Ceil
            | Opcode::Bnot
            | Opcode::Ilt
            | Opcode::Igt
            | Opcode::Flt
            | Opcode::Fgt
            | Opcode::Ile
            | Opcode::Ige
            | Opcode::Fle
            | Opcode::Fge
            | Opcode::Ieq
            | Opcode::Feq
            | Opcode::Seq
            | Opcode::Beq
            | Opcode::Ine
            | Opcode::Fne
            | Opcode::Sne
            | Opcode::Bne
            | Opcode::Br
            | Opcode::Jmp
    )
}

#[derive(Debug, Clone)]
struct ChainRule {
    pub inner_derivative: (Value, Derivative),
    pub outer_derivative: Derivative,
    pub dst_derivative: Derivative,
    pub val: Value,
}
