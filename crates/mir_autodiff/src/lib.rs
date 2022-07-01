mod builder;
mod live_derivatives;
mod postorder;
mod subgraph;
mod unkowns;

use ahash::AHashMap;
pub use builder::build_derivatives;
pub use live_derivatives::LiveDerivatives;
use mir::{DataFlowGraph, DerivativeInfo, DominatorTree, Function, Inst, Opcode, Value};

use crate::unkowns::{FirstOrderUnkown, Unkown, Unkowns};

pub fn auto_diff(
    func: &mut Function,
    dom_tree: &DominatorTree,
    unkowns: &DerivativeInfo,
    extra_derivatives: &[(Value, mir::Unkown)],
) -> AHashMap<(Value, mir::Unkown), Value> {
    let mut unkowns = Unkowns::new(unkowns);
    let live_derivative = LiveDerivatives::build(func, &mut unkowns, extra_derivatives, dom_tree);
    // for inst in live_derivative.derivatives.rows() {
    //     let unkowns_ = live_derivative.derivatives.row(inst).unwrap();
    //     if unkowns_.is_empty() {
    //         continue;
    //     }
    //     println!("{}:", func.dfg.display_inst(inst));
    //     for unkown in unkowns_.iter() {
    //         println!("\t{:?}", unkown)
    //     }
    // }
    build_derivatives(func, &mut unkowns, &live_derivative, dom_tree.cfg_postorder())
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
    pub unkown: Unkown,
    pub out_derivative_unkown: FirstOrderUnkown,
    pub outer_val: Value,
}
