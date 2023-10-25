//! various utilities used in this crate

use bitset::BitSet;
use hir_lower::HirInterner;
use mir::{Function, Inst, InstructionData, Opcode, Value, ValueDef};

pub fn strip_optbarrier(func: impl AsRef<Function>, mut val: Value) -> Value {
    let func = func.as_ref();
    while let Some(inst) = func.dfg.value_def(val).inst() {
        if let InstructionData::Unary { opcode: Opcode::OptBarrier, arg } = func.dfg.insts[inst] {
            val = arg;
        } else {
            break;
        }
    }
    val
}

pub fn is_op_dependent(
    func: impl AsRef<Function>,
    val: Value,
    op_dependent_insts: &BitSet<Inst>,
    intern: &HirInterner,
) -> bool {
    match func.as_ref().dfg.value_def(val) {
        ValueDef::Result(inst, _) => op_dependent_insts.contains(inst),
        ValueDef::Param(param) => intern.params.get_index(param).unwrap().0.op_dependent(),
        ValueDef::Const(_) | ValueDef::Invalid => false,
    }
}
