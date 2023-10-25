//! various utilities used in this crate

use bitset::BitSet;
use hir_lower::HirInterner;
use mir::builder::InstBuilder;
use mir::cursor::{Cursor, FuncCursor};
use mir::{Function, Inst, InstructionData, Opcode, Value, ValueDef, F_ZERO};

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

pub fn strip_optbarrier_if_const(func: impl AsRef<Function>, val: Value) -> Value {
    let func = func.as_ref();
    let stripped = strip_optbarrier(func, val);
    if func.dfg.value_def(stripped).as_const().is_some() {
        stripped
    } else {
        val
    }
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

pub fn update_optbarrier(
    func: &mut Function,
    val: &mut Value,
    update: impl FnOnce(Value, &mut FuncCursor) -> Value,
) {
    if let Some(inst) = func.dfg.value_def(*val).inst() {
        let mut arg = func.dfg.instr_args(inst)[0];
        arg = update(arg, &mut FuncCursor::new(func).at_inst(inst));
        func.dfg.replace(inst).optbarrier(arg);
    } else {
        let mut cursor = FuncCursor::new(&mut *func).at_exit();
        *val = update(*val, &mut cursor);
        *val = cursor.ins().ensure_optbarrier(*val)
    }
}

pub fn add(cursor: &mut FuncCursor, dst: &mut Value, val: Value, negate: bool) {
    match (*dst, val) {
        (_, F_ZERO) => (),
        (F_ZERO, _) if negate => *dst = cursor.ins().fneg(val),
        (F_ZERO, _) => *dst = val,
        (old, _) if negate => *dst = cursor.ins().fsub(old, val),
        (old, _) => *dst = cursor.ins().fadd(old, val),
    }
}
