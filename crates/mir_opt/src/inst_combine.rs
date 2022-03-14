use mir::builder::InstBuilder;
use mir::{
    Function, Inst, InstructionData, Opcode, Value, ValueDef, F_N_ONE, F_ONE, F_ZERO, N_ONE, ONE,
    ZERO,
};

pub fn inst_combine(func: &mut Function) {
    let mut work_list = Vec::new();

    let mut block_cursor = func.layout.blocks_cursor();
    while let Some(block) = block_cursor.next(&func.layout) {
        let mut inst_cursor = func.layout.block_inst_cursor(block);
        while let Some(inst) = inst_cursor.next(&func.layout) {
            if process::<false>(&mut work_list, inst, func) {
                revisit(&mut work_list, inst, func)
            }
        }
    }

    while let Some(inst) = work_list.pop() {
        if func.layout.inst_block(inst).is_some() && process::<true>(&mut work_list, inst, func) {
            revisit(&mut work_list, inst, func)
        }
    }
}

fn process<const REOPT: bool>(workque: &mut Vec<Inst>, inst: Inst, func: &mut Function) -> bool {
    let replace = match func.dfg.insts[inst] {
        InstructionData::Unary { opcode, arg } => {
            let inverse = match opcode {
                Opcode::Inot => Opcode::Inot,
                Opcode::Bnot => Opcode::Bnot,
                Opcode::Fneg => Opcode::Fneg,
                Opcode::Ineg => Opcode::Ineg,
                Opcode::IFcast => Opcode::FIcast,
                Opcode::BIcast => Opcode::IBcast,
                Opcode::BFcast => Opcode::FBcast,
                Opcode::Exp => Opcode::Ln,
                Opcode::Ln => Opcode::Exp,
                Opcode::Sin => Opcode::Asin,
                Opcode::Cos => Opcode::Acos,
                Opcode::Tan => Opcode::Atan,
                Opcode::Asin => Opcode::Sin,
                Opcode::Acos => Opcode::Cos,
                Opcode::Atan => Opcode::Tan,
                Opcode::Sinh => Opcode::Asinh,
                Opcode::Cosh => Opcode::Acosh,
                Opcode::Tanh => Opcode::Atanh,
                Opcode::Asinh => Opcode::Sinh,
                Opcode::Acosh => Opcode::Cosh,
                Opcode::Atanh => Opcode::Tanh,
                _ => return false,
            };

            if let Some(val) = as_unary(func, arg, inverse) {
                val
            } else {
                return false;
            }
        }

        InstructionData::Binary { opcode, args } => match opcode {
            Opcode::Isub if REOPT && args[0] == args[1] => ZERO,
            Opcode::Isub | Opcode::Iadd if args[1] == ZERO => args[0],

            Opcode::Isub if args[0] == ZERO => {
                func.dfg.replace(inst).ineg(args[1]);
                return true;
            }

            Opcode::Iadd if args[0] == ZERO => args[1],

            Opcode::Fsub if REOPT && args[0] == args[1] => F_ZERO,
            Opcode::Fsub | Opcode::Fadd if args[1] == F_ZERO => args[0],

            Opcode::Fsub if args[0] == F_ZERO => {
                func.dfg.replace(inst).fneg(args[1]);
                return true;
            }

            Opcode::Fadd if args[0] == F_ZERO => args[1],

            Opcode::Imul | Opcode::Idiv if args[1] == ONE => args[0],
            Opcode::Imul if args[0] == ONE => args[1],

            Opcode::Imul if args[0] == N_ONE => {
                func.dfg.replace(inst).ineg(args[1]);
                return true;
            }

            Opcode::Imul | Opcode::Idiv if args[1] == N_ONE => {
                func.dfg.replace(inst).ineg(args[0]);
                return true;
            }

            Opcode::Fmul | Opcode::Fdiv if args[1] == F_ONE => args[0],

            Opcode::Fmul if args[0] == F_N_ONE => {
                func.dfg.replace(inst).fneg(args[1]);
                return true;
            }

            Opcode::Fmul | Opcode::Fdiv if args[1] == F_N_ONE => {
                func.dfg.replace(inst).fneg(args[0]);
                return true;
            }

            Opcode::Fmul => {
                if let Some(val) = try_simplify_inverse_fmul(func, args[0], args[1])
                    .or_else(|| try_simplify_inverse_fmul(func, args[1], args[0]))
                {
                    val
                } else {
                    return false;
                }
            }

            Opcode::Imul => {
                if let Some(val) = try_simplify_inverse_imul(func, args[0], args[1])
                    .or_else(|| try_simplify_inverse_imul(func, args[1], args[0]))
                {
                    val
                } else {
                    return false;
                }
            }

            Opcode::Fdiv => {
                if let Some(arg) = try_simplify_inverse_fdiv_fmul(func, args[0], args[1])
                    .or_else(|| try_simplify_inverse_fdiv_fdiv(func, args[1], args[0]))
                {
                    arg
                } else if let Some(arg) = try_simplify_inverse_fdiv_fmul(func, args[1], args[0])
                    .or_else(|| try_simplify_inverse_fdiv_fdiv(func, args[0], args[1]))
                {
                    func.dfg.replace(inst).fdiv(F_ONE, arg);
                    return true;
                } else {
                    return false;
                }
            }

            Opcode::Idiv => {
                if let Some(arg) = try_simplify_inverse_idiv_imul(func, args[0], args[1])
                    .or_else(|| try_simplify_inverse_idiv_idiv(func, args[1], args[0]))
                {
                    arg
                } else if let Some(arg) = try_simplify_inverse_idiv_imul(func, args[1], args[0])
                    .or_else(|| try_simplify_inverse_idiv_idiv(func, args[0], args[1]))
                {
                    func.dfg.replace(inst).idiv(ONE, arg);
                    return true;
                } else {
                    return false;
                }
            }

            Opcode::Fadd => {
                if let Some(arg) = try_simplify_fadd_fsub(func, args[0], args[1])
                    .or_else(|| try_simplify_fadd_fsub(func, args[0], args[1]))
                {
                    arg
                } else if let Some(arg) = as_unary(func, args[1], Opcode::Fneg) {
                    func.dfg.replace(inst).fsub(args[0], arg);
                    return as_unary(func, args[0], Opcode::Fneg).is_none();
                } else if let Some(arg) = as_unary(func, args[0], Opcode::Fneg) {
                    func.dfg.replace(inst).fsub(args[1], arg);
                    return false;
                } else {
                    return false;
                }
            }

            Opcode::Iadd => {
                if let Some(arg) = try_simplify_iadd_isub(func, args[0], args[1])
                    .or_else(|| try_simplify_iadd_isub(func, args[0], args[1]))
                {
                    arg
                } else if let Some(arg) = as_unary(func, args[1], Opcode::Ineg) {
                    func.dfg.replace(inst).isub(args[0], arg);
                    return as_unary(func, args[0], Opcode::Ineg).is_none();
                } else if let Some(arg) = as_unary(func, args[0], Opcode::Ineg) {
                    func.dfg.replace(inst).isub(args[1], arg);
                    return false;
                } else {
                    return false;
                }
            }

            Opcode::Fsub => {
                if let Some(arg) = try_simplify_fsub_fadd(func, args[0], args[1])
                    .or_else(|| try_simplify_fsub_fsub(func, args[1], args[0]))
                {
                    arg
                } else if let Some(arg) = try_simplify_fsub_fadd(func, args[1], args[0])
                    .or_else(|| try_simplify_fsub_fsub(func, args[0], args[1]))
                {
                    func.dfg.replace(inst).fneg(arg);
                    return true;
                } else if let Some(val) = as_unary(func, args[1], Opcode::Fneg) {
                    func.dfg.replace(inst).fadd(args[0], val);
                    return true;
                } else {
                    return false;
                }
            }

            Opcode::Isub => {
                if let Some(arg) = try_simplify_isub_iadd(func, args[0], args[1])
                    .or_else(|| try_simplify_isub_isub(func, args[1], args[0]))
                {
                    arg
                } else if let Some(arg) = try_simplify_isub_iadd(func, args[1], args[0])
                    .or_else(|| try_simplify_isub_isub(func, args[0], args[1]))
                {
                    func.dfg.replace(inst).ineg(arg);
                    return true;
                } else if let Some(val) = as_unary(func, args[1], Opcode::Ineg) {
                    func.dfg.replace(inst).fadd(args[0], val);
                    return true;
                } else {
                    return false;
                }
            }

            _ => return false,
        },

        InstructionData::PhiNode(phi) if REOPT => {
            let mut edges = func.dfg.phi_edges(phi);
            if let Some((_, first)) = edges.next() {
                if edges.all(|(_, val)| val == first) {
                    first
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }

        _ => return false,
    };

    let old = func.dfg.first_result(inst);
    for use_ in func.dfg.uses(old) {
        let use_ = func.dfg.use_to_operand(use_).0;
        workque.push(use_)
    }

    func.dfg.replace_uses(old, replace);
    func.dfg.zap_inst(inst);
    func.layout.remove_inst(inst);
    false
}

fn revisit(workque: &mut Vec<Inst>, inst: Inst, func: &Function) {
    workque.push(inst);
    for use_ in func.dfg.uses(func.dfg.first_result(inst)) {
        let use_ = func.dfg.use_to_operand(use_).0;
        workque.push(use_)
    }
}

// TODO nested
fn try_simplify_inverse_fmul(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst_, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Fdiv, args } = func.dfg.insts[inst_] {
            if args[1] == other {
                return Some(args[0]);
            }
        }
    }

    None
}

fn try_simplify_inverse_imul(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst_, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Idiv, args } = func.dfg.insts[inst_] {
            if args[1] == other {
                return Some(args[0]);
            }
        }
    }

    None
}

fn try_simplify_inverse_fdiv_fmul(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst_, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Fmul, args } = func.dfg.insts[inst_] {
            if args[1] == other {
                return Some(args[0]);
            }

            if args[0] == other {
                return Some(args[1]);
            }
        }
    }

    None
}

fn try_simplify_inverse_fdiv_fdiv(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst_, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Fdiv, args } = func.dfg.insts[inst_] {
            if args[0] == other {
                return Some(args[1]);
            }
        }
    }

    None
}

fn try_simplify_inverse_idiv_imul(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst_, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Imul, args } = func.dfg.insts[inst_] {
            if args[1] == other {
                return Some(args[0]);
            }

            if args[0] == other {
                return Some(args[1]);
            }
        }
    }

    None
}

fn try_simplify_inverse_idiv_idiv(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst_, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Idiv, args } = func.dfg.insts[inst_] {
            if args[0] == other {
                return Some(args[1]);
            }
        }
    }

    None
}

fn as_unary(func: &Function, val: Value, op: Opcode) -> Option<Value> {
    if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
        if let InstructionData::Unary { opcode, arg } = func.dfg.insts[inst] {
            if opcode != op {
                return None;
            }
            return Some(arg);
        }
    }
    None
}

fn try_simplify_fadd_fsub(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Fsub, args } = func.dfg.insts[inst] {
            if args[1] == other {
                return Some(args[0]);
            }
        }
    }
    None
}

fn try_simplify_iadd_isub(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Isub, args } = func.dfg.insts[inst] {
            if args[1] == other {
                return Some(args[0]);
            }
        }
    }
    None
}

fn try_simplify_fsub_fadd(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Fadd, args } = func.dfg.insts[inst] {
            if args[1] == other {
                return Some(args[0]);
            }

            if args[0] == other {
                return Some(args[1]);
            }
        }
    }
    None
}

fn try_simplify_fsub_fsub(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Fsub, args } = func.dfg.insts[inst] {
            if args[0] == other {
                return Some(args[1]);
            }
        }
    }
    None
}

fn try_simplify_isub_iadd(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Iadd, args } = func.dfg.insts[inst] {
            if args[1] == other {
                return Some(args[0]);
            }

            if args[0] == other {
                return Some(args[1]);
            }
        }
    }
    None
}

fn try_simplify_isub_isub(func: &Function, val: Value, other: Value) -> Option<Value> {
    if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
        if let InstructionData::Binary { opcode: Opcode::Isub, args } = func.dfg.insts[inst] {
            if args[0] == other {
                return Some(args[1]);
            }
        }
    }
    None
}
