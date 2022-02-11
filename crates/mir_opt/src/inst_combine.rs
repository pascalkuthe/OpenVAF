use mir::builder::InstBuilder;
use mir::{
    Function, Inst, InstructionData, Opcode, ValueDef, F_N_ONE, F_ONE, F_ZERO, N_ONE, ONE, ZERO,
};

pub fn inst_combine(func: &mut Function) {
    let mut work_list = Vec::new();

    let mut block_cursor = func.layout.blocks_cursor();
    while let Some(block) = block_cursor.next(&func.layout) {
        let mut inst_cursor = func.layout.block_inst_cursor(block);
        while let Some(inst) = inst_cursor.next(&func.layout) {
            process::<false>(&mut work_list, inst, func);
        }
    }

    while let Some(inst) = work_list.pop() {
        if func.layout.inst_block(inst).is_some() {
            process::<true>(&mut work_list, inst, func);
        }
    }
}

fn process<const REOPT: bool>(workque: &mut Vec<Inst>, inst: Inst, func: &mut Function) {
    let replace = match func.dfg.insts[inst] {
        InstructionData::Unary { opcode, arg } => match opcode {
            Opcode::Fneg => {
                if let ValueDef::Result(inst, _) = func.dfg.value_def(arg) {
                    if let InstructionData::Unary { opcode: Opcode::Fneg, arg } =
                        func.dfg.insts[inst]
                    {
                        arg
                    } else {
                        return;
                    }
                } else {
                    return;
                }
            }

            Opcode::Ineg => {
                if let ValueDef::Result(inst, _) = func.dfg.value_def(arg) {
                    if let InstructionData::Unary { opcode: Opcode::Ineg, arg } =
                        func.dfg.insts[inst]
                    {
                        arg
                    } else {
                        return;
                    }
                } else {
                    return;
                }
            }

            _ => return,
        },
        InstructionData::Binary { opcode, args } => match opcode {
            Opcode::Isub if REOPT && args[0] == args[1] => ZERO,
            Opcode::Isub | Opcode::Iadd if args[1] == ZERO => args[0],

            Opcode::Isub if args[0] == ZERO => {
                func.dfg.replace(inst).ineg(args[1]);
                return;
            }

            Opcode::Iadd if args[0] == ZERO => args[1],

            Opcode::Fsub if REOPT && args[0] == args[1] => F_ZERO,
            Opcode::Fsub | Opcode::Fadd if args[1] == F_ZERO => args[0],

            Opcode::Fsub if args[0] == F_ZERO => {
                func.dfg.replace(inst).fneg(args[1]);
                workque.push(inst);
                return;
            }

            Opcode::Fadd if args[0] == F_ZERO => args[1],

            Opcode::Imul | Opcode::Idiv if args[1] == ONE => args[0],
            Opcode::Imul if args[0] == ONE => args[1],

            Opcode::Imul if args[0] == N_ONE => {
                func.dfg.replace(inst).ineg(args[1]);
                workque.push(inst);
                return;
            }

            Opcode::Imul | Opcode::Fdiv if args[1] == N_ONE => {
                func.dfg.replace(inst).ineg(args[0]);
                workque.push(inst);
                return;
            }

            Opcode::Fmul | Opcode::Fdiv if args[1] == F_ONE => args[0],

            Opcode::Fmul if args[0] == F_N_ONE => {
                func.dfg.replace(inst).fneg(args[1]);
                workque.push(inst);
                return;
            }

            Opcode::Fmul | Opcode::Fdiv if args[1] == F_N_ONE => {
                func.dfg.replace(inst).fneg(args[0]);
                workque.push(inst);
                return;
            }

            Opcode::Fsub => {
                if let ValueDef::Result(inst_, _) = func.dfg.value_def(args[1]) {
                    if let InstructionData::Unary { opcode: Opcode::Fneg, arg } =
                        func.dfg.insts[inst_]
                    {
                        workque.push(inst);
                        func.dfg.replace(inst).fadd(args[0], arg);
                    }
                }

                return;
            }

            Opcode::Fadd => {
                if let ValueDef::Result(inst_, _) = func.dfg.value_def(args[1]) {
                    if let InstructionData::Unary { opcode: Opcode::Fneg, arg } =
                        func.dfg.insts[inst_]
                    {
                        func.dfg.replace(inst).fsub(args[0], arg);
                        return;
                    }
                }

                if let ValueDef::Result(inst_, _) = func.dfg.value_def(args[0]) {
                    if let InstructionData::Unary { opcode: Opcode::Fneg, arg } =
                        func.dfg.insts[inst_]
                    {
                        func.dfg.replace(inst).fsub(args[1], arg);
                        return;
                    }
                }

                return;
            }

            Opcode::Isub => {
                if let ValueDef::Result(inst_, _) = func.dfg.value_def(args[1]) {
                    if let InstructionData::Unary { opcode: Opcode::Ineg, arg } =
                        func.dfg.insts[inst_]
                    {
                        workque.push(inst);
                        func.dfg.replace(inst).iadd(args[0], arg);
                    }
                }

                return;
            }

            Opcode::Iadd => {
                if let ValueDef::Result(inst_, _) = func.dfg.value_def(args[1]) {
                    if let InstructionData::Unary { opcode: Opcode::Fneg, arg } =
                        func.dfg.insts[inst_]
                    {
                        func.dfg.replace(inst).isub(args[0], arg);
                        return;
                    }
                }

                if let ValueDef::Result(inst_, _) = func.dfg.value_def(args[0]) {
                    if let InstructionData::Unary { opcode: Opcode::Fneg, arg } =
                        func.dfg.insts[inst_]
                    {
                        func.dfg.replace(inst).isub(args[1], arg);
                        return;
                    }
                }

                return;
            }
            _ => return,
        },
        InstructionData::PhiNode(phi) if REOPT => {
            let mut edges = func.dfg.phi_edges(phi);
            if let Some((_, first)) = edges.next() {
                if edges.all(|(_, val)| val == first) {
                    first
                } else {
                    return;
                }
            } else {
                return;
            }
        }
        _ => return,
    };

    let old = func.dfg.first_result(inst);
    for use_ in func.dfg.uses(old) {
        let use_ = func.dfg.use_to_operand(use_).0;
        workque.push(use_)
    }

    func.dfg.replace_uses(old, replace);
    func.dfg.zap_inst(inst);
    func.layout.remove_inst(inst)
}
