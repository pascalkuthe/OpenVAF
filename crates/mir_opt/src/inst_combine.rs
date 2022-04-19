use mir::{Function, Inst, Value};

use crate::simplify::SimplifyCtx;

pub fn inst_combine(func: &mut Function) {
    let mut work_list = Vec::new();
    let mut ctx = SimplifyCtx::<f64, _>::new(func, |val, _| val);

    let mut block_cursor = ctx.func.layout.blocks_cursor();
    while let Some(block) = block_cursor.next(&ctx.func.layout) {
        let mut inst_cursor = ctx.func.layout.block_inst_cursor(block);
        while let Some(inst) = inst_cursor.next(&ctx.func.layout) {
            if let Some(val) = ctx.simplify_inst(inst) {
                replace_uses(ctx.func, &mut work_list, inst, val)
            }
        }
    }

    while let Some(inst) = work_list.pop() {
        if ctx.func.layout.inst_block(inst).is_some() {
            if let Some(val) = ctx.simplify_inst(inst) {
                replace_uses(ctx.func, &mut work_list, inst, val)
            }
        }
    }
}

fn replace_uses(func: &mut Function, workque: &mut Vec<Inst>, inst: Inst, replace: Value) {
    let old = func.dfg.first_result(inst);
    for use_ in func.dfg.uses(old) {
        let use_ = func.dfg.use_to_operand(use_).0;
        workque.push(use_)
    }

    func.dfg.replace_uses(old, replace);
    func.dfg.zap_inst(inst);
    func.layout.remove_inst(inst);
}
