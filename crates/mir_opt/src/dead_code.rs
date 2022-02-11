use std::collections::VecDeque;

use bitset::BitSet;
use mir::{Function, Inst, Value, ValueDef};
use workqueue::WorkQueue;

pub fn dead_code_elimination(func: &mut Function, output_values: &BitSet<Value>) {
    let mut work_list =
        WorkQueue { deque: VecDeque::new(), set: BitSet::new_filled(func.dfg.num_insts()) };

    let mut block_cursor = func.layout.rev_blocks_cursor();
    while let Some(block) = block_cursor.next(&func.layout) {
        let mut inst_cursor = func.layout.block_inst_cursor(block);
        while let Some(inst) = inst_cursor.next_back(&func.layout) {
            process(&mut work_list, inst, func, output_values);
        }
    }

    while let Some(inst) = work_list.take() {
        process(&mut work_list, inst, func, output_values);
    }
}

fn process(
    workque: &mut WorkQueue<Inst>,
    inst: Inst,
    func: &mut Function,
    output_values: &BitSet<Value>,
) {
    if func.dfg.inst_dead(inst, true)
        && !func.dfg.inst_results(inst).iter().any(|res| output_values.contains(*res))
    {
        func.dfg.zap_inst(inst);
        // arguments might be dead now :)
        for arg in func.dfg.instr_args(inst) {
            if let ValueDef::Result(inst, _) = func.dfg.value_def(*arg) {
                workque.insert(inst);
            }
        }
        func.layout.remove_inst(inst)
    } else {
        // instruction is still live and might be visited again
        workque.set.remove(inst);
    }
}
