use crate::instructions::Opcode;

use super::*;

#[test]
fn make_inst() {
    let mut dfg = DataFlowGraph::new();

    let idata = InstructionData::UnaryInt { imm: 0 };
    let inst = dfg.make_inst(idata);

    dfg.make_inst_results(inst);
    assert_eq!(inst.to_string(), "inst0");
    assert_eq!(dfg.display_inst(inst).to_string(), "v0 = iconst 0");

    // Immutable reference resolution.
    {
        let immdfg = &dfg;
        let ins = &immdfg[inst];
        assert_eq!(ins.opcode(), Opcode::Iconst);
    }

    // Results.
    let val = dfg.first_result(inst);
    assert_eq!(dfg.inst_results(inst), &[val]);

    assert_eq!(dfg.value_def(val), ValueDef::Result(inst, 0));

    // Replacing results.
    assert!(dfg.value_is_attached(val));
    let v2 = dfg.replace_result(val);
    assert!(!dfg.value_is_attached(val));
    assert!(dfg.value_is_attached(v2));
    assert_eq!(dfg.inst_results(inst), &[v2]);
    assert_eq!(dfg.value_def(v2), ValueDef::Result(inst, 0));
}

#[test]
fn block() {
    let mut dfg = DataFlowGraph::new();

    let block = dfg.make_block();
    assert_eq!(block.to_string(), "block0");
    assert_eq!(dfg.num_block_params(block), 0);
    assert_eq!(dfg.block_params(block), &[]);
    assert!(dfg.detach_block_params(block).is_empty());
    assert_eq!(dfg.num_block_params(block), 0);
    assert_eq!(dfg.block_params(block), &[]);

    let arg1 = dfg.append_block_param(block);
    assert_eq!(arg1.to_string(), "v0");
    assert_eq!(dfg.num_block_params(block), 1);
    assert_eq!(dfg.block_params(block), &[arg1]);

    let arg2 = dfg.append_block_param(block);
    assert_eq!(arg2.to_string(), "v1");
    assert_eq!(dfg.num_block_params(block), 2);
    assert_eq!(dfg.block_params(block), &[arg1, arg2]);

    assert_eq!(dfg.value_def(arg1), ValueDef::Param(block, 0));
    assert_eq!(dfg.value_def(arg2), ValueDef::Param(block, 1));

    // Swap the two block parameters.
    let vlist = dfg.detach_block_params(block);
    assert_eq!(dfg.num_block_params(block), 0);
    assert_eq!(dfg.block_params(block), &[]);
    assert_eq!(vlist.as_slice(&dfg.value_lists), &[arg1, arg2]);
    dfg.attach_block_param(block, arg2);
    let arg3 = dfg.append_block_param(block);
    dfg.attach_block_param(block, arg1);
    assert_eq!(dfg.block_params(block), &[arg2, arg3, arg1]);
}

#[test]
fn swap_remove_block_params() {
    let mut dfg = DataFlowGraph::new();

    let block = dfg.make_block();
    let arg1 = dfg.append_block_param(block);
    let arg2 = dfg.append_block_param(block);
    let arg3 = dfg.append_block_param(block);
    assert_eq!(dfg.block_params(block), &[arg1, arg2, arg3]);

    dfg.swap_remove_block_param(arg1);
    assert!(!dfg.value_is_attached(arg1));
    assert!(dfg.value_is_attached(arg2));
    assert!(dfg.value_is_attached(arg3));
    assert_eq!(dfg.block_params(block), &[arg3, arg2]);
    dfg.swap_remove_block_param(arg2);
    assert!(!dfg.value_is_attached(arg2));
    assert!(dfg.value_is_attached(arg3));
    assert_eq!(dfg.block_params(block), &[arg3]);
    dfg.swap_remove_block_param(arg3);
    assert!(!dfg.value_is_attached(arg3));
    assert_eq!(dfg.block_params(block), &[]);
}
