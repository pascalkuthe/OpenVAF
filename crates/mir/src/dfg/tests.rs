use expect_test::expect;

use crate::instructions::{Opcode, PhiMap, PhiNode};
use crate::{Block, F_ZERO};

use super::*;

#[test]
fn make_inst() {
    let mut dfg = DataFlowGraph::new();
    let v3 = dfg.make_param(0u32.into());

    let idata = InstructionData::Binary { opcode: Opcode::Fadd, args: [F_ZERO, v3] };
    let inst = dfg.make_inst(idata);

    dfg.make_inst_results(inst);
    assert_eq!(inst.to_string(), "inst0");
    expect![[r#"
        "v14 = fadd v3, v13"
    "#]]
    .assert_debug_eq(&dfg.display_inst(inst).to_string());

    // Results.
    let v4 = dfg.first_result(inst);
    assert_eq!(dfg.inst_results(inst), &[v4]);

    assert_eq!(dfg.value_def(v4), ValueDef::Result(inst, 0));

    // Replacing results.
    assert!(dfg.value_attached(v4));
    assert_eq!(dfg.uses(v4).count(), 0);

    let idata = InstructionData::Binary { opcode: Opcode::Fadd, args: [v4, v4] };
    let inst = dfg.make_inst(idata);
    dfg.make_inst_results(inst);
    let v5 = dfg.first_result(inst);

    assert_eq!(dfg.value_def(v5), ValueDef::Result(inst, 0));
    assert_eq!(dfg.uses(v5).count(), 0);
    // test that uses are created correctly
    assert_eq!(dfg.uses(v4).count(), 2);

    // linked list is fifo so reverse the iterator.
    // The code does not make any garuntee about the order of the iterator just the contents so if
    // this test ever fails because of order its ok to change this
    assert_eq!(dfg.uses_double_ended(v4).rev().collect::<Vec<_>>(), dfg.operands(inst));
    // test that updating is a noop when nothing has changed
    dfg.zap_inst(inst);
    dfg.update_inst_uses(inst);
    assert_eq!(dfg.uses(v4).count(), 2);

    dfg.replace_uses(v4, F_ZERO);
    assert_eq!(dfg.instr_args(inst), &[F_ZERO, F_ZERO]);
    assert_eq!(dfg.uses(F_ZERO).count(), 3);
    assert!(dfg.value_dead(v4));
    assert_eq!(dfg.uses_double_ended(v4).rev().count(), 0);

    dfg.zap_inst(inst);
    dfg.zap_inst(inst);

    assert_eq!(dfg.uses(v3).count(), 1);
    assert_eq!(dfg.uses(F_ZERO).count(), 1);
    assert!(dfg.value_dead(v4));
    assert_eq!(dfg.uses(v4).count(), 0);

    dfg.instr_args_mut(inst).copy_from_slice(&[v3, v4]);
    dfg.update_inst_uses(inst);

    assert_eq!(dfg.uses(F_ZERO).count(), 1);
    assert_eq!(dfg.uses(v4).count(), 1);
    assert_eq!(dfg.uses(v3).count(), 2);
}

#[test]
fn phi() {
    let mut dfg = DataFlowGraph::new();
    let v3 = dfg.fconst(2f64.into());
    let v4 = dfg.fconst(4f64.into());
    let b0 = Block::from(0u32);
    let b1 = Block::from(1u32);
    let inst = dfg.make_inst(PhiNode { args: ValueList::new(), blocks: PhiMap::new() }.into());
    assert_eq!(
        dfg.insts[inst].unwrap_phi().edges(&dfg.insts.value_lists, &dfg.phi_forest).count(),
        0
    );
    dfg.insert_phi_edge(inst, b0, F_ZERO);
    dfg.insert_phi_edge(inst, b1, v3);
    assert_eq!(dfg.uses(F_ZERO).count(), 1);
    assert_eq!(dfg.uses(v3).count(), 1);
    assert_eq!(
        dfg.insts[inst]
            .unwrap_phi()
            .edges(&dfg.insts.value_lists, &dfg.phi_forest)
            .collect::<Vec<_>>(),
        &[(b0, F_ZERO), (b1, v3)]
    );

    dfg.insert_phi_edge(inst, b1, v4);
    assert_eq!(dfg.uses(F_ZERO).count(), 1);
    assert_eq!(dfg.uses(v4).count(), 1);
    assert_eq!(dfg.uses(v3).count(), 0);
    assert_eq!(
        dfg.insts[inst]
            .unwrap_phi()
            .edges(&dfg.insts.value_lists, &dfg.phi_forest)
            .collect::<Vec<_>>(),
        &[(b0, F_ZERO), (b1, v4)]
    );
    assert_eq!(
        dfg.insts[inst].unwrap_phi().edge_val(b0, &dfg.insts.value_lists, &dfg.phi_forest),
        Some(F_ZERO)
    );
    assert!(dfg.try_remove_phi_edge_at(inst, b0).is_some());

    assert_eq!(dfg.uses(F_ZERO).count(), 0);
    assert_eq!(dfg.uses(v3).count(), 0);
    assert_eq!(dfg.uses(v4).count(), 1);
    assert_eq!(
        dfg.insts[inst]
            .unwrap_phi()
            .edges(&dfg.insts.value_lists, &dfg.phi_forest)
            .collect::<Vec<_>>(),
        &[(b1, v4)]
    );
}
