use crate::{ControlFlowGraph, DominatorTree, Function, InstructionData, ValueDef};

impl Function {
    pub fn validate(&self) -> bool {
        let mut cfg = ControlFlowGraph::new();
        let mut valid = true;
        cfg.compute(self);
        let mut dom_tree = DominatorTree::default();
        dom_tree.compute(self, &cfg, true, false, true);
        for &bb in dom_tree.cfg_postorder() {
            for (seq_num, inst) in self.layout.block_insts(bb).enumerate() {
                let mut valid_inst = true;
                macro_rules! assert {
                    ($predicate: expr, $($msg: expr),*) => {
                        if !$predicate {
                            eprintln!($($msg),*);
                            valid_inst = false
                        }
                    };
                }
                if let InstructionData::PhiNode(phi) = &self.dfg.insts[inst] {
                    for (edge_bb, edge_val) in self.dfg.phi_edges(phi) {
                        assert!(
                            cfg.is_predecessors(edge_bb, bb),
                            "{edge_bb} is not a predecessor of {bb}"
                        );
                        let edge_def = self.dfg.value_def(edge_val);
                        if let Some(arg_def) = edge_def.inst() {
                            if let Some(edge_val_bb) = self.layout.inst_block(arg_def) {
                                assert!(
                                    dom_tree.dominates(edge_bb, edge_val_bb),
                                    "{edge_val} doesn't dominate use ({edge_val_bb} !dom {edge_bb})"
                                );
                            }
                        } else {
                            assert!(edge_def != ValueDef::Invalid, "invalid argument {edge_val}");
                        }
                    }
                } else {
                    for &arg in self.dfg.instr_args(inst) {
                        let arg_def = self.dfg.value_def(arg);
                        if let Some(arg_inst) = arg_def.inst() {
                            if let Some(arg_bb) = self.layout.inst_block(arg_inst) {
                                if arg_bb == bb {
                                    let arg_seq_num = self
                                        .layout
                                        .block_insts(bb)
                                        .position(|inst| inst == arg_inst)
                                        .unwrap();
                                    assert!(arg_seq_num < seq_num, "{arg} doesn't dominate use");
                                } else {
                                    assert!(
                                        dom_tree.dominates(bb, arg_bb),
                                        "{arg} doesn't dominate use ({arg_bb} !dom {bb})"
                                    );
                                }
                            }
                        } else {
                            assert!(arg_def != ValueDef::Invalid, "invalid argument {arg}");
                        }
                    }
                }

                for use_ in self.dfg.inst_uses(inst) {
                    let (use_inst, _) = self.dfg.use_to_operand(use_);
                    let use_val = self.dfg.use_to_value(use_);
                    assert!(
                        self.dfg.inst_results(inst).contains(&use_val),
                        "invalid use {} ({use_val})",
                        self.dfg.display_inst(use_inst)
                    );
                    assert!(
                        self.layout.inst_block(use_inst).is_some(),
                        "removed use {}",
                        self.dfg.display_inst(use_inst)
                    );
                }
                if !valid_inst {
                    eprintln!("{}", self.dfg.display_inst(inst));
                    eprintln!();
                    valid = false
                }
            }
        }
        if !valid {
            println!("{:?}", self);
        }
        valid
    }
}
