use bitset::{BitSet, SparseBitMatrix};
use hir::CompilationDB;
use hir_lower::{HirInterner, MirBuilder, PlaceKind};
use lasso::Rodeo;
use mir::{Block, ControlFlowGraph, DominatorTree, Function, Inst, Value};
use mir_opt::{
    aggressive_dead_code_elimination, dead_code_elimination, inst_combine, propagate_direct_taint,
    propagate_taint, simplify_cfg, simplify_cfg_no_phi_merge,
    sparse_conditional_constant_propagation, GVN,
};
use stdx::packed_option::PackedOption;

use crate::ModuleInfo;

pub(crate) struct Context<'a> {
    pub(crate) func: Function,
    pub(crate) cfg: ControlFlowGraph,
    pub(crate) dom_tree: DominatorTree,
    pub(crate) intern: HirInterner,
    pub(crate) db: &'a CompilationDB,
    pub(crate) module: &'a ModuleInfo,
    pub(crate) output_values: BitSet<Value>,
    pub(crate) op_dependent_insts: BitSet<Inst>,
    pub(crate) op_dependent_vals: Vec<Value>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum OptimiziationStage {
    Inital,
    PostDerivative,
    Final,
}

impl<'a> Context<'a> {
    pub fn new(db: &'a CompilationDB, literals: &mut Rodeo, module: &'a ModuleInfo) -> Self {
        let (mut func, mut intern) = MirBuilder::new(
            db,
            module.module,
            &|kind| match kind {
                PlaceKind::Contribute { .. }
                | PlaceKind::ImplicitResidual { .. }
                | PlaceKind::CollapseImplicitEquation(_)
                | PlaceKind::IsVoltageSrc(_) => true,
                PlaceKind::Var(var) => module.op_vars.contains_key(&var),
                _ => false,
            },
            &mut module.op_vars.keys().copied(),
        )
        .with_equations()
        .with_tagged_writes()
        .build(literals);
        // TODO hidden state
        intern.insert_var_init(db, &mut func, literals);

        Context {
            output_values: BitSet::new_empty(func.dfg.num_values()),
            func,
            cfg: ControlFlowGraph::new(),
            dom_tree: DominatorTree::default(),
            intern,
            db,
            module,
            op_dependent_insts: BitSet::new_empty(0),
            op_dependent_vals: Vec::new(),
        }
    }

    pub fn optimize(&mut self, stage: OptimiziationStage) -> GVN {
        if stage == OptimiziationStage::Inital {
            dead_code_elimination(&mut self.func, &self.output_values);
        }
        sparse_conditional_constant_propagation(&mut self.func, &self.cfg);
        inst_combine(&mut self.func);
        if stage == OptimiziationStage::Final {
            simplify_cfg(&mut self.func, &mut self.cfg);
        } else {
            simplify_cfg_no_phi_merge(&mut self.func, &mut self.cfg);
        }
        self.compute_domtree(true, true, false);

        let mut gvn = GVN::default();
        gvn.init(&self.func, &self.dom_tree, self.intern.params.len() as u32);
        gvn.solve(&mut self.func);
        gvn.remove_unnecessary_insts(&mut self.func, &self.dom_tree);

        if stage == OptimiziationStage::Final {
            let mut control_dep = SparseBitMatrix::new_square(0);
            self.dom_tree.compute_postdom_frontiers(&self.cfg, &mut control_dep);
            aggressive_dead_code_elimination(
                &mut self.func,
                &mut self.cfg,
                &|val, _| self.output_values.contains(val),
                &control_dep,
            );
            simplify_cfg(&mut self.func, &mut self.cfg);
        }

        gvn
    }

    pub fn compute_cfg(&mut self) {
        self.cfg.compute(&self.func);
    }

    pub fn compute_domtree(&mut self, dom: bool, pdom: bool, postorder: bool) {
        self.dom_tree.compute(&self.func, &self.cfg, dom, pdom, postorder);
    }

    pub fn compute_outputs(&mut self, contributes: bool) {
        self.output_values.clear();
        self.output_values.ensure(self.func.dfg.num_values() + 1);
        if contributes {
            self.output_values
                .extend(self.intern.outputs.values().copied().filter_map(PackedOption::expand));
        } else {
            for (kind, val) in self.intern.outputs.iter() {
                if matches!(kind, PlaceKind::Var(var) if self.module.op_vars.contains_key(var))
                    || matches!(kind, PlaceKind::CollapseImplicitEquation(_) | PlaceKind::BoundStep)
                {
                    self.output_values.insert(val.unwrap_unchecked());
                }
            }
        }
    }

    pub fn init_op_dependent_insts(&mut self, dom_frontiers: &mut SparseBitMatrix<Block, Block>) {
        self.dom_tree.compute_dom_frontiers(&self.cfg, dom_frontiers);
        let dfg = &mut self.func.dfg;
        self.op_dependent_insts.ensure(dfg.num_insts());

        for (cb, uses) in self.intern.callback_uses.iter_mut_enumerated() {
            if self.intern.callbacks[cb].is_noise() {
                uses.retain(|&inst| {
                    if self.func.layout.inst_block(inst).is_none() {
                        return false;
                    }
                    self.op_dependent_insts.insert(inst);
                    for &result in dfg.inst_results(inst) {
                        self.op_dependent_vals.push(result);
                    }
                    true
                })
            }
        }
        for (param, &val) in self.intern.params.iter() {
            if !dfg.value_dead(val) && param.op_dependent() {
                self.op_dependent_vals.push(val)
            }
        }
        propagate_direct_taint(
            &self.func,
            dom_frontiers,
            self.op_dependent_vals.iter().copied(),
            &mut self.op_dependent_insts,
        )
    }

    pub fn refresh_op_dependent_insts(&mut self) {
        let dfg = &mut self.func.dfg;
        self.op_dependent_vals.clear();
        self.op_dependent_insts.clear();
        self.op_dependent_insts.ensure(dfg.num_insts());
        for (cb, uses) in self.intern.callback_uses.iter_mut_enumerated() {
            if self.intern.callbacks[cb].op_dependent() {
                uses.retain(|&inst| {
                    if self.func.layout.inst_block(inst).is_none() {
                        return false;
                    }
                    self.op_dependent_insts.insert(inst);
                    for &result in dfg.inst_results(inst) {
                        self.op_dependent_vals.push(result);
                    }
                    true
                })
            }
        }
        for (param, &val) in self.intern.params.iter() {
            if !dfg.value_dead(val) && param.op_dependent() {
                self.op_dependent_vals.push(val)
            }
        }
        propagate_taint(
            &self.func,
            &self.dom_tree,
            &self.cfg,
            self.op_dependent_vals.iter().copied(),
            &mut self.op_dependent_insts,
        )
    }
}
