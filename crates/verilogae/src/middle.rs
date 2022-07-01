use ahash::AHashSet;
use bitset::{BitSet, SparseBitMatrix};
use hir_lower::{CallBackKind, HirInterner, MirBuilder, ParamKind, PlaceKind};
use lasso::Rodeo;
use mir::{ControlFlowGraph, DominatorTree, Function, ValueDef};
use mir_autodiff::auto_diff;
use mir_opt::{
    agressive_dead_code_elimination, dead_code_elimination, inst_combine, simplify_cfg,
    sparse_conditional_constant_propagation,
};

use crate::compiler_db::{CompilationDB, FuncSpec, ModelInfo};

impl FuncSpec {
    pub fn slice_mir(
        &self,
        func: &Function,
        cfg: &ControlFlowGraph,
        intern: &HirInterner,
    ) -> (Function, ControlFlowGraph) {
        let ret_val = intern.outputs[&PlaceKind::Var(self.var)].unwrap();
        let mut func = func.clone();
        let mut cfg = cfg.clone();

        let depbreak_vars: Vec<_> = self
            .dependency_breaking
            .iter()
            .map(|var| intern.params.raw[&ParamKind::HiddenState(*var)])
            .collect();

        for (val, var) in &intern.tagged_reads {
            let new_val = if let Some(i) = self.dependency_breaking.iter().position(|it| it == var)
            {
                depbreak_vars[i]
            } else if let ValueDef::Result(inst, _) = func.dfg.value_def(*val) {
                func.dfg.instr_args(inst)[0]
            } else {
                continue;
            };

            func.dfg.replace_uses(*val, new_val)
        }

        simplify_cfg(&mut func, &mut cfg);

        let mut dom_tree = DominatorTree::default();
        dom_tree.compute(&func, &cfg, false, true, false);
        let mut control_dep = SparseBitMatrix::new(0, 0);
        dom_tree.compute_postdom_frontiers(&cfg, &mut control_dep);

        agressive_dead_code_elimination(
            &mut func,
            &mut cfg,
            &|val, _| val == ret_val,
            &control_dep,
        );
        simplify_cfg(&mut func, &mut cfg);

        (func, cfg)
    }
}

impl CompilationDB {
    pub fn build_module_mir(
        &self,
        info: &ModelInfo,
    ) -> (Function, HirInterner, Rodeo, ControlFlowGraph) {
        let dep_break: AHashSet<_> = info
            .functions
            .iter()
            .flat_map(|func| func.dependency_breaking.iter().copied())
            .collect();

        let outputs: AHashSet<_> = info.functions.iter().map(|func| func.var).collect();
        let mut literals = Rodeo::new();
        let (mut func, mut intern) = MirBuilder::new(
            self,
            info.module.into(),
            &|kind| {
                matches!(
                    kind,
                    PlaceKind::Var(var) if outputs.contains(&var)
                )
            },
            &mut outputs.iter().copied(),
        )
        .with_tagged_reads(dep_break)
        .build(&mut literals);

        // remove unused sideeffects
        for (id, kind) in intern.callbacks.iter_enumerated() {
            if let CallBackKind::CollapseHint(_, _) = kind {
                func.dfg.signatures[id].has_sideeffects = false;
            }
        }

        intern.insert_var_init(self, &mut func, &mut literals);

        let mut cfg = ControlFlowGraph::new();
        cfg.compute(&func);

        simplify_cfg(&mut func, &mut cfg);

        for (param, (kind, _)) in intern.params.iter_enumerated() {
            if matches!(kind, ParamKind::Voltage { .. } | ParamKind::Current(_)) {
                let changed = intern.callbacks.ensure(CallBackKind::Derivative(param)).1;
                if changed {
                    let signature = CallBackKind::Derivative(param).signature();
                    func.import_function(signature);
                }
            }
        }

        let mut output_values = BitSet::new_empty(func.dfg.num_values());
        output_values.extend(intern.outputs.values().filter_map(|it| it.expand()));

        dead_code_elimination(&mut func, &output_values);

        let mut dom_tree = DominatorTree::default();
        dom_tree.compute(&func, &cfg, true, false, true);
        let unkowns = intern.unkowns(&mut func, false);
        auto_diff(&mut func, &dom_tree, &unkowns, &[]);

        sparse_conditional_constant_propagation(&mut func, &cfg);
        inst_combine(&mut func);
        simplify_cfg(&mut func, &mut cfg);

        (func, intern, literals, cfg)
        // (func, intern, literals, Vec::new(), cfg)
    }

    pub fn build_param_init_mir(
        &self,
        info: &ModelInfo,
        literals: &mut Rodeo,
    ) -> (Function, HirInterner) {
        let mut func = Function::default();
        let mut intern = HirInterner::default();

        let params: Vec<_> = info.params.keys().copied().collect();
        intern.insert_param_init(self, &mut func, literals, true, false, &params);

        (func, intern)
    }
}
