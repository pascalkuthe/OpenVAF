use ahash::AHashSet;
use bitset::BitSet;
use hir::{CompilationDB, Node};
use hir_lower::{CallBackKind, HirInterner, MirBuilder, ParamKind, PlaceKind};
use indexmap::IndexSet;
use lasso::Rodeo;
use mir::{Block, ControlFlowGraph, DominatorTree, Function, Inst, InstructionData, Value};
use mir_opt::{
    dead_code_elimination, inst_combine, simplify_cfg_no_phi_merge,
    sparse_conditional_constant_propagation, GVN,
};
use stdx::packed_option::PackedOption;

use crate::prune::prune_unknowns;
use crate::ModuleInfo;

pub(crate) struct Context<'a> {
    pub(crate) func: Function,
    pub(crate) cfg: ControlFlowGraph,
    pub(crate) dom_tree: DominatorTree,
    pub(crate) intern: HirInterner,
    pub(crate) literals: &'a mut Rodeo,
    pub(crate) db: &'a CompilationDB,
    pub(crate) module: &'a ModuleInfo,
    pub(crate) output_values: BitSet<Value>,
    pub(crate) op_dependent_insts: BitSet<Inst>,
}
impl<'a> Context<'a> {
    pub fn new(db: &'a CompilationDB, literals: &'a mut Rodeo, module: &'a ModuleInfo) -> Self {
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
        .with_split_contributions()
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
            literals,
            db,
            module,
            op_dependent_insts: BitSet::new_empty(0),
        }
    }

    pub fn optimize(&mut self) {
        simplify_cfg_no_phi_merge(&mut self.func, &mut self.cfg);
        dead_code_elimination(&mut self.func, &self.output_values);
        sparse_conditional_constant_propagation(&mut self.func, &self.cfg);
        inst_combine(&mut self.func);
        simplify_cfg_no_phi_merge(&mut self.func, &mut self.cfg);
        self.compute_domtree(true, true, false);
        let mut gvn = GVN::default();
        gvn.init(&self.func, &self.dom_tree, self.intern.params.len() as u32);
        gvn.solve(&mut self.func);
        gvn.remove_unnecessary_insts(&mut self.func, &self.dom_tree);
        gvn.clear(&mut self.func);
    }

    pub fn compute_cfg(&mut self) {
        self.cfg.compute(&self.func);
    }

    pub fn compute_domtree(&mut self, dom: bool, pdom: bool, postorder: bool) {
        self.dom_tree.compute(&self.func, &self.cfg, dom, pdom, postorder);
    }

    pub fn compute_outputs(&mut self, residuals: bool) {
        self.output_values.ensure(self.func.dfg.num_values() + 1);
        if residuals {
            self.output_values.clear();
            self.output_values
                .extend(self.intern.outputs.values().copied().filter_map(PackedOption::expand));
        } else {
            for (kind, val) in self.intern.outputs.iter() {
                if matches!(kind, PlaceKind::Var(var) if self.module.op_vars.contains_key(var))
                    || matches!(kind, PlaceKind::CollapseImplicitEquation(_))
                {
                    self.output_values.insert(val.unwrap_unchecked());
                }
            }
        }
    }
    pub fn output_bb(&self) -> Block {
        self.intern
            .outputs
            .values()
            .find_map(|val| val.expand().and_then(|val| self.func.dfg.value_def(val).inst()))
            .map_or(self.func.layout.entry_block(), |inst| self.func.layout.inst_block(inst))
            .unwrap()
    }

    pub fn prune_nodes(&mut self) -> AHashSet<Node> {
        let op_dependent_params: IndexSet<Value, _> = self
            .intern
            .params
            .iter()
            .filter_map(|(param, &val)| {
                (!self.func.dfg.value_dead(val) && param.op_dependent()).then_some(val)
            })
            .collect();
        let is_noise;
        let pruned;

        (self.op_dependent_insts, is_noise, pruned) = prune_unknowns(
            self.db,
            &mut self.func,
            &mut self.intern,
            &self.dom_tree,
            &op_dependent_params,
        );

        let mut pruned_nodes = AHashSet::default();
        for pruned_param in pruned.iter() {
            let node = match self.intern.params.get_index(pruned_param).unwrap().0 {
                ParamKind::Voltage { hi, lo: None } => *hi,
                ParamKind::Current(_) => continue,
                _ => unreachable!(),
            };
            pruned_nodes.insert(node);
        }

        pruned_nodes
    }

    fn bound_step_op_dependent(&self) -> bool {
        self.op_dependent_insts.iter().any(|inst| {
            if let InstructionData::Call { func_ref, .. } = self.func.dfg.insts[inst] {
                self.intern.callbacks[func_ref] == CallBackKind::BoundStep
            } else {
                false
            }
        })
    }
}
