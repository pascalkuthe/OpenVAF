use hir::{BranchWrite, CompilationDB, Node};
use hir_lower::{CurrentKind, HirInterner, ImplicitEquation};
use lasso::Rodeo;
use mir::Function;
use mir_opt::{simplify_cfg, sparse_conditional_constant_propagation};
use stdx::impl_debug_display;

pub use module_info::{collect_modules, ModuleInfo};

use crate::context::{Context, OptimiziationStage};
use crate::dae::DaeSystem;
use crate::init::Initialization;
use crate::node_collapse::NodeCollapse;
use crate::topology::Topology;

mod context;
pub mod dae;
pub mod init;
mod module_info;
pub mod node_collapse;
mod noise;
mod topology;

mod util;

// #[cfg(test)]
// mod tests;

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub enum SimUnknownKind {
    KirchoffLaw(Node),
    Current(CurrentKind),
    Implicit(ImplicitEquation),
}

impl_debug_display! {
    match SimUnknownKind{
        SimUnknownKind::KirchoffLaw(node) => "{node:?}";
        SimUnknownKind::Current(curr) => "br[{curr:?}]";
        SimUnknownKind::Implicit(node) => "{node}";
    }
}

pub struct CompiledModule<'a> {
    pub info: &'a ModuleInfo,
    pub dae_system: DaeSystem,
    pub eval: Function,
    pub intern: HirInterner,
    pub init: Initialization,
    pub model_param_setup: Function,
    pub model_param_intern: HirInterner,
    pub node_collapse: NodeCollapse,
}

impl<'a> CompiledModule<'a> {
    pub fn new(
        db: &CompilationDB,
        module: &'a ModuleInfo,
        literals: &mut Rodeo,
    ) -> CompiledModule<'a> {
        let mut cx = Context::new(db, literals, module);
        cx.compute_outputs(true);
        cx.compute_cfg();
        cx.optimize(OptimiziationStage::Initial);
        debug_assert!(cx.func.validate());

        let topology = Topology::new(&mut cx);
        debug_assert!(cx.func.validate());
        let mut dae_system = DaeSystem::new(&mut cx, topology);
        debug_assert!(cx.func.validate());
        cx.compute_cfg();
        let gvn = cx.optimize(OptimiziationStage::PostDerivative);
        dae_system.sparsify(&mut cx);
        debug_assert!(cx.func.validate());

        cx.refresh_op_dependent_insts();
        let mut init = Initialization::new(&mut cx, gvn);
        let node_collapse = NodeCollapse::new(&init, &dae_system, &cx);
        debug_assert!(cx.func.validate());
        debug_assert!(init.func.validate());

        // TODO: refactor param intilization to use tables
        let inst_params: Vec<_> = module
            .params
            .iter()
            .filter_map(|(param, info)| info.is_instance.then_some(*param))
            .collect();
        init.intern.insert_param_init(db, &mut init.func, literals, false, true, &inst_params);

        let mut model_param_setup = Function::default();
        let model_params: Vec<_> = module.params.keys().copied().collect();
        let mut model_param_intern = HirInterner::default();
        model_param_intern.insert_param_init(
            db,
            &mut model_param_setup,
            literals,
            false,
            true,
            &model_params,
        );
        cx.cfg.compute(&model_param_setup);
        simplify_cfg(&mut model_param_setup, &mut cx.cfg);
        sparse_conditional_constant_propagation(&mut model_param_setup, &cx.cfg);
        simplify_cfg(&mut model_param_setup, &mut cx.cfg);

        CompiledModule {
            eval: cx.func,
            intern: cx.intern,
            info: module,
            dae_system,
            init,
            model_param_intern,
            model_param_setup,
            node_collapse,
        }
    }
}
