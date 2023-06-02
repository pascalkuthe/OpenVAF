use ahash::AHashSet;
use bitset::{BitSet, SparseBitMatrix};
use hir_def::{NodeId, Type};
use hir_lower::{
    CallBackKind, CurrentKind, HirInterner, ImplicitEquation, MirBuilder, ParamKind, PlaceKind,
};
use hir_ty::inference::BranchWrite;
use indexmap::{IndexMap, IndexSet};
use lasso::Rodeo;
use mir::builder::InstBuilder;
use mir::cursor::{Cursor, FuncCursor};
use mir::{ControlFlowGraph, DominatorTree, Function, Inst, InstructionData, Value, FALSE};
use mir_autodiff::auto_diff;
use mir_opt::{
    agressive_dead_code_elimination, dead_code_elimination, inst_combine, propagate_taint,
    simplify_cfg, simplify_cfg_no_phi_merge, sparse_conditional_constant_propagation, ClassId, GVN,
};
use stdx::packed_option::PackedOption;
use stdx::{impl_debug_display, impl_idx_from};
use typed_indexmap::TiMap;

use crate::compilation_db::{CompilationDB, ModuleInfo};
use crate::lim_rhs::LimRhs;
use crate::matrix::JacobianMatrix;
use crate::prune::prune_unkowns;
use crate::residual::Residual;
use crate::util::strip_optbarrier;
use crate::SimUnknown;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct CacheSlot(u32);
impl_idx_from!(CacheSlot(u32));
impl_debug_display! {match CacheSlot{CacheSlot(id) => "cslot{id}";}}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct CollapsePair(u32);
impl_idx_from!(CollapsePair(u32));
impl_debug_display! {match CollapsePair{CollapsePair(id) => "collapse{id}";}}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BoundStepKind {
    None,
    Setup,
    Eval,
}

pub struct EvalMir {
    pub init_inst_func: Function,
    pub init_inst_cfg: ControlFlowGraph,
    pub init_inst_cache_vals: IndexMap<Value, CacheSlot, ahash::RandomState>,
    pub eval_cache_vals: IndexMap<ImplicitEquation, CacheSlot, ahash::RandomState>,
    pub cache_slots: TiMap<CacheSlot, (PackedOption<ClassId>, u32), Type>,
    pub init_inst_intern: HirInterner,

    pub eval_intern: HirInterner,
    pub eval_func: Function,
    pub eval_cfg: ControlFlowGraph,
    pub eval_outputs: BitSet<Value>,
    pub matrix: JacobianMatrix,
    pub residual: Residual,

    pub init_model_intern: HirInterner,
    pub init_model_func: Function,
    pub bound_step: BoundStepKind,

    pub collapse: TiMap<CollapsePair, (SimUnknown, Option<SimUnknown>), Vec<CollapsePair>>,
    pub lim_rhs: LimRhs,
    pub pruned_nodes: AHashSet<NodeId>,
}

impl EvalMir {
    pub fn new(db: &CompilationDB, module: &ModuleInfo, literals: &mut Rodeo) -> EvalMir {
        let (mut func, mut intern) = MirBuilder::new(
            db,
            module.id,
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

        let mut output_values = BitSet::new_empty(func.dfg.num_values());
        output_values.extend(intern.outputs.values().copied().filter_map(PackedOption::expand));

        // TODO enable hidden state or warn
        intern.insert_var_init(db, &mut func, literals);

        let mut cfg = ControlFlowGraph::new();
        cfg.compute(&func);

        simplify_cfg_no_phi_merge(&mut func, &mut cfg);

        dead_code_elimination(&mut func, &output_values);
        sparse_conditional_constant_propagation(&mut func, &cfg);

        inst_combine(&mut func);
        simplify_cfg_no_phi_merge(&mut func, &mut cfg);

        let mut dom_tree = DominatorTree::default();
        dom_tree.compute(&func, &cfg, true, true, false);

        let mut gvn = GVN::default();
        gvn.init(&func, &dom_tree, intern.params.len() as u32);
        gvn.solve(&mut func);
        gvn.remove_unnecessary_insts(&mut func, &dom_tree);
        gvn.clear(&mut func);

        let mut output_block = intern
            .outputs
            .values()
            .find_map(|val| val.expand().and_then(|val| func.dfg.value_def(val).inst()))
            .map_or(func.layout.entry_block().unwrap(), |inst| {
                func.layout.inst_block(inst).unwrap()
            });

        output_values.clear();
        for (kind, val) in intern.outputs.iter() {
            if matches!(kind, PlaceKind::Var(var) if module.op_vars.contains_key(var))
                || matches!(kind, PlaceKind::CollapseImplicitEquation(_))
            {
                output_values.insert(val.unwrap_unchecked());
            }
        }

        let mut op_dependent: IndexSet<Value, _> = intern
            .params
            .raw
            .iter()
            .filter_map(|(param, &val)| {
                if func.dfg.value_dead(val) {
                    return None;
                }
                if param.op_dependent() {
                    Some(val)
                } else {
                    None
                }
            })
            .collect();

        let (mut op_dependent_insts, _is_noise, pruned) =
            prune_unkowns(db, &mut func, &mut intern, &dom_tree, &op_dependent);

        let mut pruned_nodes = AHashSet::default();
        for pruned_param in pruned.iter() {
            let node = match intern.params.get_index(pruned_param).unwrap().0 {
                ParamKind::Voltage { hi, lo: None } => *hi,
                ParamKind::Current(_) => continue,
                _ => unreachable!(),
            };
            pruned_nodes.insert(node);
        }

        let bound_step_op_dependent = op_dependent_insts.iter().any(|inst| {
            if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                intern.callbacks[func_ref] == CallBackKind::BoundStep
            } else {
                false
            }
        });

        let mut cursor = FuncCursor::new(&mut func).at_bottom(output_block);
        let mut residual = Residual::default();

        residual.populate(
            db,
            &mut cursor,
            &mut intern,
            &mut cfg,
            &op_dependent_insts,
            &pruned_nodes,
        );
        let new_output_bb = cursor.current_block().unwrap();
        output_block = new_output_bb;

        dom_tree.compute(&func, &cfg, true, true, false);
        let derivatives = intern.unkowns(&mut func, true);
        let extra_derivatives = residual.jacobian_derivatives(&func, &intern, &derivatives);
        let ad = auto_diff(&mut func, &dom_tree, &derivatives, &extra_derivatives);
        cfg.clear();
        cfg.compute(&func);

        while let Some(terminator) = func.layout.block_terminator(output_block) {
            match func.dfg.insts[terminator] {
                // auto_diff does not generate loops and keeps the dominance structure intact
                // so just follow any jumps/branches to the end of the function
                InstructionData::Branch { then_dst, .. } => output_block = then_dst,
                InstructionData::Jump { destination } => output_block = destination,
                _ => break, // terminator is not a jump we found the exit
            }
        }

        let mut cursor = FuncCursor::new(&mut func).at_bottom(output_block);
        let mut matrix =
            JacobianMatrix::new(&mut cursor, &mut intern, &residual, &ad, &derivatives);
        let mut lim_rhs = LimRhs::new(&mut cursor, &mut intern, &residual, &ad, &derivatives);

        output_values.ensure(cursor.func.dfg.num_values() + 1);
        matrix.insert_opt_barries(&mut cursor, &mut output_values);
        residual.insert_opt_barries(&mut cursor, &mut output_values);
        lim_rhs.insert_opt_barries(&mut cursor, &mut output_values);

        sparse_conditional_constant_propagation(&mut func, &cfg);
        simplify_cfg(&mut func, &mut cfg);
        inst_combine(&mut func);

        dom_tree.compute(&func, &cfg, true, true, false);

        gvn.init(&func, &dom_tree, intern.params.len() as u32);
        gvn.solve(&mut func);
        gvn.remove_unnecessary_insts(&mut func, &dom_tree);

        let mut control_dep = SparseBitMatrix::new(0, 0);
        dom_tree.compute_postdom_frontiers(&cfg, &mut control_dep);

        agressive_dead_code_elimination(
            &mut func,
            &mut cfg,
            &|val, _| output_values.contains(val),
            &control_dep,
        );

        simplify_cfg(&mut func, &mut cfg);

        matrix.strip_opt_barries(&mut func, &mut output_values);
        lim_rhs.strip_opt_barries(&mut func, &mut output_values);
        residual.strip_opt_barries(&mut func, &mut output_values);
        matrix.sparsify();
        residual.sparsify();
        lim_rhs.sparsify();

        let mut collapse = TiMap::default();
        let mut init_output_values = BitSet::new_empty(func.dfg.num_values());
        for (kind, out_val) in intern.outputs.iter_mut() {
            let dst = match kind {
                PlaceKind::Var(var) if module.op_vars.contains_key(var) => &mut output_values,
                PlaceKind::CollapseImplicitEquation(equ)
                    if strip_optbarrier(&func, out_val.unwrap_unchecked()) != FALSE
                        && residual.contains(SimUnknown::Implicit(*equ)) =>
                {
                    collapse.insert((SimUnknown::Implicit(*equ), None), vec![]);
                    &mut init_output_values
                }

                PlaceKind::CollapseImplicitEquation(_) => {
                    let old_val = out_val.unwrap_unchecked();
                    output_values.remove(old_val);
                    continue;
                }

                _ => continue,
            };
            let old_val = out_val.unwrap_unchecked();
            let val = strip_optbarrier(&func, old_val);
            dst.insert(val);
            output_values.remove(old_val);
            *out_val = val.into();
        }

        // it's possible that the residual generated new op_dependent
        // variables or that previous ones were removed so recompute them here
        op_dependent.clear();
        op_dependent.extend(intern.params.raw.iter().filter_map(|(param, &val)| {
            if func.dfg.value_dead(val) {
                return None;
            }
            if param.op_dependent() {
                Some(val)
            } else {
                None
            }
        }));

        op_dependent_insts.clear();
        op_dependent_insts.ensure(func.dfg.num_insts());
        let mut has_bound_step = false;
        for inst in func.dfg.insts.iter() {
            if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                match intern.callbacks[func_ref] {
                    CallBackKind::SimParam
                    | CallBackKind::SimParamOpt
                    | CallBackKind::StoreLimit(_)
                    | CallBackKind::Analysis
                    | CallBackKind::SimParamStr => {
                        op_dependent_insts.insert(inst);
                        op_dependent.insert(func.dfg.first_result(inst));
                    }
                    CallBackKind::BoundStep if func.layout.inst_block(inst).is_some() => {
                        has_bound_step = true;
                        if bound_step_op_dependent {
                            op_dependent_insts.insert(inst);
                        }
                    }
                    CallBackKind::LimDiscontinuity | CallBackKind::BuiltinLimit { .. } => {
                        op_dependent_insts.insert(inst);
                    }
                    _ => (),
                }
            }
        }

        dom_tree.compute(&func, &cfg, true, true, false);
        propagate_taint(&func, &dom_tree, &op_dependent, &mut op_dependent_insts);

        let mut init_inst_func = func.clone();
        let mut init_inst_cfg = cfg.clone();

        let mut init_inst_cache: IndexSet<_, ahash::RandomState> = IndexSet::default();
        let mut eval_cache: IndexSet<_, ahash::RandomState> = IndexSet::default();

        for inst in func.dfg.insts.iter() {
            let bb = if let Some(bb) = init_inst_func.layout.inst_block(inst) {
                bb
            } else {
                continue;
            };

            if op_dependent_insts.contains(inst) {
                if !func.dfg.insts[inst].is_terminator() {
                    if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                        match intern.callbacks[func_ref] {
                            CallBackKind::CollapseHint(_, _) => {
                                func.dfg.zap_inst(inst);
                                func.layout.remove_inst(inst);
                            }

                            CallBackKind::StoreDelayTime(eq) => {
                                let arg = func.dfg.instr_args(inst)[0];
                                if init_inst_func.dfg.value_def(arg).inst().is_some() {
                                    eval_cache.insert((arg, eq));
                                }
                            }
                            _ => (),
                        }
                    }
                    init_inst_func.dfg.zap_inst(inst);
                    init_inst_func.layout.remove_inst(inst);
                } else if let InstructionData::Branch { else_dst, then_dst, .. } =
                    func.dfg.insts[inst]
                {
                    init_inst_func.dfg.replace(inst).jump(else_dst);
                    let block = init_inst_func.layout.inst_block(inst).unwrap();
                    let mut pos = init_inst_func.layout.block_inst_cursor(then_dst);
                    while let Some(inst) = pos.next(&init_inst_func.layout) {
                        if init_inst_func.dfg.insts[inst].is_phi() {
                            let rem = init_inst_func.dfg.try_remove_phi_edge_at(inst, block);
                            debug_assert!(rem.is_some());
                        }
                    }
                    init_inst_cfg.recompute_block(&init_inst_func, bb);
                }
            } else if func
                .dfg
                .inst_results(inst)
                .iter()
                .any(|val| func.dfg.tag(*val).is_some() || output_values.contains(*val))
            {
                func.dfg.zap_inst(inst);
                func.layout.remove_inst(inst);

                for val in func.dfg.insts.results(inst) {
                    let param = init_inst_cache.insert_full(*val).0 + intern.params.len();
                    func.dfg.values.make_param_at(param.into(), *val);
                }
            } else if func.dfg.has_sideeffects(inst, false)
                && func.dfg.inst_results(inst).is_empty()
            {
                if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                    if let CallBackKind::CollapseHint(hi, lo) = intern.callbacks[func_ref] {
                        if let Some(lo) = lo {
                            collapse.insert(
                                (SimUnknown::KirchoffLaw(hi), Some(SimUnknown::KirchoffLaw(lo))),
                                vec![],
                            );
                        } else if pruned_nodes.contains(&hi) {
                            init_inst_func.dfg.zap_inst(inst);
                            init_inst_func.layout.remove_inst(inst);
                        } else {
                            collapse.insert((SimUnknown::KirchoffLaw(hi), None), vec![]);
                        }
                    }
                }
                func.dfg.zap_inst(inst);
                func.layout.remove_inst(inst);
            } else if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                if let CallBackKind::StoreDelayTime(_) = intern.callbacks[func_ref] {
                    func.dfg.zap_inst(inst);
                    func.layout.remove_inst(inst);
                    init_inst_func.dfg.zap_inst(inst);
                    init_inst_func.layout.remove_inst(inst);

                    let arg = func.dfg.instr_args(inst)[0];
                    let res = func.dfg.first_result(inst);
                    func.dfg.replace_uses(res, arg);
                    init_inst_func.dfg.replace_uses(res, arg);
                    if func.dfg.value_def(arg).inst().is_some() {
                        let param = init_inst_cache.insert_full(arg).0 + intern.params.len();
                        func.dfg.values.make_param_at(param.into(), arg);
                    }
                }
            }
        }

        for nodes in [residual.resistive.raw.keys(), residual.reactive.raw.keys()] {
            for node in nodes {
                if let SimUnknown::Current(curr) = node {
                    let (hi, lo) = match *curr {
                        CurrentKind::Branch(br) => BranchWrite::Named(br).nodes(db),
                        CurrentKind::Unnamed { hi, lo } => (hi, lo),
                        CurrentKind::Port(_) => continue,
                    };
                    let lo = lo.map(SimUnknown::KirchoffLaw);
                    let hi = SimUnknown::KirchoffLaw(hi);
                    if collapse.contains_key(&(hi, lo)) {
                        // careful, if we insert extra derivatives for currents then we need to
                        // check that we are not overwritign that list here
                        let pair = collapse.insert_full((*node, None), vec![]).0;
                        if let Some(extra_collapse) = collapse.raw.get_mut(&(hi, lo)) {
                            extra_collapse.push(pair)
                        }
                    }
                }
            }
        }

        dom_tree.compute(&func, &cfg, false, true, false);
        dom_tree.compute_postdom_frontiers(&cfg, &mut control_dep);

        agressive_dead_code_elimination(
            &mut func,
            &mut cfg,
            &|val, _| output_values.contains(val),
            &control_dep,
        );
        simplify_cfg(&mut func, &mut cfg);

        let mut extra_class = 0;
        let mut cache_slots: TiMap<CacheSlot, _, _> = TiMap::default();
        let mut ensure_cache_slot = |inst: Option<Inst>, res, ty| {
            let class = inst.and_then(|inst| gvn.inst_class(inst).expand());
            let equiv_class = class.unwrap_or_else(|| {
                let class = gvn.num_class() + extra_class;
                extra_class += 1;
                class.into()
            });
            cache_slots.insert_full((equiv_class.into(), res as u32), ty).0
        };

        let init_inst_cache_vals: IndexMap<_, _, _> = init_inst_cache
            .iter()
            .filter_map(|&val| {
                if func.dfg.value_dead(val) && !output_values.contains(val) {
                    func.dfg.values.fconst_at(0f64.into(), val);
                    return None;
                }

                let ty = if let Some(tag) = func.dfg.tag(val) {
                    let idx = usize::from(tag);
                    let place = intern.outputs.get_index(idx).unwrap().0;
                    place.ty(db)
                } else {
                    Type::Real
                };

                let (inst, res) = init_inst_func.dfg.value_def(val).unwrap_result();
                let pos = ensure_cache_slot(Some(inst), res, ty);

                func.dfg.values.make_param_at((pos.0 as usize + intern.params.len()).into(), val);
                let val = FuncCursor::new(&mut init_inst_func)
                    .after_inst_no_phi(inst)
                    .ins()
                    .optbarrier(val);

                Some((val, pos))
            })
            .collect();
        let eval_cache_vals = eval_cache
            .iter()
            .filter_map(|&(val, eq)| {
                if func.dfg.value_dead(val) && !output_values.contains(val) {
                    func.dfg.values.fconst_at(0f64.into(), val);
                    return None;
                }

                let (inst, res) = match func.dfg.value_def(val).result() {
                    Some((inst, res)) => (Some(inst), res),
                    None => (None, 0),
                };

                let pos = ensure_cache_slot(inst, res, Type::Real);
                Some((eq, pos))
            })
            .collect();

        dom_tree.compute(&init_inst_func, &init_inst_cfg, false, true, false);
        dom_tree.compute_postdom_frontiers(&init_inst_cfg, &mut control_dep);

        // TODO seperate model dependent code
        agressive_dead_code_elimination(
            &mut init_inst_func,
            &mut init_inst_cfg,
            &|val, _| init_inst_cache_vals.contains_key(&val) || init_output_values.contains(val),
            &control_dep,
        );

        simplify_cfg(&mut init_inst_func, &mut init_inst_cfg);

        let eval_intern = intern.clone();
        let mut init_inst_intern = intern;

        let inst_params: Vec<_> = module
            .params
            .iter()
            .filter_map(|(param, info)| info.is_instance.then_some(*param))
            .collect();
        init_inst_intern.insert_param_init(
            db,
            &mut init_inst_func,
            literals,
            false,
            true,
            &inst_params,
        );
        init_inst_cfg.compute(&init_inst_func);

        let mut init_model_func = Function::default();

        let model_params: Vec<_> = module.params.keys().copied().collect();

        let mut init_model_intern = HirInterner::default();

        init_model_intern.insert_param_init(
            db,
            &mut init_model_func,
            literals,
            false,
            true,
            &model_params,
        );

        let mut init_model_cfg = ControlFlowGraph::new();
        init_model_cfg.compute(&init_model_func);
        simplify_cfg(&mut init_model_func, &mut init_model_cfg);
        sparse_conditional_constant_propagation(&mut init_model_func, &init_model_cfg);
        simplify_cfg(&mut init_model_func, &mut init_model_cfg);

        let bound_step = if has_bound_step {
            if bound_step_op_dependent {
                BoundStepKind::Eval
            } else {
                BoundStepKind::Setup
            }
        } else {
            BoundStepKind::None
        };

        EvalMir {
            init_inst_func,
            init_inst_cfg,
            init_inst_cache_vals,
            eval_cache_vals,
            cache_slots,
            init_inst_intern,
            eval_intern,
            lim_rhs,
            eval_func: func,
            eval_cfg: cfg,
            eval_outputs: output_values,
            matrix,
            residual,
            init_model_intern,
            init_model_func,
            bound_step,
            collapse,
            pruned_nodes,
        }
    }
}

impl CompilationDB {
    pub fn build_opvar_mir(
        &self,
        info: &ModuleInfo,
    ) -> (Function, HirInterner, Rodeo, ControlFlowGraph) {
        let outputs: AHashSet<_> = info.op_vars.keys().copied().collect();
        let mut literals = Rodeo::new();
        let (mut func, mut intern) = MirBuilder::new(
            self,
            info.id,
            &|kind| {
                matches!(
                    kind,
                    PlaceKind::Var(var) if outputs.contains(&var)
                )
            },
            &mut outputs.iter().copied(),
        )
        .build(&mut literals);

        // remove unused sideeffects
        for (id, _) in intern.callbacks.iter_enumerated() {
            func.dfg.signatures[id].has_sideeffects = false;
        }

        let mut output_values = BitSet::new_empty(func.dfg.num_values());
        output_values.extend(intern.outputs.values().filter_map(|it| it.expand()));

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

        dead_code_elimination(&mut func, &output_values);

        let mut dom_tree = DominatorTree::default();
        dom_tree.compute(&func, &cfg, true, false, true);
        let unkowns = intern.unkowns(&mut func, false);
        auto_diff(&mut func, &dom_tree, &unkowns, &[]);
        cfg.clear();
        cfg.compute(&func);
        sparse_conditional_constant_propagation(&mut func, &cfg);
        inst_combine(&mut func);
        simplify_cfg(&mut func, &mut cfg);

        dom_tree.compute(&func, &cfg, false, true, false);
        let mut control_dep = SparseBitMatrix::new(0, 0);
        dom_tree.compute_postdom_frontiers(&cfg, &mut control_dep);
        output_values.ensure(func.dfg.num_values());
        agressive_dead_code_elimination(
            &mut func,
            &mut cfg,
            &|val, _| output_values.contains(val),
            &control_dep,
        );
        simplify_cfg(&mut func, &mut cfg);
        (func, intern, literals, cfg)
        // (func, intern, literals, Vec::new(), cfg)
    }
}
