use std::mem::swap;

use bitset::BitSet;
use hir_def::db::HirDefDB;
use hir_def::{NodeId, Type};
use hir_lower::{CallBackKind, HirInterner, MirBuilder, ParamKind, PlaceKind};
use indexmap::{IndexMap, IndexSet};
use lasso::Rodeo;
use mir::builder::InstBuilder;
use mir::cursor::{Cursor, FuncCursor};
use mir::{ControlFlowGraph, Function, InstructionData, Opcode, Value};
use mir_autodiff::auto_diff;
use mir_opt::{
    agressive_dead_code_elimination, dead_code_elimination, inst_combine, postdom_frontiers,
    propagate_taint, simplify_cfg, sparse_conditional_constant_propagation, ClassId, DominatorTree,
    GVN,
};
use stdx::packed_option::PackedOption;
use stdx::{impl_debug_display, impl_idx_from};
use typed_indexmap::{TiMap, TiSet};

use crate::compilation_db::{CompilationDB, ModuleInfo};
use crate::matrix::JacobianMatrix;
use crate::residual::Residual;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct CacheSlot(u32);
impl_idx_from!(CacheSlot(u32));
impl_debug_display! {match CacheSlot{CacheSlot(id) => "cslot{id}";}}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct CollapsePair(u32);
impl_idx_from!(CollapsePair(u32));
impl_debug_display! {match CollapsePair{CollapsePair(id) => "collapse{id}";}}

pub struct EvalMir {
    pub init_inst_func: Function,
    pub init_inst_cfg: ControlFlowGraph,
    pub init_inst_cache_vals: IndexMap<Value, CacheSlot, ahash::RandomState>,
    pub init_inst_cache_slots: TiMap<CacheSlot, (ClassId, u32), Type>,
    pub init_inst_intern: HirInterner,

    pub eval_intern: HirInterner,
    pub eval_func: Function,
    pub eval_cfg: ControlFlowGraph,
    pub eval_outputs: BitSet<Value>,
    pub matrix: JacobianMatrix,
    pub residual: Residual,

    pub init_model_intern: HirInterner,
    pub init_model_func: Function,

    pub collapse: TiSet<CollapsePair, (NodeId, Option<NodeId>)>,
}

impl EvalMir {
    pub fn new(db: &CompilationDB, module: &ModuleInfo, literals: &mut Rodeo) -> EvalMir {
        let (mut func, mut intern) = MirBuilder::new(
            db,
            module.id.into(),
            &|kind| match kind {
                PlaceKind::BranchVoltage { .. }
                | PlaceKind::ImplicitBranchVoltage { .. }
                | PlaceKind::BranchCurrent { .. }
                | PlaceKind::ImplicitBranchCurrent { .. } => true,
                PlaceKind::Var(var) => module.op_vars.contains_key(&var),
                _ => false,
            },
            &mut module.op_vars.keys().copied(),
        )
        .with_split_contributions()
        .with_tagged_writes()
        .build(literals);

        // TODO enable hidden state or warn
        intern.insert_var_init(db, &mut func, literals);

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
        output_values.extend(intern.outputs.values().copied().filter_map(PackedOption::expand));

        let unkowns = intern.unkowns(&mut func, true);

        let extra_derivatives: Vec<_> = intern
            .outputs
            .iter()
            .filter_map(|(kind, val)| {
                if matches!(
                    kind,
                    PlaceKind::BranchVoltage { .. }
                        | PlaceKind::ImplicitBranchVoltage { .. }
                        | PlaceKind::BranchCurrent { .. }
                        | PlaceKind::ImplicitBranchCurrent { .. }
                ) {
                    Some(val.expand()?)
                } else {
                    None
                }
            })
            .flat_map(|val| {
                let unkowns = &unkowns;
                intern.live_params(&func.dfg).filter_map(move |(_, kind, param)| {
                    if matches!(kind, ParamKind::Voltage { .. } | ParamKind::Current(_)) {
                        Some((val, unkowns.unkowns.unwrap_index(&param)))
                    } else {
                        None
                    }
                })
            })
            .collect();

        dead_code_elimination(&mut func, &output_values);
        sparse_conditional_constant_propagation(&mut func, &cfg);
        inst_combine(&mut func);
        simplify_cfg(&mut func, &mut cfg);
        let mut dom_tree = DominatorTree::default();
        dom_tree.compute(&func, &cfg);
        let mut gvn = GVN::default();
        gvn.init(&func, &dom_tree, intern.params.len() as u32);
        gvn.solve(&mut func);
        gvn.remove_unnecessary_insts(&mut func, &dom_tree);
        gvn.clear(&mut func);

        let ad = auto_diff(&mut func, &cfg, &unkowns, &extra_derivatives);

        let mut matrix = JacobianMatrix::default();
        let mut residual = Residual::default();

        let output_block = {
            let inst = intern
                .outputs
                .values()
                .find_map(|val| val.expand().and_then(|val| func.dfg.value_def(val).inst()))
                .unwrap();
            func.layout.inst_block(inst).unwrap()
        };

        let mut cursor = FuncCursor::new(&mut func).at_bottom(output_block);

        matrix.populate(db, &mut cursor, &mut intern, &ad, &unkowns);
        residual.populate(db, &mut cursor, &mut intern);

        output_values.ensure(cursor.func.dfg.num_values() + 1);
        output_values.clear();

        matrix.insert_opt_barries(&mut cursor, &mut output_values);
        residual.insert_opt_barries(&mut cursor, &mut output_values);

        for (kind, out_val) in intern.outputs.iter_mut() {
            if matches!(kind, PlaceKind::Var(var) if module.op_vars.contains_key(var)) {
                let val = out_val.unwrap_unchecked();
                output_values.insert(val);
            }
        }

        sparse_conditional_constant_propagation(&mut func, &cfg);
        simplify_cfg(&mut func, &mut cfg);
        inst_combine(&mut func);
        dom_tree.clear();
        dom_tree.compute(&func, &cfg);
        gvn.init(&func, &dom_tree, intern.params.len() as u32);
        gvn.solve(&mut func);
        gvn.remove_unnecessary_insts(&mut func, &dom_tree);

        let control_dep = postdom_frontiers(&cfg, &func);
        agressive_dead_code_elimination(
            &mut func,
            &mut cfg,
            &|val, _| output_values.contains(val),
            &control_dep,
        );

        simplify_cfg(&mut func, &mut cfg);

        matrix.strip_opt_barries(&mut func, &mut output_values);
        residual.strip_opt_barries(&mut func, &mut output_values);
        matrix.sparsify();
        residual.sparsify();

        for (kind, out_val) in intern.outputs.iter_mut() {
            if matches!(kind, PlaceKind::Var(var) if module.op_vars.contains_key(var)) {
                let val = out_val.unwrap_unchecked();
                if let Some(inst) = func.dfg.value_def(val).inst() {
                    if let InstructionData::Unary { opcode: Opcode::OptBarrier, arg } =
                        func.dfg.insts[inst]
                    {
                        output_values.remove(val);
                        output_values.insert(arg);
                        *out_val = arg.into();
                    }
                }
            }
        }


        let mut op_dependent: Vec<Value> = intern
            .params
            .raw
            .iter()
            .filter_map(|(param, &val)| {
                if func.dfg.value_dead(val) {
                    return None;
                }
                if matches!(
                    param,
                    ParamKind::Voltage { .. } | ParamKind::Current(_) | ParamKind::HiddenState(_)
                ) {
                    Some(val)
                } else {
                    None
                }
            })
            .collect();

        for inst in func.dfg.insts.iter() {
            if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                match intern.callbacks[func_ref] {
                    CallBackKind::SimParam
                    | CallBackKind::SimParamOpt
                    | CallBackKind::SimParamStr => op_dependent.push(func.dfg.first_result(inst)),
                    _ => (),
                }
            }
        }

        let control_dep = postdom_frontiers(&cfg, &func);
        let op_dependent = propagate_taint(&func, &control_dep, &op_dependent);
        let mut collapse = TiSet::default();

        let mut init_inst_func = func.clone();
        let mut init_inst_cfg = cfg.clone();

        let mut init_inst_cache: IndexSet<_, ahash::RandomState> = IndexSet::default();


        for inst in func.dfg.insts.iter() {
            let bb = if let Some(bb) = init_inst_func.layout.inst_block(inst) {
                bb
            } else {
                continue;
            };

            if op_dependent.contains(inst) {
                if !func.dfg.insts[inst].is_terminator() {
                    if let InstructionData::Call { func_ref, .. } = func.dfg.insts[inst] {
                        if let CallBackKind::CollapseHint(_, _) = intern.callbacks[func_ref] {
                            // TODO WARN
                        }
                    }
                    init_inst_func.dfg.zap_inst(inst);
                    init_inst_func.layout.remove_inst(inst);
                } else if let InstructionData::Branch { else_dst, .. } = func.dfg.insts[inst] {
                    init_inst_func.dfg.replace(inst).jump(else_dst);
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
                    if let CallBackKind::CollapseHint(mut hi, lo) = intern.callbacks[func_ref] {
                        let hi_gnd = db.node_data(hi).is_gnd;
                        if let Some(mut lo) = lo {
                            if hi_gnd {
                                if db.node_data(lo).is_gnd {
                                    continue;
                                }
                                swap(&mut lo, &mut hi);
                            }

                            collapse.ensure((hi, Some(lo)));
                        } else {
                            if hi_gnd {
                                continue;
                            }
                            collapse.ensure((hi, None));
                        }
                    }
                }
                func.dfg.zap_inst(inst);
                func.layout.remove_inst(inst);
            }
        }

        agressive_dead_code_elimination(
            &mut func,
            &mut cfg,
            &|val, _| output_values.contains(val),
            &control_dep,
        );
        simplify_cfg(&mut func, &mut cfg);

        let mut extra_class = 0;
        let mut init_inst_cache_slots: TiMap<CacheSlot, _, _> = TiMap::default();
        let init_inst_cache_vals: IndexMap<_, _, _> = init_inst_cache
            .iter()
            .filter_map(|val| {
                if func.dfg.value_dead(*val) && !output_values.contains(*val) {
                    func.dfg.values.fconst_at(0f64.into(), *val);
                    return None;
                }

                let ty = if let Some(tag) = func.dfg.tag(*val) {
                    let idx = usize::from(tag);
                    match *intern.outputs.get_index(idx).unwrap().0 {
                        PlaceKind::Var(var) => db.var_data(var).ty.clone(),
                        PlaceKind::FunctionReturn { fun } => {
                            db.function_data(fun).return_ty.clone()
                        }
                        PlaceKind::FunctionArg { fun, arg } => {
                            db.function_data(fun).args[arg].ty.clone()
                        }
                        PlaceKind::BranchVoltage { .. }
                        | PlaceKind::BranchCurrent { .. }
                        | PlaceKind::ImplicitBranchVoltage { .. }
                        | PlaceKind::ImplicitBranchCurrent { .. } => Type::Real,
                        PlaceKind::ParamMin(_) | PlaceKind::ParamMax(_) => {
                            unreachable!()
                        }
                        PlaceKind::Param(_) => unreachable!(),
                    }
                } else {
                    Type::Real
                };

                let (inst, res) = init_inst_func.dfg.value_def(*val).unwrap_result();

                let equiv_class = gvn.inst_class(inst).expand().unwrap_or_else(|| {
                    let res = gvn.num_class() + extra_class;
                    extra_class += 1;
                    res.into()
                });
                let pos = init_inst_cache_slots.insert_full((equiv_class, res as u32), ty).0;

                func.dfg.values.make_param_at((pos.0 as usize + intern.params.len()).into(), *val);
                let inst = init_inst_func.dfg.value_def(*val).unwrap_inst();
                let val = FuncCursor::new(&mut init_inst_func)
                    .after_inst_no_phi(inst)
                    .ins()
                    .optbarrier(*val);

                Some((val, pos))
            })
            .collect();

        // TODO seperate model dependent code
        agressive_dead_code_elimination(
            &mut init_inst_func,
            &mut init_inst_cfg,
            &|val, _| init_inst_cache_vals.contains_key(&val),
            &control_dep,
        );

        simplify_cfg(&mut init_inst_func, &mut init_inst_cfg);

        let eval_intern = intern.clone();
        let mut init_inst_intern = intern;

        let inst_params: Vec<_> = module
            .params
            .iter()
            .filter_map(|(param, info)| info.is_instance.then(|| *param))
            .collect();
        init_inst_intern.insert_param_init(db, &mut init_inst_func, literals, false, &inst_params);
        init_inst_cfg.compute(&init_inst_func);

        let mut init_model_func = Function::default();

        let inst_params: Vec<_> = module
            .params
            .iter()
            .filter_map(|(param, info)| (!info.is_instance).then(|| *param))
            .collect();

        let mut init_model_intern = HirInterner::default();

        init_model_intern.insert_param_init(
            db,
            &mut init_model_func,
            literals,
            false,
            &inst_params,
        );

        EvalMir {
            init_inst_func,
            init_inst_cfg,
            init_inst_cache_vals,
            init_inst_cache_slots,
            init_inst_intern,
            eval_intern,
            eval_func: func,
            eval_cfg: cfg,
            eval_outputs: output_values,
            matrix,
            residual,
            init_model_intern,
            init_model_func,
            collapse,
        }
    }
}
