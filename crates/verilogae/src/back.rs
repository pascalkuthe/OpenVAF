use std::borrow::Borrow;
use std::hash::Hash;
use std::ops::Deref;
use std::path::Path;

use ahash::AHashMap;
use backend::{
    lltype, places, sim_param_opt_stub, sim_param_str_stub, sim_param_stub, stub_callbacks,
};
use basedb::Upcast;
use cfg::{Callback, CfgParam, ControlFlowGraph, Operand, Terminator};
use cfg_opt::{remove_dead_data, simplify_branches, simplify_cfg};
use codegen_llvm::{Builder, CallbackFun, CodegenCx, LLVMBackend};
use const_eval::conditional_const_propagation;
use hir_def::db::HirDefDB;
use hir_def::{Lookup, NodeId, ParamId, Type, VarId};
use hir_lower::{CallBackKind, CurrentKind, HirInterner, ParamInfoKind, ParamKind, PlaceKind};
use lasso::Rodeo;
use llvm::{IntPredicate, Value, UNNAMED};
use salsa::Snapshot;
use stdx::iter::multiunzip;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::compiler_db::{CompilationDB, Function, ModelInfo};

pub fn param_stub_callbacks<'ll>(
    cb: &TiSet<Callback, CallBackKind>,
    cx: &mut CodegenCx<'_, 'll>,
    invalid_param_dst: &AHashMap<ParamId, &'ll Value>,
) -> TiVec<Callback, CallbackFun<'ll>> {
    let mut param_info_set_cb = None;
    let mut param_info_unset_cb = None;
    cb.raw
        .iter()
        .map(|kind| match kind {
            CallBackKind::SimParam => sim_param_stub(cx),
            CallBackKind::SimParamOpt => sim_param_opt_stub(cx),
            CallBackKind::SimParamStr => sim_param_str_stub(cx),
            CallBackKind::Derivative(_) => unreachable!("Derivative must not remain in final CFG!"),
            CallBackKind::NodeDerivative(_) => {
                unreachable!("Derivative must not remain in final CFG!")
            }
            CallBackKind::ParamInfo(kind, param) => {
                let (set, mut bits) = match kind {
                    ParamInfoKind::Invalid => (true, 0b100),
                    ParamInfoKind::MinInclusive => (true, 0b001),
                    ParamInfoKind::MaxInclusive => (true, 0b010),
                    ParamInfoKind::MinExclusive => (false, 0b001),
                    ParamInfoKind::MaxExclusive => (false, 0b010),
                };

                if !set {
                    bits = !bits;
                }

                let cb = if set { &mut param_info_set_cb } else { &mut param_info_unset_cb };

                let (fun, fun_ty) = match *cb {
                    Some((fun_ty, fun)) => (fun_ty, fun),
                    None => {
                        let name = cx.local_callback_name();
                        let fun_ty =
                            cx.ty_func(&[cx.ptr_ty(cx.ty_c_bool()), cx.ty_c_bool()], cx.ty_void());
                        let fun = cx.declare_int_fn(&name, fun_ty);
                        unsafe {
                            let bb = llvm::LLVMAppendBasicBlockInContext(cx.llcx, fun, UNNAMED);
                            let builder = llvm::LLVMCreateBuilderInContext(cx.llcx);
                            llvm::LLVMPositionBuilderAtEnd(builder, bb);
                            let ptr = llvm::LLVMGetParam(fun, 0);
                            let flag = llvm::LLVMGetParam(fun, 1);
                            let val = llvm::LLVMBuildLoad2(builder, cx.ty_c_bool(), ptr, UNNAMED);
                            let val = if set {
                                llvm::LLVMBuildOr(builder, val, flag, UNNAMED)
                            } else {
                                llvm::LLVMBuildAnd(builder, val, flag, UNNAMED)
                            };
                            llvm::LLVMBuildStore(builder, val, ptr);
                            llvm::LLVMBuildRetVoid(builder);
                            llvm::LLVMDisposeBuilder(builder);
                        }

                        *cb = Some((fun, fun_ty));
                        (fun, fun_ty)
                    }
                };
                CallbackFun {
                    fun_ty,
                    fun,
                    state: vec![invalid_param_dst[param], cx.const_u8(bits)].into_boxed_slice(),
                }
            }
        })
        .collect()
}

pub(crate) fn compile_model_info(
    db: &Snapshot<CompilationDB>,
    intern: Option<&HirInterner>,
    info: &ModelInfo,
    literals: &mut Rodeo,
    backend: &LLVMBackend,
    dst: &Path,
) {
    // ensure everything is interned so that we only need to resolve later

    #[allow(clippy::needless_collect)] // false positive
    let param_info: Vec<_> = info
        .params
        .values()
        .map(|param| {
            let name = literals.get_or_intern(&*param.name);
            let unit = literals.get_or_intern(&param.unit);
            let description = literals.get_or_intern(&param.description);
            let group = literals.get_or_intern(&param.group);
            (name, unit, description, group, &param.ty)
        })
        .collect();

    if let Some(intern) = intern {
        for param in &intern.params.raw {
            match param {
                ParamKind::Voltage { hi, lo } => {
                    literals.get_or_intern(&voltage_name(db, *hi, *lo));
                }
                ParamKind::Current(kind) => {
                    literals.get_or_intern(&current_name(db, *kind));
                }
                _ => (),
            }
        }
    }

    #[allow(clippy::needless_collect)] // false posivtive this is required to pass borrow check
    let functions: Vec<_> = info
        .functions
        .iter()
        .map(|fun| {
            // ensure that all dependency breaking is interned
            for dep in &*fun.dependency_breaking {
                literals.get_or_intern(&*info.var_names[dep]);
            }
            (
                literals.get_or_intern(&*info.var_names[&fun.var]),
                literals.get_or_intern(&fun.prefix),
            )
        })
        .collect();

    #[allow(clippy::needless_collect)] // false posivtive this is required to pass borrow check
    let op_vars: Vec<_> = info.op_vars.iter().map(|name| literals.get_or_intern(&**name)).collect();

    #[allow(clippy::needless_collect)] // false posivtive this is required to pass borrow check
    let nodes: Vec<_> = info.ports.iter().map(|name| literals.get_or_intern(&**name)).collect();

    let module_name = literals.get_or_intern(&*info.module.lookup(db.upcast()).name(db.upcast()));

    let mut cfg = ControlFlowGraph::default();
    cfg.blocks.push(cfg::BasicBlockData {
        phis: Default::default(),
        instructions: Default::default(),
        terminator: Some(Terminator::Ret),
    });
    let mut intern = HirInterner::default();

    intern.params.raw.extend(info.params.keys().map(|param| ParamKind::Param(*param)));
    intern.insert_param_init(&**db, literals, &mut cfg);

    cfg.visit_operands_mut(|op| {
        if let Operand::CfgParam(param) = *op {
            if let ParamKind::Param(param) = intern.params[param] {
                *op = intern.places.unwrap_index(&PlaceKind::Param(param)).into();
            }
        }
    });

    let (_, place_map, param_map, callback_map) = remove_dead_data(&mut cfg);
    intern.map(&place_map, &param_map, &callback_map);
    conditional_const_propagation(&mut cfg, &AHashMap::new());
    simplify_branches(&mut cfg);
    simplify_cfg(&mut cfg);
    cfg.assert_verified();

    let module = unsafe { backend.new_module("model_info").unwrap() };
    let mut cx = unsafe { backend.new_ctx(literals, &module) };

    let param_info: Vec<_> = param_info
        .into_iter()
        .map(|(name, unit, description, group, ty)| {
            (
                (
                    cx.const_str(name),
                    cx.const_str(unit),
                    cx.const_str(description),
                    cx.const_str(group),
                ),
                ty,
            )
        })
        .collect();

    let (fun_names, fun_symbols): (Vec<_>, Vec<_>) =
        functions.into_iter().map(|(name, sym)| (cx.const_str(name), cx.const_str(sym))).unzip();

    cx.export_array("functions", cx.ty_str(), &fun_names, true, true);
    cx.export_array("functions.sym", cx.ty_str(), &fun_symbols, true, false);

    let op_vars: Vec<_> = op_vars.into_iter().map(|name| cx.const_str(name)).collect();
    cx.export_array("opvars", cx.ty_str(), &op_vars, true, true);

    let nodes: Vec<_> = nodes.into_iter().map(|name| cx.const_str(name)).collect();
    cx.export_array("nodes", cx.ty_str(), &nodes, true, true);

    let module_name = cx.const_str(module_name);
    let module_name_ = cx
        .define_global("module_name", cx.ty_str())
        .unwrap_or_else(|| unreachable!("symbol 'module_name' already defined"));
    unsafe {
        llvm::LLVMSetInitializer(module_name_, module_name);
        llvm::LLVMSetGlobalConstant(module_name_, llvm::True);
        llvm::LLVMSetLinkage(module_name_, llvm::Linkage::ExternalLinkage);
        llvm::LLVMSetUnnamedAddress(module_name_, llvm::UnnamedAddr::No);
        llvm::LLVMSetDLLStorageClass(module_name_, llvm::DLLStorageClass::Export);
    }

    let (params, units, descriptions, groups): (Vec<_>, Vec<_>, Vec<_>, Vec<_>) =
        multiunzip(param_info.iter().filter_map(|(info, ty)| (*ty == &Type::Real).then(|| *info)));

    cx.export_array("params.real", cx.ty_str(), &params, true, true);
    cx.export_array("params.unit.real", cx.ty_str(), &units, true, false);
    cx.export_array("params.desc.real", cx.ty_str(), &descriptions, true, false);
    cx.export_array("params.group.real", cx.ty_str(), &groups, true, false);

    let (params, units, descriptions, groups): (Vec<_>, Vec<_>, Vec<_>, Vec<_>) = multiunzip(
        param_info.iter().filter_map(|(info, ty)| (*ty == &Type::Integer).then(|| *info)),
    );

    cx.export_array("params.integer", cx.ty_str(), &params, true, true);
    cx.export_array("params.unit.integer", cx.ty_str(), &units, true, false);
    cx.export_array("params.desc.integer", cx.ty_str(), &descriptions, true, false);
    cx.export_array("params.group.integer", cx.ty_str(), &groups, true, false);

    let (params, units, descriptions, groups): (Vec<_>, Vec<_>, Vec<_>, Vec<_>) = multiunzip(
        param_info.iter().filter_map(|(info, ty)| (*ty == &Type::String).then(|| *info)),
    );

    cx.export_array("params.string", cx.ty_str(), &params, true, true);
    cx.export_array("params.unit.string", cx.ty_str(), &units, true, false);
    cx.export_array("params.desc.string", cx.ty_str(), &descriptions, true, false);
    cx.export_array("params.group.string", cx.ty_str(), &groups, true, false);

    let fun_ty = cx.ty_func(
        &[
            cx.ptr_ty(cx.ty_real()),   // real param values
            cx.ptr_ty(cx.ty_int()),    // int param values
            cx.ptr_ty(cx.ty_str()),    // str param values
            cx.ptr_ty(cx.ty_real()),   // real param min
            cx.ptr_ty(cx.ty_int()),    // int param min
            cx.ptr_ty(cx.ty_real()),   // real param max
            cx.ptr_ty(cx.ty_int()),    // int param max
            cx.ptr_ty(cx.ty_c_bool()), // param_given/error
        ],
        cx.ty_void(),
    );

    let llfun = cx.declare_ext_fn("init_modelcard", fun_ty);

    let param_val_real = unsafe { llvm::LLVMGetParam(llfun, 0) };
    let param_val_int = unsafe { llvm::LLVMGetParam(llfun, 1) };
    let param_val_str = unsafe { llvm::LLVMGetParam(llfun, 2) };
    let param_min_real = unsafe { llvm::LLVMGetParam(llfun, 3) };
    let param_min_int = unsafe { llvm::LLVMGetParam(llfun, 4) };
    let param_max_real = unsafe { llvm::LLVMGetParam(llfun, 5) };
    let param_max_int = unsafe { llvm::LLVMGetParam(llfun, 6) };
    let param_flags = unsafe { llvm::LLVMGetParam(llfun, 7) };

    let mut builder = Builder::new(&mut cx, &cfg, llfun);

    let real_params =
        info.params.keys().filter(|param| db.param_data(**param).ty == Type::Real).enumerate();
    let int_params =
        info.params.keys().filter(|param| db.param_data(**param).ty == Type::Integer).enumerate();

    let str_params =
        info.params.keys().filter(|param| db.param_data(**param).ty == Type::String).enumerate();

    let param_given = real_params
        .clone()
        .chain(int_params.clone())
        .chain(str_params.clone())
        .map(|(i, param)| {
            (*param, unsafe { builder.gep(param_flags, &[builder.cx.const_usize(i)]) })
        })
        .collect();

    let real_params: AHashMap<_, _> = real_params
        .map(|(i, param)| unsafe {
            (
                *param,
                (
                    builder.gep(param_val_real, &[builder.cx.const_usize(i)]),
                    builder.gep(param_min_real, &[builder.cx.const_usize(i)]),
                    builder.gep(param_max_real, &[builder.cx.const_usize(i)]),
                ),
            )
        })
        .collect();

    let int_params: AHashMap<_, _> = int_params
        .map(|(i, param)| unsafe {
            (
                *param,
                (
                    builder.gep(param_val_int, &[builder.cx.const_usize(i)]),
                    builder.gep(param_min_int, &[builder.cx.const_usize(i)]),
                    builder.gep(param_max_int, &[builder.cx.const_usize(i)]),
                ),
            )
        })
        .collect();

    let str_params: AHashMap<_, _> = str_params
        .map(|(i, param)| unsafe {
            (*param, builder.gep(param_val_str, &[builder.cx.const_usize(i)]))
        })
        .collect();

    builder.callbacks = param_stub_callbacks(&intern.callbacks, builder.cx, &param_given);

    builder.places = places(db.deref(), &builder, intern.places.raw.iter(), |kind| match kind {
        PlaceKind::Param(param) => real_params
            .get(param)
            .map(|(val, _, _)| (builder.cx.ty_real(), *val))
            .or_else(|| int_params.get(param).map(|(val, _, _)| (builder.cx.ty_int(), *val)))
            .or_else(|| str_params.get(param).map(|val| (builder.cx.ty_str(), *val))),

        PlaceKind::ParamMin(param) => real_params
            .get(param)
            .map(|(_, min, _)| (builder.cx.ty_real(), *min))
            .or_else(|| int_params.get(param).map(|(_, min, _)| (builder.cx.ty_int(), *min))),

        PlaceKind::ParamMax(param) => real_params
            .get(param)
            .map(|(_, _, max)| (builder.cx.ty_real(), *max))
            .or_else(|| int_params.get(param).map(|(_, _, max)| (builder.cx.ty_int(), *max))),
        _ => None,
    });

    builder.params = intern
        .params
        .raw
        .iter()
        .map(|kind| match kind {
            ParamKind::Param(_) => unreachable!(),
            ParamKind::Voltage { .. } | ParamKind::Current(_) => {
                unreachable!("const values must not depend upon current/voltage")
            }
            // not my problem if you do this
            ParamKind::Temperature => builder.cx.const_real(293f64),
            ParamKind::ParamGiven { param } => unsafe {
                let c_bool = builder.load(builder.cx.ty_c_bool(), param_given[param]);
                builder.int_cmp(c_bool, builder.cx.const_c_bool(false), IntPredicate::IntNE)
            },
            ParamKind::PortConnected { .. } => builder.cx.const_bool(true),
            ParamKind::ParamSysFun(param) => builder.cx.const_real(param.default_value()),
        })
        .collect();

    unsafe {
        builder.build_cfg();
        builder.build_returns(|builder, _| builder.ret_void())
    }

    assert!(module.verify_and_print(), "generated invalid code");
    module.optimize(backend);
    module.emit_obect(dst).expect("code generation failed!");
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn compile_fun(
    backend: &LLVMBackend,
    db: &Snapshot<CompilationDB>,
    model_info: &ModelInfo,
    cfg: &mut ControlFlowGraph,
    intern: &HirInterner,
    literals: &Rodeo,
    dst: &Path,
    fun: &Function,
    prefix: &str,
) {
    let ret = intern.places.index(&PlaceKind::Var(fun.var)).unwrap();
    let ret_info = db.var_data(fun.var);
    let module = unsafe { backend.new_module(&*ret_info.name).unwrap() };
    let mut cx = unsafe { backend.new_ctx(literals, &module) };

    let ret_ty = lltype(&ret_info.ty, &cx);

    let fun_ty = cx.ty_func(
        &[
            cx.ty_isize(),                             // offset
            cx.ptr_ty(cx.fat_ptr(cx.ty_real(), true)), // voltages
            cx.ptr_ty(cx.fat_ptr(cx.ty_real(), true)), // curents
            cx.ptr_ty(cx.fat_ptr(cx.ty_real(), true)), // real paras
            cx.ptr_ty(cx.fat_ptr(cx.ty_int(), true)),  // int paras
            cx.ptr_ty(cx.fat_ptr(cx.ty_str(), true)),  // str paras
            cx.ptr_ty(cx.fat_ptr(cx.ty_real(), true)), // real dependency_breaking
            cx.ptr_ty(cx.fat_ptr(cx.ty_int(), true)),  // int dependency_breaking
            cx.ptr_ty(cx.fat_ptr(cx.ty_real(), true)), // temperature
            cx.ptr_ty(ret_ty),                         // ret
        ],
        cx.ty_void(),
    );
    let llfun = cx.declare_ext_fn(prefix, fun_ty);
    let true_val = cx.const_bool(true);

    let offset = unsafe { llvm::LLVMGetParam(llfun, 0) };
    let voltages = unsafe { llvm::LLVMGetParam(llfun, 1) };
    let currents = unsafe { llvm::LLVMGetParam(llfun, 2) };
    let real_params = unsafe { llvm::LLVMGetParam(llfun, 3) };
    let int_params = unsafe { llvm::LLVMGetParam(llfun, 4) };
    let str_params = unsafe { llvm::LLVMGetParam(llfun, 5) };
    let real_dep_break = unsafe { llvm::LLVMGetParam(llfun, 6) };
    let int_dep_break = unsafe { llvm::LLVMGetParam(llfun, 7) };
    let temperature = unsafe { llvm::LLVMGetParam(llfun, 8) };
    let out = unsafe { llvm::LLVMGetParam(llfun, 9) };

    // setup builder
    let mut builder = Builder::new(&mut cx, cfg, llfun);

    // setup cfg params

    let voltages =
        unsafe { map_voltages(db, &mut builder, prefix, &intern.params, offset, voltages) };

    let currents =
        unsafe { map_currents(db, &mut builder, prefix, &intern.params, offset, currents) };

    let real_params = unsafe {
        map_params(
            db,
            model_info,
            &mut builder,
            prefix,
            &intern.params,
            offset,
            real_params,
            &Type::Real,
        )
    };

    let int_params = unsafe {
        map_params(
            db,
            model_info,
            &mut builder,
            prefix,
            &intern.params,
            offset,
            int_params,
            &Type::Integer,
        )
    };

    let str_params =
        unsafe { map_str_params(db, model_info, &mut builder, prefix, &intern.params, str_params) };

    let real_dep_break = unsafe {
        map_dependecy_breaking(
            db,
            model_info,
            &mut builder,
            prefix,
            &fun.dependency_breaking,
            offset,
            real_dep_break,
            Type::Real,
        )
    };

    let int_dep_break = unsafe {
        map_dependecy_breaking(
            db,
            model_info,
            &mut builder,
            prefix,
            &fun.dependency_breaking,
            offset,
            int_dep_break,
            Type::Integer,
        )
    };

    builder.places = places(db.deref(), &builder, intern.places.raw.iter(), |_| None);

    for (var, val) in real_dep_break.into_iter().chain(int_dep_break) {
        if let Some(place) = intern.places.index(&PlaceKind::Var(var)) {
            unsafe { builder.store(builder.places[place].1, val) }
        } else {
            use std::io::Write;

            let mut stderr = StandardStream::stderr(ColorChoice::Auto);
            let _ = stderr.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)).set_bold(true));
            let _ = write!(&mut stderr, "warning");
            let _ = stderr.set_color(&ColorSpec::new());
            let name = &model_info.var_names[&fun.var];
            let dep_name = &model_info.var_names[&var];
            let _ = writeln!(
                &mut stderr,
                ":  function '{}' does not depend on variable '{}'",
                name, dep_name
            );
        }
    }

    builder.callbacks = stub_callbacks(&intern.callbacks, builder.cx);

    builder.params = intern
        .params
        .raw
        .iter()
        .map(|kind| match kind {
            ParamKind::Param(param) => real_params
                .get(param)
                .or_else(|| int_params.get(param))
                .copied()
                .unwrap_or_else(|| str_params[param]),
            ParamKind::Voltage { hi, lo } => voltages[&(*hi, *lo)],
            ParamKind::Current(current) => currents[current],
            ParamKind::Temperature => unsafe {
                read_fat_ptr_at(&mut builder, 0, offset, temperature)
            },
            ParamKind::ParamGiven { .. } | ParamKind::PortConnected { .. } => true_val,
            ParamKind::ParamSysFun(param) => builder.cx.const_real(param.default_value()),
        })
        .collect();

    unsafe {
        builder.build_cfg();
        builder.build_returns(|builder, _| {
            let fun_out = builder.gep(out, &[offset]);
            let val = builder.operand(&Operand::Place(ret));
            builder.store(fun_out, val);
            builder.ret_void();
        })
    }

    drop(builder);
    debug_assert!(module.verify_and_print(), "Invalid code generated");
    module.optimize(backend);

    module.emit_obect(dst).expect("code generation failed!")
}

#[allow(clippy::too_many_arguments)]
unsafe fn map_dependecy_breaking<'ll>(
    db: &Snapshot<CompilationDB>,
    model_info: &ModelInfo,
    builder: &mut Builder<'_, '_, 'll>,
    prefix: &str,
    dependency_breaking: &[VarId],
    offset: &'ll llvm::Value,
    ptr: &'ll llvm::Value,
    ty: Type,
) -> AHashMap<VarId, &'ll llvm::Value> {
    let vars = dependency_breaking.iter().copied().filter(|var| db.var_data(*var).ty == ty);
    let global_name = format!("{}.depbreak.{}", prefix, ty);
    let names = vars.clone().map(|var| &*model_info.var_names[&var]);
    export_names(builder.cx, names, &global_name);
    mapping(vars, builder, offset, ptr)
}

unsafe fn map_str_params<'ll>(
    db: &Snapshot<CompilationDB>,
    model_info: &ModelInfo,
    builder: &mut Builder<'_, '_, 'll>,
    prefix: &str,
    params: &TiSet<CfgParam, ParamKind>,
    ptr: &'ll llvm::Value,
) -> AHashMap<ParamId, &'ll llvm::Value> {
    let kinds = params.raw.iter().filter_map(|kind| {
        if let ParamKind::Param(param) = kind {
            (db.param_data(*param).ty == Type::String).then(|| *param)
        } else {
            None
        }
    });

    let global_name = format!("{}.params.{}", prefix, Type::String);
    let names = kinds.clone().map(|param| &*model_info.params[&param].name);
    export_names(builder.cx, names, &global_name);
    kinds
        .enumerate()
        .map(|(i, key)| {
            (key, unsafe {
                let ptr = builder.cx.const_gep(ptr, &[builder.cx.const_usize(i)]);
                builder.load(builder.cx.ty_str(), ptr)
            })
        })
        .collect()
}

#[allow(clippy::too_many_arguments)]
unsafe fn map_params<'ll>(
    db: &Snapshot<CompilationDB>,
    model_info: &ModelInfo,
    builder: &mut Builder<'_, '_, 'll>,
    prefix: &str,
    params: &TiSet<CfgParam, ParamKind>,
    offset: &'ll llvm::Value,
    ptr: &'ll llvm::Value,
    ty: &Type,
) -> AHashMap<ParamId, &'ll llvm::Value> {
    let kinds = params.raw.iter().filter_map(|kind| {
        if let ParamKind::Param(param) = kind {
            (&db.param_data(*param).ty == ty).then(|| *param)
        } else {
            None
        }
    });

    let global_name = format!("{}.params.{}", prefix, ty);
    let names = kinds.clone().map(|param| &*model_info.params[&param].name);
    export_names(builder.cx, names, &global_name);
    mapping(kinds, builder, offset, ptr)
}

fn voltage_name(db: &Snapshot<CompilationDB>, hi: NodeId, lo: Option<NodeId>) -> String {
    let mut name = format!("br_{}", &db.node_data(hi).name);
    if let Some(lo) = lo {
        name.push_str(&*db.node_data(lo).name)
    }
    name
}

unsafe fn map_voltages<'ll>(
    db: &Snapshot<CompilationDB>,
    builder: &mut Builder<'_, '_, 'll>,
    prefix: &str,
    params: &TiSet<CfgParam, ParamKind>,
    offset: &'ll llvm::Value,
    ptr: &'ll llvm::Value,
) -> AHashMap<(NodeId, Option<NodeId>), &'ll llvm::Value> {
    let kinds = params.raw.iter().filter_map(|kind| {
        if let ParamKind::Voltage { hi, lo } = kind {
            Some((*hi, *lo))
        } else {
            None
        }
    });

    let global_name = format!("{}.voltages", prefix);
    let names = kinds.clone().map(|(hi, lo)| voltage_name(db, hi, lo));
    export_names(builder.cx, names, &global_name);
    mapping(kinds, builder, offset, ptr)
}

fn current_name(db: &Snapshot<CompilationDB>, kind: CurrentKind) -> String {
    match kind {
        CurrentKind::ExplicitBranch(branch) => db.branch_data(branch).name.deref().to_owned(),
        CurrentKind::ImplictBranch { hi, lo } => {
            let mut name = format!(" {} ", &db.node_data(hi).name);
            if let Some(lo) = lo {
                name.push_str(&*db.node_data(lo).name);
                name.push(' ');
            }
            name
        }
        CurrentKind::Port(port) => format!("< {} >", &db.node_data(port).name),
    }
}

unsafe fn map_currents<'ll>(
    db: &Snapshot<CompilationDB>,
    builder: &mut Builder<'_, '_, 'll>,
    prefix: &str,
    params: &TiSet<CfgParam, ParamKind>,
    offset: &'ll llvm::Value,
    ptr: &'ll llvm::Value,
) -> AHashMap<CurrentKind, &'ll llvm::Value> {
    let kinds = params.raw.iter().filter_map(|kind| {
        if let ParamKind::Current(kind) = kind {
            Some(*kind)
        } else {
            None
        }
    });

    let global_name = format!("{}.currents", prefix);
    let names = kinds.clone().map(|kind| current_name(db, kind));
    export_names(builder.cx, names, &global_name);
    mapping(kinds, builder, offset, ptr)
}

fn export_names<T: Borrow<str>>(
    cx: &mut CodegenCx<'_, '_>,
    names: impl Iterator<Item = T>,
    global_name: &str,
) {
    let names: Vec<_> = names
        .map(|name| {
            let name = name.borrow();
            let name = cx.literals.get(name).unwrap();
            cx.const_str(name)
        })
        .collect();
    cx.export_array(global_name, cx.ty_str(), &names, true, true);
}

fn mapping<'ll, T: Copy + Eq + Hash>(
    fields: impl Iterator<Item = T>,
    builder: &mut Builder<'_, '_, 'll>,
    offset: &'ll llvm::Value,
    ptr: &'ll llvm::Value,
) -> AHashMap<T, &'ll llvm::Value> {
    fields
        .enumerate()
        .map(|(i, key)| (key, unsafe { read_fat_ptr_at(builder, i, offset, ptr) }))
        .collect()
}

unsafe fn read_fat_ptr_at<'ll>(
    builder: &mut Builder<'_, '_, 'll>,
    pos: usize,
    offset: &'ll llvm::Value,
    ptr: &'ll llvm::Value,
) -> &'ll llvm::Value {
    // get correct ptrs from array
    let fat_ptr = builder.gep(ptr, &[builder.cx.const_usize(pos)]);

    let (ptr, meta) = builder.fat_ptr_to_parts(fat_ptr);
    let ptr_ty = builder.cx.elem_ty(builder.cx.val_ty(ptr));

    // get stride or scalar ptr by bitcasting
    let stride_ptr = builder.ptrcast(meta, builder.cx.ptr_ty(builder.cx.ty_isize()));
    let stride = builder.load(builder.cx.ty_isize(), stride_ptr);
    let scalar_ptr = builder.ptrcast(meta, ptr_ty);

    // load the array ptr and check if its a null ptr
    let arr_ptr = builder.load(ptr_ty, ptr);
    let is_arr_null = builder.is_null_ptr(arr_ptr);

    // offset the array _ptr
    let offset = builder.imul(stride, offset);
    let arr_ptr = builder.gep(arr_ptr, &[offset]);

    // if the array_ptr is null select the scalar_ptr otherwise select the arr_ptr
    let ptr = builder.select(is_arr_null, scalar_ptr, arr_ptr);

    //final load
    builder.load(builder.cx.elem_ty(ptr_ty), ptr)
}
