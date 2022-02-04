//! This crate contains a  couple of utilits and tests
//! These are not part of their respective crates
//! to prevent unwanted coupleing

use ahash::AHashMap;
use auto_diff::auto_diff;
use basedb::FileId;
use bitset::BitSet;
use cfg::{Callback, CfgParam, ControlFlowGraph, Operand, Place};
use cfg_opt::{copy_propagation, remove_dead_data, simplify_branches, simplify_cfg};
use codegen_llvm::{Builder, CallbackFun, CodegenCx, LLVMBackend, ModuleLlvm};
use const_eval::conditional_const_propagation;
use hir_def::nameres::ScopeOrigin;
use hir_def::{ModuleId, NodeId, ParamId, Type};
use hir_lower::{CallBackKind, CurrentKind, HirInterner, ParamKind, PlaceKind};
use hir_ty::db::HirTyDB;
use lasso::Rodeo;
use program_dependence::{use_def, AssigmentInterner, ProgramDependenGraph};
use stdx::iter::zip;
use target::spec::Target;
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

#[cfg(test)]
mod tests;

pub fn sim_param_stub<'ll>(cx: &mut CodegenCx<'_, 'll>) -> CallbackFun<'ll> {
    cx.const_callback(&[cx.ty_str()], cx.const_real(0.0))
}

pub fn sim_param_opt_stub<'ll>(cx: &mut CodegenCx<'_, 'll>) -> CallbackFun<'ll> {
    cx.const_return(&[cx.ty_str(), cx.ty_real()], 1)
}

pub fn sim_param_str_stub<'ll>(cx: &mut CodegenCx<'_, 'll>) -> CallbackFun<'ll> {
    let empty_str = cx.literals.get("").unwrap();
    let empty_str = cx.const_str(empty_str);
    let ty_str = cx.ty_str();
    cx.const_callback(&[ty_str], empty_str)
}

pub fn stub_callbacks<'ll>(
    cb: &TiSet<Callback, CallBackKind>,
    cx: &mut CodegenCx<'_, 'll>,
) -> TiVec<Callback, CallbackFun<'ll>> {
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
            CallBackKind::ParamInfo(_, _) => {
                unreachable!("Standard cfg should not check parameters")
            }
        })
        .collect()
}

pub fn callback_sideeffects(cb: &TiSet<Callback, CallBackKind>) -> BitSet<Callback> {
    let mut res = BitSet::new_empty(cb.len());
    for (cb, kind) in cb.iter_enumerated() {
        match kind {
            CallBackKind::SimParam
            | CallBackKind::SimParamOpt
            | CallBackKind::SimParamStr
            | CallBackKind::Derivative(_)
            | CallBackKind::NodeDerivative(_) => (),
            CallBackKind::ParamInfo(_, _) => {
                res.insert(cb);
            }
        }
    }
    res
}

pub fn places<'ll, 'a>(
    db: &dyn HirTyDB,
    builder: &Builder<'_, '_, 'll>,
    places: impl IntoIterator<Item = &'a PlaceKind>,
    mut overwrite: impl FnMut(&PlaceKind) -> Option<(&'ll llvm::Type, &'ll llvm::Value)>,
) -> TiVec<Place, (&'ll llvm::Type, &'ll llvm::Value)> {
    places
        .into_iter()
        .map(|kind| {
            if let Some(overwrite) = overwrite(kind) {
                return overwrite;
            }

            let ty = match kind {
                PlaceKind::Var(var) => lltype(&db.var_data(*var).ty, builder.cx),
                PlaceKind::FunctionReturn { fun } => {
                    lltype(&db.function_data(*fun).return_ty, builder.cx)
                }
                PlaceKind::ImplicitBranchVoltage { .. }
                | PlaceKind::ImplicitBranchCurrent { .. }
                | PlaceKind::Derivative { .. }
                | PlaceKind::BranchCurrent(_)
                | PlaceKind::BranchVoltage(_) => builder.cx.ty_real(),
                PlaceKind::Param(param)
                | PlaceKind::ParamMin(param)
                | PlaceKind::ParamMax(param) => lltype(&db.param_data(*param).ty, builder.cx),
            };

            (ty, unsafe { builder.alloca(ty) })
        })
        .collect()
}

pub fn lltype<'ll>(ty: &Type, cx: &CodegenCx<'_, 'll>) -> &'ll llvm::Type {
    match ty {
        Type::Real => cx.ty_real(),
        Type::Integer => cx.ty_int(),
        Type::String => cx.ty_str(),
        Type::Array { ty, len } => cx.ty_array(lltype(&*ty, cx), *len),
        Type::EmptyArray => cx.zst(),
        Type::Bool => cx.ty_bool(),
        Type::Void => cx.ty_void(),
        Type::Err => unreachable!(),
    }
}

pub fn find_branch_place(places: &TiSet<Place, PlaceKind>, dst: &mut BitSet<Place>) {
    for (place, kind) in places.iter_enumerated() {
        if matches!(
            kind,
            PlaceKind::BranchCurrent(_)
                | PlaceKind::BranchVoltage(_)
                | PlaceKind::ImplicitBranchVoltage { .. }
                | PlaceKind::ImplicitBranchCurrent { .. }
        ) {
            dst.insert(place);
        }
    }
}

pub fn find_module(db: &dyn HirTyDB, root_file: FileId) -> ModuleId {
    let def_map = db.def_map(root_file);
    let root = def_map.entry();
    def_map[root]
        .children
        .values()
        .find_map(|scope| {
            if let ScopeOrigin::Module(module) = def_map[*scope].origin {
                Some(module)
            } else {
                None
            }
        })
        .expect("No Module found")
}

pub fn compile_to_cfg(
    db: &dyn HirTyDB,
    module: ModuleId,
    jacobian: bool,
    opt: bool,
    verify: bool,
    find_output_places: &mut dyn FnMut(&mut TiSet<Place, PlaceKind>, &mut BitSet<Place>),
) -> (ControlFlowGraph, HirInterner, Rodeo) {
    let (mut cfg, mut res, mut literals) = hir_lower::HirInterner::lower_body(db, module.into());
    literals.get_or_intern_static("");
    if verify {
        cfg.assert_verified();
    }

    let sideeffects = callback_sideeffects(&res.callbacks);
    let mut output_places = BitSet::new_empty(cfg.next_place.into());
    find_output_places(&mut res.places, &mut output_places);

    copy_propagation(&mut cfg);
    // eprintln!("{}", cfg.dump(Some(&res.literals)));
    simplify_cfg(&mut cfg);
    if verify {
        cfg.assert_verified();
    }

    cfg.cannonicalize_ret();

    for (param, kind) in res.params.iter_enumerated() {
        if matches!(kind, ParamKind::Voltage { .. }) {
            res.callbacks.ensure(CallBackKind::Derivative(param));
        }
    }

    let assignments = AssigmentInterner::new(&cfg);
    let mut pdg = ProgramDependenGraph::build(&assignments, &cfg);

    // eliminate dead code
    let mut live_code = use_def::DepthFirstSearch::new(&pdg);
    live_code.walk_places::<_, true>(output_places.iter(), &pdg, &cfg);
    live_code.walk_sideffects::<true>(&sideeffects, &pdg, &cfg);
    live_code.remove_unvisited_from_cfg(&mut cfg, &assignments, Some(&mut pdg));

    // live_code.clear();

    let mut dfs = use_def::DepthFirstSearch::new(&pdg);

    let mut matrix_entries: TiSet<CfgParam, (Place, Callback)> = TiSet::default();

    if jacobian {
        for place in output_places.iter() {
            dfs.walk_place::<false>(place, &pdg, &cfg);
            dfs.visited_instructions(&pdg, &cfg, |_, instr| {
                instr.visit_operands(|op| {
                    if let Operand::CfgParam(param) = op {
                        if matches!(
                            res.params[*param],
                            ParamKind::Voltage { .. } | ParamKind::Current(_)
                        ) {
                            let unkown = res.callbacks.ensure(CallBackKind::Derivative(*param)).0;
                            matrix_entries.ensure((place, unkown));
                        }
                    }
                })
            });

            dfs.clear();
        }
    }

    let (_, ad_places, unkowns) = auto_diff(
        &mut cfg,
        &pdg,
        &live_code,
        res.params.len(),
        res.unkowns(),
        matrix_entries.raw.iter().cloned(),
    );

    res.places.raw.extend(ad_places.iter().map(|(original, unkown)| PlaceKind::Derivative {
        original: *original,
        unkown: (*unkown).into(),
    }));

    output_places.ensure(res.places.len());

    if jacobian {
        for (place, kind) in res.places.iter_enumerated() {
            if let PlaceKind::Derivative { original, unkown } = kind {
                if let Some((cb, _)) = unkowns.first_order_unkowns.raw.get_index(*unkown as usize) {
                    if matrix_entries.contains(&(*original, *cb)) {
                        output_places.insert(place);
                    }
                }
            }
        }
    }

    res.insert_var_init(db, &mut literals, &mut cfg);

    copy_propagation(&mut cfg);
    simplify_branches(&mut cfg);
    simplify_cfg(&mut cfg);
    if verify {
        cfg.assert_verified();
    }

    conditional_const_propagation(&mut cfg, &AHashMap::new());
    simplify_branches(&mut cfg);
    simplify_cfg(&mut cfg);
    if verify {
        cfg.assert_verified();
    }

    if opt {
        // let assignments = AssigmentInterner::new(&cfg);
        // let mut pdg = ProgramDependenGraph::build(&assignments, &cfg);

        // // eliminate dead code
        // let mut live_code = use_def::DepthFirstSearch::new(&pdg);
        // live_code.walk_places::<_, true>(output_places.iter(), &pdg, &cfg);
        // live_code.walk_sideffects::<true>(&sideeffects, &pdg, &cfg);
        // live_code.remove_unvisited_from_cfg(&mut cfg, &assignments, Some(&mut pdg));
        // simplify_branches(&mut cfg);
        // simplify_cfg(&mut cfg);
        // if verify {
        //     cfg.assert_verified();
        // }
        let (_, place_map, param_map, callback_map) = remove_dead_data(&mut cfg);
        res.map(&place_map, &param_map, &callback_map);
    }

    (cfg, res, literals)
}

pub fn compile_to_bin(
    db: &dyn HirTyDB,
    name: &str,
    cfg: ControlFlowGraph,
    res: &HirInterner,
    literals: &Rodeo,
    ret: (&Type, &Operand),
) -> ModuleLlvm {
    let target = Target::host_target().unwrap();
    let backend = LLVMBackend::new(&[], &target, llvm::OptLevel::Aggressive);

    let module = unsafe { backend.new_module(name).unwrap() };
    let mut cx = unsafe { backend.new_ctx(literals, &module) };
    let param_struct = ParamStruct::new(db, &cx, &format!("{}Params", name), &res.params);
    let current_struct = CurrentStruct::new(&cx, &res.params);
    let voltage_struct = VoltagesStruct::new(&cx, &res.params);

    let args = [
        cx.ty_real(),
        cx.ptr_ty(param_struct.llty),
        cx.ptr_ty(voltage_struct.llty),
        cx.ptr_ty(current_struct.llty),
    ];
    let fun_ty = cx.ty_func(&args, lltype(ret.0, &cx));
    let fun = cx.declare_ext_fn(name, fun_ty);

    let true_val = cx.const_bool(true);
    let mut builder = Builder::new(&mut cx, &cfg, fun);
    builder.places = places(db, &builder, res.places.raw.iter(), |_| None);
    builder.callbacks = stub_callbacks(&res.callbacks, builder.cx);

    let params = unsafe { llvm::LLVMGetParam(fun, 1) };
    let params = param_struct.mapping(params, &builder);
    let voltages = unsafe { llvm::LLVMGetParam(fun, 2) };
    let voltages = voltage_struct.mapping(voltages, &builder);
    let currents = unsafe { llvm::LLVMGetParam(fun, 3) };
    let currents = current_struct.mapping(currents, &builder);
    builder.params = res
        .params
        .raw
        .iter()
        .map(|kind| match kind {
            ParamKind::Param(param) => params[param],
            ParamKind::Voltage { hi, lo } => voltages[&(*hi, *lo)],
            ParamKind::Current(current) => currents[current],
            ParamKind::Temperature => unsafe { llvm::LLVMGetParam(fun, 0) },
            ParamKind::ParamGiven { .. } | ParamKind::PortConnected { .. } => true_val,
            ParamKind::ParamSysFun(param) => builder.cx.const_real(param.default_value()),
        })
        .collect();

    unsafe {
        builder.build_cfg();
        builder.build_returns(|builder, _| {
            let val = builder.operand(ret.1);
            builder.ret(val)
        })
    }

    drop(builder);
    assert!(module.verify_and_print(), "Invalid code generated");
    module
}

pub struct ParamStruct<'ll> {
    llty: &'ll llvm::Type,
    fields: Box<[ParamId]>,
    field_tys: Box<[&'ll llvm::Type]>,
}

impl<'ll> ParamStruct<'ll> {
    pub fn new(
        db: &dyn HirTyDB,
        cx: &CodegenCx<'_, 'll>,
        name: &str,
        params: &TiSet<CfgParam, ParamKind>,
    ) -> Self {
        let fields: Box<[_]> = params
            .raw
            .iter()
            .filter_map(|kind| if let ParamKind::Param(param) = kind { Some(*param) } else { None })
            .collect();
        let (llty, field_tys) = Self::llty(db, cx, name, &fields);
        Self { llty, fields, field_tys }
    }

    pub fn llty(
        db: &dyn HirTyDB,
        cx: &CodegenCx<'_, 'll>,
        name: &str,
        params: &[ParamId],
    ) -> (&'ll llvm::Type, Box<[&'ll llvm::Type]>) {
        let tys: Box<[_]> =
            params.iter().map(|param| lltype(&db.param_data(*param).ty, cx)).collect();
        (cx.struct_ty(name, &tys), tys)
    }

    pub fn mapping(
        &self,
        ptr: &'ll llvm::Value,
        builder: &Builder<'_, '_, 'll>,
    ) -> AHashMap<ParamId, &'ll llvm::Value> {
        // let fields: Vec<_> = unsafe {
        //     let fields =
        //         llvm::struct_element_types(llvm::LLVMGetElementType(llvm::LLVMTypeOf(ptr)));
        //     fields.iter().map(|ty| llvm::LLVMGetTypeKind(ty)).collect()
        // };
        // eprintln!("{:?}", fields);

        // let fields: Vec<_> =
        //     unsafe { self.field_tys.iter().map(|ty| llvm::LLVMGetTypeKind(ty)).collect() };
        // eprintln!("{:?}", fields);
        zip(&*self.fields, &*self.field_tys)
            .enumerate()
            .map(|(i, (param, ty))| {
                (*param, unsafe {
                    let ptr = builder.struct_gep(ptr, i as u32);
                    builder.load(ty, ptr)
                })
            })
            .collect()
    }
}

pub struct VoltagesStruct<'ll> {
    llty: &'ll llvm::Type,
    fields: Box<[(NodeId, Option<NodeId>)]>,
}

impl<'ll> VoltagesStruct<'ll> {
    pub fn new(cx: &CodegenCx<'_, 'll>, params: &TiSet<CfgParam, ParamKind>) -> Self {
        let fields: Box<[_]> = params
            .raw
            .iter()
            .filter_map(|kind| {
                if let ParamKind::Voltage { hi, lo } = kind {
                    Some((*hi, *lo))
                } else {
                    None
                }
            })
            .collect();
        let llty = cx.ty_array(cx.ty_real(), fields.len() as u32);
        Self { llty, fields }
    }
    pub fn mapping(
        &self,
        ptr: &'ll llvm::Value,
        builder: &Builder<'_, '_, 'll>,
    ) -> AHashMap<(NodeId, Option<NodeId>), &'ll llvm::Value> {
        self.fields
            .iter()
            .enumerate()
            .map(|(i, voltage)| {
                let val = unsafe {
                    let ptr =
                        builder.gep(ptr, &[builder.cx.const_usize(0), builder.cx.const_usize(i)]);
                    builder.load(builder.cx.ty_real(), ptr)
                };
                (*voltage, val)
            })
            .collect()
    }
}

pub struct CurrentStruct<'ll> {
    llty: &'ll llvm::Type,
    fields: Box<[CurrentKind]>,
}

impl<'ll> CurrentStruct<'ll> {
    pub fn new(cx: &CodegenCx<'_, 'll>, params: &TiSet<CfgParam, ParamKind>) -> Self {
        let fields: Box<[_]> = params
            .raw
            .iter()
            .filter_map(|kind| if let ParamKind::Current(kind) = kind { Some(*kind) } else { None })
            .collect();
        let llty = cx.ty_array(cx.ty_real(), fields.len() as u32);
        Self { llty, fields }
    }
    pub fn mapping(
        &self,
        ptr: &'ll llvm::Value,
        builder: &Builder<'_, '_, 'll>,
    ) -> AHashMap<CurrentKind, &'ll llvm::Value> {
        self.fields
            .iter()
            .enumerate()
            .map(|(i, current)| {
                (*current, unsafe {
                    let ptr =
                        builder.gep(ptr, &[builder.cx.const_usize(0), builder.cx.const_usize(i)]);
                    builder.load(builder.cx.ty_real(), ptr)
                })
            })
            .collect()
    }
}
