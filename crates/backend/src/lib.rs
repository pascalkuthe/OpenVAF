//! This crate contains a  couple of utilits and tests
//! These are not part of their respective crates
//! to prevent unwanted coupleing

use ahash::AHashMap;
use auto_diff::auto_diff;
use basedb::FileId;
use bitset::BitSet;
use cfg::{Callback, CfgParam, ControlFlowGraph, Operand, Place};
use cfg_opt::{
    copy_propagation, dead_code_elimination, remove_dead_data, simplify_branches, simplify_cfg,
};
use codegen_llvm::{Builder, CallbackFun, CodegenCx, LLVMBackend, ModuleLlvm};
use const_eval::conditional_const_propagation;
use hir_def::nameres::ScopeOrigin;
use hir_def::{NodeId, ParamId, ParamSysFun, Type};
use hir_lower::{CallBackKind, CurrentKind, HirInterner, ParamKind, PlaceKind};
use hir_ty::db::HirTyDB;
use stdx::iter::zip;
use syntax::name::Name;
use target::spec::Target;
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

#[cfg(test)]
mod tests;

pub fn stub_callbacks<'ll>(
    cb: &TiSet<Callback, CallBackKind>,
    cx: &mut CodegenCx<'_, 'll>,
) -> TiVec<Callback, CallbackFun<'ll>> {
    cb.raw
        .iter()
        .map(|kind| match kind {
            CallBackKind::SimParam => cx.const_callback(&[cx.ty_str()], cx.const_real(0.0)),
            CallBackKind::SimParamOpt => cx.const_return(&[cx.ty_str(), cx.ty_real()], 1),
            CallBackKind::SimParamStr => {
                let empty_str = cx.literals.get_or_intern_static("");
                let empty_str = cx.const_str(empty_str);
                let ty_str = cx.ty_str();
                cx.const_callback(&[ty_str], empty_str)
            }
            CallBackKind::Derivative(_) => unreachable!("Derivative must not remain in final CFG!"),
            CallBackKind::NodeDerivative(_) => {
                unreachable!("Derivative must not remain in final CFG!")
            }
        })
        .collect()
}

pub fn callback_sideeffects(cb: &TiSet<Callback, CallBackKind>) -> BitSet<Callback> {
    let res = BitSet::new_empty(cb.len());
    for (_cb, kind) in cb.iter_enumerated() {
        match kind {
            CallBackKind::SimParam
            | CallBackKind::SimParamOpt
            | CallBackKind::SimParamStr
            | CallBackKind::Derivative(_)
            | CallBackKind::NodeDerivative(_) => (),
        }
    }
    res
}

pub fn places<'ll, 'a>(
    db: &dyn HirTyDB,
    builder: &Builder<'_, '_, 'll>,
    places: impl IntoIterator<Item = &'a PlaceKind>,
) -> TiVec<Place, (&'ll llvm::Type, &'ll llvm::Value)> {
    places
        .into_iter()
        .map(|kind| {
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
                PlaceKind::Param(param) => lltype(&db.param_data(*param).ty, builder.cx),
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

pub fn compile_to_cfg(
    db: &dyn HirTyDB,
    root_file: FileId,
) -> (Name, ControlFlowGraph, HirInterner) {
    let def_map = db.def_map(root_file);
    let root = def_map.entry();
    let (module_name, module) = def_map[root]
        .children
        .iter()
        .find_map(|(name, scope)| {
            if let ScopeOrigin::Module(module) = def_map[*scope].origin {
                Some((name.clone(), module))
            } else {
                None
            }
        })
        .expect("No Module found");
    let (mut cfg, mut res) = hir_lower::HirInterner::lower_body(db, module.into());
    cfg.assert_verified(&res.literals);

    let sideeffects = callback_sideeffects(&res.callbacks);
    let mut output_places = BitSet::new_empty(cfg.next_place.into());

    for (place, kind) in res.places.iter_enumerated() {
        if matches!(
            kind,
            PlaceKind::BranchCurrent(_)
                | PlaceKind::BranchVoltage(_)
                | PlaceKind::ImplicitBranchVoltage { .. }
                | PlaceKind::ImplicitBranchCurrent { .. }
                | PlaceKind::Var(_)
        ) {
            output_places.insert(place);
        }
    }

    // copy_propagation(&mut cfg);
    // eprintln!("{}", cfg.dump(Some(&res.literals)));
    // dead_code_elimination(&mut cfg, &output_places, &sideeffects);
    simplify_branches(&mut cfg);
    simplify_cfg(&mut cfg);
    cfg.assert_verified(&res.literals);

    for (param, kind) in res.params.iter_enumerated() {
        if matches!(kind, ParamKind::Voltage { .. }) {
            res.callbacks.ensure(CallBackKind::Derivative(param));
        }
    }

    let (_, ad_places) = auto_diff(&mut cfg, res.unkowns(), None);
    res.places.raw.extend(ad_places.iter().map(|(original, unkown)| PlaceKind::Derivative {
        original: *original,
        unkown: (*unkown).into(),
    }));
    output_places.ensure_enabled(res.places.len());

    // copy_propagation(&mut cfg);
    // dead_code_elimination(&mut cfg, &output_places, &sideeffects);
    simplify_branches(&mut cfg);
    simplify_cfg(&mut cfg);
    cfg.assert_verified(&res.literals);

    // eprintln!("{}", cfg.dump(&res.literals));
    conditional_const_propagation(&mut cfg, &AHashMap::new());
    cfg.assert_verified(&res.literals);
    simplify_branches(&mut cfg);
    simplify_cfg(&mut cfg);
    cfg.assert_verified(&res.literals);
    let (_, place_map, param_map, callback_map) = remove_dead_data(&mut cfg);
    res.map(&place_map, &param_map, &callback_map);

    (module_name, cfg, res)
}

pub fn compile_to_bin(
    db: &dyn HirTyDB,
    name: &str,
    cfg: ControlFlowGraph,
    mut res: HirInterner,
    ret: (&Type, &Operand),
) -> ModuleLlvm {
    let target = Target::host_target().unwrap();
    let backend = LLVMBackend::new(&[], &target, llvm::CodeGenOptLevel::Aggressive);
    let module = unsafe { backend.new_module(name).unwrap() };
    let mut cx = unsafe { backend.new_ctx(&mut res.literals, &module) };
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
    builder.places = places(db, &builder, res.places.raw.iter());
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
            ParamKind::ParamSysFun(ParamSysFun::mfactor) => builder.cx.const_real(1.0),
            ParamKind::ParamSysFun(_) => todo!(),
        })
        .collect();

    unsafe {
        builder.build_cfg();
        builder.build_returns(|builder, _| builder.operand(ret.1))
    }

    drop(builder);
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
