use std::borrow::Borrow;
use std::path::Path;

use hir_def::db::HirDefDB;
use hir_def::Type;
use hir_lower::{CallBackKind, CurrentKind, HirInterner, ParamInfoKind, ParamKind, PlaceKind};
use lasso::Rodeo;
use llvm::UNNAMED;
use mir::{ControlFlowGraph, FuncRef, Function};
use mir_llvm::{Builder, CallbackFun, CodegenCx, LLVMBackend};
use stdx::iter::multiunzip;
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::compiler_db::{CompilationDB, FuncSpec, InternedModel, ModelInfo};

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

pub fn lltype<'ll>(ty: &Type, cx: &CodegenCx<'_, 'll>) -> &'ll llvm::Type {
    match ty {
        Type::Real => cx.ty_real(),
        Type::Integer => cx.ty_int(),
        Type::String => cx.ty_str(),
        Type::Array { ty, len } => cx.ty_array(lltype(&*ty, cx), *len),
        Type::EmptyArray => cx.ty_array(cx.ty_int(), 0),
        Type::Bool => cx.ty_bool(),
        Type::Void => cx.ty_void(),
        Type::Err => unreachable!(),
    }
}

pub fn stub_callbacks<'ll>(
    cb: &TiSet<FuncRef, CallBackKind>,
    cx: &mut CodegenCx<'_, 'll>,
    // invalid_param_dst: &AHashMap<ParamId, &'ll Value>,
) -> TiVec<FuncRef, Option<CallbackFun<'ll>>> {
    cb.raw
        .iter()
        .map(|kind| {
            let res = match kind {
                CallBackKind::SimParam => sim_param_stub(cx),
                CallBackKind::SimParamOpt => sim_param_opt_stub(cx),
                CallBackKind::SimParamStr => sim_param_str_stub(cx),
                CallBackKind::Derivative(_) | CallBackKind::NodeDerivative(_) => {
                    cx.const_callback(&[cx.ty_real()], cx.const_real(0.0))
                }
                CallBackKind::ParamInfo(_, _) | CallBackKind::CollapseHint(_, _) => return None,
            };

            Some(res)
        })
        .collect()
}

pub struct CodegenCtx<'a, 't> {
    pub model_info: &'a ModelInfo,
    pub llbackend: &'a LLVMBackend<'t>,
    pub literals: &'a mut Rodeo,
}

struct Codegen<'a, 'b, 'll> {
    db: &'a CompilationDB,
    model_info: &'a ModelInfo,
    intern: &'a HirInterner,
    builder: &'b mut Builder<'a, 'a, 'll>,
    func: &'a Function,
    spec: &'a FuncSpec,
}

impl<'ll> Codegen<'_, '_, 'll> {
    unsafe fn read_depbreak(&mut self, offset: &'ll llvm::Value, ptr: &'ll llvm::Value, ty: Type) {
        let vars = self
            .spec
            .dependency_breaking
            .iter()
            .copied()
            .filter(|var| self.db.var_data(*var).ty == ty);

        for (i, var) in vars.clone().enumerate() {
            if let Some(id) = self.intern.params.index(&ParamKind::HiddenState(var)) {
                self.builder.params[id] = Some(self.read_fat_ptr_at(i, offset, ptr));
            }
        }

        let global_name = format!("{}.depbreak.{}", self.spec.prefix, ty);
        let names = vars.clone().map(|var| &*self.model_info.var_names[&var]);
        self.export_names(names, &global_name);
    }

    unsafe fn read_str_params(&mut self, ptr: &'ll llvm::Value) {
        let params = self.intern.live_params(&self.func.dfg).filter_map(|(id, kind, _)| {
            if let ParamKind::Param(param) = *kind {
                (self.db.param_data(param).ty == Type::String).then(|| (id, param))
            } else {
                None
            }
        });

        for (i, (id, _)) in params.clone().enumerate() {
            let ptr = self.builder.cx.const_gep(ptr, &[self.builder.cx.const_usize(i)]);
            self.builder.params[id] = Some(self.builder.load(self.builder.cx.ty_str(), ptr));
        }

        let global_name = format!("{}.params.{}", self.spec.prefix, Type::String);
        let names = params.map(|(_, param)| &*self.model_info.params[&param].name);
        self.export_names(names, &global_name);
    }

    unsafe fn read_params(&mut self, offset: &'ll llvm::Value, ptr: &'ll llvm::Value, ty: Type) {
        let params = self.intern.live_params(&self.func.dfg).filter_map(|(id, kind, _)| {
            if let ParamKind::Param(param) = kind {
                (self.db.param_data(*param).ty == ty).then(|| (id, *param))
            } else {
                None
            }
        });

        for (i, (id, _)) in params.clone().enumerate() {
            self.builder.params[id] = Some(self.read_fat_ptr_at(i, offset, ptr));
        }

        let global_name = format!("{}.params.{}", self.spec.prefix, ty);
        let names = params.clone().map(|(_, param)| &*self.model_info.params[&param].name);
        self.export_names(names, &global_name);
    }

    unsafe fn read_voltages(&mut self, offset: &'ll llvm::Value, ptr: &'ll llvm::Value) {
        let voltages = self.intern.live_params(&self.func.dfg).filter_map(|(id, kind, _)| {
            if let ParamKind::Voltage { hi, lo } = kind {
                Some((id, (*hi, *lo)))
            } else {
                None
            }
        });

        let default_val = |(id, volt)| {
            self.model_info
                .optional_voltages
                .get(&volt)
                .map(|val| ((id, volt), self.builder.cx.const_real(*val)))
        };

        let (optional_voltages, default_vals): (Vec<_>, Vec<_>) =
            voltages.clone().filter_map(default_val).unzip();

        let non_optional_voltages: Vec<_> =
            voltages.filter(|kind| default_val(*kind).is_none()).collect();

        let voltages = optional_voltages.into_iter().chain(non_optional_voltages);

        for (i, (id, _)) in voltages.clone().enumerate() {
            self.builder.params[id] = Some(self.read_fat_ptr_at(i, offset, ptr));
        }

        let global_name = format!("{}.voltages.default", self.spec.prefix);
        self.builder.cx.export_array(
            &global_name,
            self.builder.cx.ty_real(),
            &default_vals,
            true,
            true,
        );

        let global_name = format!("{}.voltages", self.spec.prefix);
        let names = voltages.map(|(_, (hi, lo))| self.db.voltage_name(hi, lo));
        self.export_names(names, &global_name);
    }

    unsafe fn read_currents(&mut self, offset: &'ll llvm::Value, ptr: &'ll llvm::Value) {
        let voltages = self.intern.live_params(&self.func.dfg).filter_map(|(id, kind, _)| {
            if let ParamKind::Current(kind) = kind {
                Some((id, *kind))
            } else {
                None
            }
        });

        let default_val = |(id, kind)| {
            if let CurrentKind::ExplicitBranch(branch) = kind {
                if let Some(val) = self.model_info.optional_currents.get(&branch) {
                    return Some(((id, kind), self.builder.cx.const_real(*val)));
                }
            }
            None
        };

        let (optional_voltages, default_vals): (Vec<_>, Vec<_>) =
            voltages.clone().filter_map(default_val).unzip();

        let non_optional_voltages: Vec<_> =
            voltages.filter(|kind| default_val(*kind).is_none()).collect();

        let voltages = optional_voltages.into_iter().chain(non_optional_voltages);

        for (i, (id, _)) in voltages.clone().enumerate() {
            self.builder.params[id] = Some(self.read_fat_ptr_at(i, offset, ptr));
        }

        let global_name = format!("{}.currents.default", self.spec.prefix);
        self.builder.cx.export_array(
            &global_name,
            self.builder.cx.ty_real(),
            &default_vals,
            true,
            true,
        );

        let global_name = format!("{}.currents", self.spec.prefix);
        let names = voltages.map(|(_, kind)| self.db.current_name(kind));
        self.export_names(names, &global_name);
    }

    fn export_names<T: Borrow<str>>(&mut self, names: impl Iterator<Item = T>, global_name: &str) {
        let cx = &mut self.builder.cx;
        let names: Vec<_> = names
            .map(|name| {
                let name = name.borrow();
                let name = cx.literals.get(name).unwrap();
                cx.const_str(name)
            })
            .collect();
        cx.export_array(global_name, cx.ty_str(), &names, true, true);
    }

    unsafe fn read_fat_ptr_at(
        &mut self,
        pos: usize,
        offset: &'ll llvm::Value,
        ptr: &'ll llvm::Value,
    ) -> &'ll llvm::Value {
        let builder = &mut self.builder;

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
}

impl CodegenCtx<'_, '_> {
    pub(crate) fn gen_func_obj(
        &self,
        db: &CompilationDB,
        spec: &FuncSpec,
        func: &Function,
        cfg: &ControlFlowGraph,
        intern: &HirInterner,
        dst: &Path,
    ) {
        let ret_info = db.var_data(spec.var);

        let module = unsafe { self.llbackend.new_module(&*ret_info.name).unwrap() };
        let mut cx = unsafe { self.llbackend.new_ctx(self.literals, &module) };

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
        let llfun = cx.declare_ext_fn(&spec.prefix, fun_ty);

        // setup builder
        let mut builder = Builder::new(&mut cx, func, llfun);

        let mut codegen = Codegen {
            db: &*db,
            model_info: &*self.model_info,
            intern: &*intern,
            builder: &mut builder,
            func: &*func,
            spec: &*spec,
        };

        // read parameters

        let offset = unsafe { llvm::LLVMGetParam(llfun, 0) };

        let true_val = codegen.builder.cx.const_bool(true);
        codegen.builder.params = intern
            .params
            .raw
            .iter()
            .map(|(kind, val)| {
                if func.dfg.value_dead(*val) {
                    return None;
                }

                let val = match kind {
                    ParamKind::Param(_)
                    | ParamKind::Voltage { .. }
                    | ParamKind::Current(_)
                    | ParamKind::HiddenState(_) => return None,
                    ParamKind::Temperature => unsafe {
                        let temperature = llvm::LLVMGetParam(llfun, 8);
                        codegen.read_fat_ptr_at(0, offset, temperature)
                    },
                    ParamKind::ParamGiven { .. } | ParamKind::PortConnected { .. } => true_val,
                    ParamKind::ParamSysFun(param) => {
                        codegen.builder.cx.const_real(param.default_value())
                    }
                };

                Some(val)
            })
            .collect();

        let voltages = unsafe { llvm::LLVMGetParam(llfun, 1) };
        unsafe { codegen.read_voltages(offset, voltages) };

        let currents = unsafe { llvm::LLVMGetParam(llfun, 2) };
        unsafe { codegen.read_currents(offset, currents) };

        let real_params = unsafe { llvm::LLVMGetParam(llfun, 3) };
        unsafe { codegen.read_params(offset, real_params, Type::Real) };

        let int_params = unsafe { llvm::LLVMGetParam(llfun, 4) };
        unsafe { codegen.read_params(offset, int_params, Type::Integer) };

        let str_params = unsafe { llvm::LLVMGetParam(llfun, 5) };
        unsafe { codegen.read_str_params(str_params) };

        let real_dep_break = unsafe { llvm::LLVMGetParam(llfun, 6) };
        unsafe { codegen.read_depbreak(offset, real_dep_break, Type::Real) };

        let int_dep_break = unsafe { llvm::LLVMGetParam(llfun, 7) };
        unsafe { codegen.read_depbreak(offset, int_dep_break, Type::Integer) };

        // setup callbacks

        codegen.builder.callbacks = stub_callbacks(&intern.callbacks, codegen.builder.cx);
        drop(codegen);

        let postorder: Vec<_> = cfg.postorder(func).collect();

        let exit_bb = *postorder
            .iter()
            .find(|bb| {
                func.layout
                    .last_inst(**bb)
                    .map_or(true, |term| !func.dfg.insts[term].is_terminator())
            })
            .unwrap();

        unsafe {
            // the actual compiled function
            builder.build_consts();
            builder.build_cfg(&postorder);

            // write the return value
            builder.select_bb(exit_bb);

            let out = llvm::LLVMGetParam(llfun, 9);
            let out = builder.gep(out, &[offset]);

            let ret_val = intern.outputs[&PlaceKind::Var(spec.var)].unwrap();
            let ret_val = builder.values[ret_val].unwrap();

            builder.store(out, ret_val);

            builder.ret_void();
        }

        // build object file
        drop(builder);
        debug_assert!(module.verify_and_print(), "Invalid code generated");
        module.optimize(self.llbackend);

        module.emit_obect(dst).expect("code generation failed!")
    }

    pub(crate) fn ensure_names(&mut self, db: &CompilationDB, intern: &HirInterner) {
        for param in &intern.params.raw {
            match param.0 {
                ParamKind::Voltage { hi, lo } => {
                    self.literals.get_or_intern(&db.voltage_name(*hi, *lo));
                }
                ParamKind::Current(kind) => {
                    self.literals.get_or_intern(&db.current_name(*kind));
                }
                _ => (),
            }
        }

        for func in &self.model_info.functions {
            for dep in &*func.dependency_breaking {
                self.literals.get_or_intern(&*self.model_info.var_names[dep]);
            }
        }
    }

    fn read_params<'ll>(
        &self,
        intern: &HirInterner,
        ty: Type,
        builder: &mut Builder<'_, '_, 'll>,
        val_ptr: &'ll llvm::Value,
        param_given_ptr: &'ll llvm::Value,
        param_given_offset: usize,
    ) -> usize {
        let llty = lltype(&ty, builder.cx);
        let mut offset = 0;
        for (param, info) in self.model_info.params.iter() {
            if info.ty != ty {
                continue;
            }

            let given_id = intern.params.unwrap_index(&ParamKind::ParamGiven { param: *param });
            let given = unsafe {
                let off = builder.cx.const_usize(param_given_offset + offset);
                let ptr = builder.gep(param_given_ptr, &[off]);
                let cbool = builder.load(builder.cx.ty_c_bool(), ptr);
                builder.int_cmp(cbool, builder.cx.const_c_bool(false), llvm::IntPredicate::IntNE)
            };

            let val_id = intern.params.unwrap_index(&ParamKind::Param(*param));
            let val = unsafe {
                let off = builder.cx.const_usize(offset);
                let ptr = builder.gep(val_ptr, &[off]);
                builder.load(llty, ptr)
            };

            builder.params[given_id] = Some(given);
            builder.params[val_id] = Some(val);
            offset += 1;
        }
        offset
    }

    fn write_params<'ll>(
        &self,
        intern: &HirInterner,
        ty: Type,
        builder: &Builder<'_, '_, 'll>,
        val_ptr: &'ll llvm::Value,
        bounds_ptrs: Option<(&'ll llvm::Value, &'ll llvm::Value)>,
    ) {
        for (i, (param, _)) in
            self.model_info.params.iter().filter(|(_, info)| info.ty == ty).enumerate()
        {
            let param_val = intern.outputs[&PlaceKind::Param(*param)].unwrap();
            let param_val = builder.values[param_val].unwrap();

            unsafe {
                let off = builder.cx.const_usize(i);
                let ptr = builder.gep(val_ptr, &[off]);
                builder.store(ptr, param_val)
            };

            if let Some((min_ptr, max_ptr)) = bounds_ptrs {
                unsafe {
                    let param_min = intern.outputs[&PlaceKind::ParamMin(*param)].unwrap();
                    let param_min = builder.values[param_min].unwrap();
                    let off = builder.cx.const_usize(i);
                    let ptr = builder.gep(min_ptr, &[off]);
                    builder.store(ptr, param_min)
                }

                unsafe {
                    let param_max = intern.outputs[&PlaceKind::ParamMax(*param)].unwrap();
                    let param_max = builder.values[param_max].unwrap();
                    let off = builder.cx.const_usize(i);
                    let ptr = builder.gep(max_ptr, &[off]);
                    builder.store(ptr, param_max)
                }
            }
        }
    }

    fn param_flag_cb<'ll>(
        &self,
        cx: &mut mir_llvm::CodegenCx<'_, 'll>,
        set: bool,
    ) -> (&'ll llvm::Value, &'ll llvm::Type) {
        let name = cx.local_callback_name();
        let fun_ty = cx.ty_func(&[cx.ptr_ty(cx.ty_c_bool()), cx.ty_c_bool()], cx.ty_void());
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

        (fun, fun_ty)
    }

    fn insert_param_info_callbacks<'ll>(
        &self,
        intern: &HirInterner,
        builder: &mut Builder<'_, '_, 'll>,
        param_flags: &'ll llvm::Value,
        real_cnt: usize,
        int_cnt: usize,
    ) {
        let mut real_off = 0;
        let mut int_off = real_cnt;
        let mut str_off = int_off + int_cnt;

        let param_info_set_cb = self.param_flag_cb(builder.cx, true);
        let param_info_unset_cb = self.param_flag_cb(builder.cx, false);
        for (param, info) in self.model_info.params.iter() {
            let off = match info.ty {
                Type::Real => &mut real_off,
                Type::Integer => &mut int_off,
                Type::String => &mut str_off,
                _ => unreachable!(),
            };

            let dst = unsafe {
                let off = builder.cx.const_usize(*off);
                builder.gep(param_flags, &[off])
            };

            *off += 1;

            for (kind, set, mut bits) in [
                (ParamInfoKind::Invalid, true, 0b100),
                (ParamInfoKind::MinInclusive, true, 0b001),
                (ParamInfoKind::MaxInclusive, true, 0b010),
                (ParamInfoKind::MinExclusive, false, 0b001),
                (ParamInfoKind::MaxExclusive, false, 0b010),
            ] {
                let (fun, fun_ty) = if set {
                    param_info_set_cb
                } else {
                    bits = !bits;
                    param_info_unset_cb
                };

                let res = CallbackFun {
                    fun_ty,
                    fun,
                    state: vec![dst, builder.cx.const_u8(bits)].into_boxed_slice(),
                };

                let cb = intern.callbacks.unwrap_index(&CallBackKind::ParamInfo(kind, *param));
                builder.callbacks[cb] = Some(res)
            }
        }

        assert_eq!(real_off, real_cnt);
        assert_eq!(int_off, real_cnt + int_cnt);
    }

    pub(crate) fn compile_model_info(
        &self,
        dst: &Path,
        interned_model: InternedModel,
        param_init_func: Function,
        param_init_intern: HirInterner,
    ) {
        let module = unsafe { self.llbackend.new_module("model_info").unwrap() };
        let mut cx = unsafe { self.llbackend.new_ctx(self.literals, &module) };

        let (fun_names, fun_symbols) = interned_model.functions(&mut cx);
        cx.export_array("functions", cx.ty_str(), &fun_names, true, true);
        cx.export_array("functions.sym", cx.ty_str(), &fun_symbols, true, false);

        let op_vars = interned_model.opvars(&mut cx);
        cx.export_array("opvars", cx.ty_str(), &op_vars, true, true);

        let nodes = interned_model.nodes(&mut cx);
        cx.export_array("nodes", cx.ty_str(), &nodes, true, true);

        let module_name = cx.const_str(interned_model.module_name);
        cx.export_val("module_name", cx.ty_str(), module_name, true);

        interned_model.export_param_info(&mut cx, Type::Real);
        interned_model.export_param_info(&mut cx, Type::Integer);
        interned_model.export_param_info(&mut cx, Type::String);

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

        let mut builder = Builder::new(&mut cx, &param_init_func, llfun);

        // read parameters

        builder.params = param_init_intern
            .params
            .raw
            .iter()
            .map(|(kind, val)| {
                if param_init_func.dfg.value_dead(*val) {
                    return None;
                }
                let val = match kind {
                    ParamKind::Voltage { .. }
                    | ParamKind::Current(_)
                    | ParamKind::HiddenState(_) => {
                        unreachable!()
                    }
                    ParamKind::Param(_) | ParamKind::ParamGiven { .. } => return None,
                    ParamKind::Temperature => builder.cx.const_real(293f64),
                    ParamKind::PortConnected { .. } => builder.cx.const_bool(true),
                    ParamKind::ParamSysFun(param) => builder.cx.const_real(param.default_value()),
                };

                Some(val)
            })
            .collect();

        let param_flags = unsafe { llvm::LLVMGetParam(llfun, 7) };

        let param_val_real = unsafe { llvm::LLVMGetParam(llfun, 0) };
        let real_cnt = self.read_params(
            &param_init_intern,
            Type::Real,
            &mut builder,
            param_val_real,
            param_flags,
            0,
        );

        let param_val_int = unsafe { llvm::LLVMGetParam(llfun, 1) };
        let int_cnt = self.read_params(
            &param_init_intern,
            Type::Integer,
            &mut builder,
            param_val_int,
            param_flags,
            real_cnt,
        );

        let param_val_str = unsafe { llvm::LLVMGetParam(llfun, 2) };
        self.read_params(
            &param_init_intern,
            Type::String,
            &mut builder,
            param_val_str,
            param_flags,
            int_cnt + real_cnt,
        );

        // insert callbacks

        builder.callbacks = stub_callbacks(&param_init_intern.callbacks, builder.cx);
        self.insert_param_info_callbacks(
            &param_init_intern,
            &mut builder,
            param_flags,
            real_cnt,
            int_cnt,
        );

        let postorder: Vec<_> = {
            let mut cfg = ControlFlowGraph::new();
            cfg.compute(&param_init_func);
            cfg.postorder(&param_init_func).collect()
        };

        unsafe {
            // the actual compiled function
            builder.build_consts();
            builder.build_cfg(&postorder);

            // write the return values
            builder.select_bb(postorder[0]);

            let param_min_real = llvm::LLVMGetParam(llfun, 3);
            let param_max_real = llvm::LLVMGetParam(llfun, 5);
            self.write_params(
                &param_init_intern,
                Type::Real,
                &builder,
                param_val_real,
                Some((param_min_real, param_max_real)),
            );

            let param_min_int = llvm::LLVMGetParam(llfun, 4);
            let param_max_int = llvm::LLVMGetParam(llfun, 6);
            self.write_params(
                &param_init_intern,
                Type::Integer,
                &builder,
                param_val_int,
                Some((param_min_int, param_max_int)),
            );

            self.write_params(&param_init_intern, Type::String, &builder, param_val_str, None);

            builder.ret_void();
        }

        debug_assert!(module.verify_and_print(), "generated invalid code");
        module.optimize(self.llbackend);

        module.emit_obect(dst).expect("code generation failed!");
    }
}

impl InternedModel<'_> {
    fn functions<'ll>(
        &self,
        cx: &mut mir_llvm::CodegenCx<'_, 'll>,
    ) -> (Vec<&'ll llvm::Value>, Vec<&'ll llvm::Value>) {
        self.functions
            .iter()
            .map(|func| (cx.const_str(func.name), cx.const_str(func.prefix)))
            .unzip()
    }

    fn opvars<'ll>(&self, cx: &mut mir_llvm::CodegenCx<'_, 'll>) -> Vec<&'ll llvm::Value> {
        self.opvars.iter().map(|name| cx.const_str(*name)).collect()
    }

    fn nodes<'ll>(&self, cx: &mut mir_llvm::CodegenCx<'_, 'll>) -> Vec<&'ll llvm::Value> {
        self.nodes.iter().map(|name| cx.const_str(*name)).collect()
    }

    fn param_info<'ll>(&self, cx: &mut mir_llvm::CodegenCx<'_, 'll>, ty: &Type) -> ParamInfo<'ll> {
        let iter = self.params.iter().filter_map(|param| {
            if ty == param.ty {
                Some((
                    cx.const_str(param.name),
                    cx.const_str(param.unit),
                    cx.const_str(param.description),
                    cx.const_str(param.group),
                ))
            } else {
                None
            }
        });
        let (names, units, descriptions, groups) = multiunzip(iter);
        ParamInfo { units, groups, names, descriptions }
    }

    fn export_param_info<'ll>(&self, cx: &mut mir_llvm::CodegenCx<'_, 'll>, ty: Type) {
        let params = self.param_info(cx, &ty);

        let sym = format!("params.{}", ty);
        cx.export_array(&sym, cx.ty_str(), &params.names, true, true);

        let sym = format!("params.unit.{}", ty);
        cx.export_array(&sym, cx.ty_str(), &params.units, true, false);

        let sym = format!("params.desc.{}", ty);
        cx.export_array(&sym, cx.ty_str(), &params.descriptions, true, false);

        let sym = format!("params.group.{}", ty);
        cx.export_array(&sym, cx.ty_str(), &params.groups, true, false);
    }
}

struct ParamInfo<'ll> {
    names: Vec<&'ll llvm::Value>,
    units: Vec<&'ll llvm::Value>,
    descriptions: Vec<&'ll llvm::Value>,
    groups: Vec<&'ll llvm::Value>,
}
