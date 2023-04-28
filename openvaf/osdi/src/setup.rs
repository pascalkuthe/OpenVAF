use hir_lower::{CallBackKind, ParamInfoKind, ParamKind, PlaceKind};

use llvm::IntPredicate::IntSLT;
use llvm::{
    LLVMAppendBasicBlockInContext, LLVMBuildBr, LLVMBuildCondBr, LLVMBuildRetVoid,
    LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMGetParam, LLVMPositionBuilderAtEnd,
    UNNAMED,
};
use mir::ControlFlowGraph;
use mir_llvm::{Builder, BuilderVal, CallbackFun, CodegenCx};
use sim_back::{BoundStepKind, SimUnknown};

use crate::compilation_unit::{general_callbacks, OsdiCompilationUnit};
use crate::inst_data::OsdiInstanceParam;

impl<'ll> OsdiCompilationUnit<'_, '_, 'll> {
    fn mark_collapsed(&self) -> (&'ll llvm::Value, &'ll llvm::Type) {
        let OsdiCompilationUnit { inst_data, cx, .. } = self;
        let inst_data_ptr = cx.ptr_ty(inst_data.ty);
        let fn_type = cx.ty_func(&[inst_data_ptr, cx.ty_int()], cx.ty_void());
        let name = &format!("collapse_{}", &self.module.sym);
        let llfunc = cx.declare_int_c_fn(name, fn_type);

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);
            LLVMPositionBuilderAtEnd(llbuilder, entry);

            // get params
            let inst = LLVMGetParam(llfunc, 0);
            let idx = LLVMGetParam(llfunc, 1);

            inst_data.store_is_collapsible(cx, llbuilder, inst, idx);

            LLVMBuildRetVoid(llbuilder);
            LLVMDisposeBuilder(llbuilder);
        }

        (llfunc, fn_type)
    }

    fn invalid_param_err(cx: &CodegenCx<'_, 'll>) -> (&'ll llvm::Type, &'ll llvm::Value) {
        let val = cx
            .get_func_by_name("push_invalid_param_err")
            .expect("stdlib function push_invalid_param_err is missing");

        let ty = cx.ty_func(
            &[
                cx.ptr_ty(cx.ty_void_ptr()),
                cx.ptr_ty(cx.ty_int()),
                cx.ptr_ty(cx.ty_int()),
                cx.ty_int(),
            ],
            cx.ty_void(),
        );

        (ty, val)
    }

    pub fn setup_model_prototype(&self) -> &'ll llvm::Value {
        let cx = &self.cx;
        let name = &format!("setup_model_{}", &self.module.sym);
        let simparam_ptr_ty = cx.ptr_ty(self.tys.osdi_sim_paras);

        let fun_ty = cx.ty_func(
            &[
                cx.ty_void_ptr(),
                cx.ty_void_ptr(),
                simparam_ptr_ty,
                cx.ptr_ty(self.tys.osdi_init_info),
            ],
            cx.ty_void(),
        );
        cx.declare_ext_fn(name, fun_ty)
    }

    pub fn setup_model(&self) -> &'ll llvm::Value {
        let llfunc = self.setup_model_prototype();
        let OsdiCompilationUnit { inst_data, model_data, tys, cx, .. } = self;

        let func = &self.module.mir.init_model_func;
        let intern = &self.module.mir.init_model_intern;

        let mut cfg = ControlFlowGraph::new();
        cfg.compute(func);
        let mut builder = Builder::new(cx, func, llfunc);
        let postorder: Vec<_> = cfg.postorder(func).collect();

        let handle = unsafe { llvm::LLVMGetParam(llfunc, 0) };
        let model = unsafe {
            let raw = llvm::LLVMGetParam(llfunc, 1);
            llvm::LLVMBuildPointerCast(builder.llbuilder, raw, cx.ptr_ty(model_data.ty), UNNAMED)
        };
        let simparam = unsafe { llvm::LLVMGetParam(llfunc, 2) };

        builder.params = vec![BuilderVal::Undef; intern.params.len()].into();

        for (i, param) in model_data.params.keys().copied().enumerate() {
            let i = i as u32;

            let dst = intern.params.unwrap_index(&ParamKind::Param(param));
            let loc = model_data.nth_param_loc(cx, i, model);
            builder.params[dst] = BuilderVal::Load(Box::new(loc));

            let dst = intern.params.unwrap_index(&ParamKind::ParamGiven { param });
            let is_given =
                unsafe { model_data.is_nth_param_given(cx, i, model, builder.llbuilder) };
            builder.params[dst] = BuilderVal::Eager(is_given);
        }

        for (i, param) in inst_data.params.keys().enumerate() {
            let i = i as u32;

            let is_given =
                unsafe { model_data.is_nth_inst_param_given(cx, i, model, builder.llbuilder) };

            let val = unsafe {
                model_data.read_nth_inst_param(inst_data, i as u32, model, builder.llbuilder)
            };

            match *param {
                OsdiInstanceParam::Builtin(builtin) => {
                    if let Some(dst) = intern.params.index(&ParamKind::ParamSysFun(builtin)) {
                        let default_val = builtin.default_value();
                        let default_val = cx.const_real(default_val);
                        let val = unsafe { builder.select(is_given, val, default_val) };
                        builder.params[dst] = BuilderVal::Eager(val);
                    }
                }
                OsdiInstanceParam::User(param) => {
                    let dst = intern.params.unwrap_index(&ParamKind::Param(param));
                    builder.params[dst] = BuilderVal::Eager(val);
                    let dst = intern.params.unwrap_index(&ParamKind::ParamGiven { param });
                    builder.params[dst] = BuilderVal::Eager(is_given);
                }
            }
        }

        let res = unsafe { llvm::LLVMGetParam(llfunc, 3) };

        let err_cap = unsafe { builder.alloca(cx.ty_int()) };

        let flags = unsafe { builder.struct_gep(res, 0) };
        let err_len = unsafe { builder.struct_gep(res, 1) };
        let err_ptr = unsafe { builder.struct_gep(res, 2) };

        let err_ptr_ty = cx.ptr_ty(tys.osdi_init_error);
        let nullptr = cx.const_null_ptr(err_ptr_ty);
        let zero = cx.const_unsigned_int(0);

        unsafe {
            builder.store(err_ptr, nullptr);
            builder.store(err_len, zero);
            builder.store(err_cap, zero);
            builder.store(flags, zero);
        }

        let void_ptr_ptr = cx.ptr_ty(cx.ty_void_ptr());
        let err_ptr_void = unsafe { builder.ptrcast(err_ptr, void_ptr_ptr) };

        let invalid_param_err = Self::invalid_param_err(cx);

        let ret_flags = unsafe { builder.alloca(cx.ty_int()) };
        unsafe { builder.store(ret_flags, cx.const_int(0)) };

        builder.callbacks = general_callbacks(intern, &mut builder, ret_flags, handle, simparam);
        for (call_id, call) in intern.callbacks.iter_enumerated() {
            if let CallBackKind::ParamInfo(ParamInfoKind::Invalid, param) = call {
                if !self.module.base.params[param].is_instance {
                    let id =
                        model_data.params.get_index_of(param).unwrap() + inst_data.params.len();
                    let err_param = cx.const_unsigned_int(id as u32);
                    let cb = CallbackFun {
                        fun_ty: invalid_param_err.0,
                        fun: invalid_param_err.1,
                        state: vec![err_ptr_void, err_len, err_cap, err_param].into_boxed_slice(),
                        num_state: 0,
                    };

                    builder.callbacks[call_id] = Some(cb);
                }
            }
        }

        unsafe {
            builder.build_consts();
            builder.build_cfg(&postorder);
        }

        let exit_bb = *postorder
            .iter()
            .find(|bb| {
                func.layout
                    .last_inst(**bb)
                    .map_or(true, |term| !func.dfg.insts[term].is_terminator())
            })
            .unwrap();

        // store parameters
        for (i, param) in model_data.params.keys().enumerate() {
            let val = intern.outputs[&PlaceKind::Param(*param)].unwrap_unchecked();
            let inst = func.dfg.value_def(val).unwrap_inst();
            let bb = func.layout.inst_block(inst).unwrap();
            builder.select_bb_before_terminator(bb);
            unsafe {
                let val = builder.values[val].get(&builder);
                model_data.store_nth_param(i as u32, model, val, builder.llbuilder);
            }
        }

        builder.select_bb(exit_bb);
        unsafe { builder.ret_void() }

        llfunc
    }

    pub fn setup_instance_prototype(&self) -> &'ll llvm::Value {
        let name = &format!("setup_instance_{}", &self.module.sym);
        let cx = &self.cx;

        let ty_void_ptr = cx.ty_void_ptr();

        let simparam_ptr_ty = cx.ptr_ty(self.tys.osdi_sim_paras);
        let fun_ty = cx.ty_func(
            &[
                ty_void_ptr,
                ty_void_ptr,
                ty_void_ptr,
                cx.ty_real(),
                cx.ty_int(),
                simparam_ptr_ty,
                cx.ptr_ty(self.tys.osdi_init_info),
            ],
            cx.ty_void(),
        );

        cx.declare_ext_fn(name, fun_ty)
    }

    pub fn setup_instance(&mut self) -> &'ll llvm::Value {
        let mark_collapsed = self.mark_collapsed();
        let llfunc = self.setup_instance_prototype();
        let OsdiCompilationUnit { inst_data, model_data, tys, cx, module, .. } = self;

        let func = &module.mir.init_inst_func;
        let cfg = &module.mir.init_inst_cfg;
        let intern = &module.mir.init_inst_intern;
        let mut builder = Builder::new(cx, func, llfunc);
        let postorder: Vec<_> = cfg.postorder(func).collect();

        let handle = unsafe { llvm::LLVMGetParam(llfunc, 0) };
        let instance = unsafe {
            let raw = llvm::LLVMGetParam(llfunc, 1);
            llvm::LLVMBuildPointerCast(builder.llbuilder, raw, cx.ptr_ty(inst_data.ty), UNNAMED)
        };
        let model = unsafe {
            let raw = llvm::LLVMGetParam(llfunc, 2);
            llvm::LLVMBuildPointerCast(builder.llbuilder, raw, cx.ptr_ty(model_data.ty), UNNAMED)
        };
        let temperature = unsafe { llvm::LLVMGetParam(llfunc, 3) };
        let connected_terminals = unsafe { llvm::LLVMGetParam(llfunc, 4) };
        let simparam = unsafe { llvm::LLVMGetParam(llfunc, 5) };
        let res = unsafe { llvm::LLVMGetParam(llfunc, 6) };

        let ret_flags = unsafe { builder.alloca(cx.ty_int()) };
        unsafe { builder.store(ret_flags, cx.const_int(0)) };

        builder.params = vec![BuilderVal::Undef; intern.params.len()].into();

        let true_ = cx.const_bool(true);

        for (i, param) in inst_data.params.keys().enumerate() {
            let i = i as u32;

            let is_inst_given =
                unsafe { inst_data.is_nth_param_given(cx, i, instance, builder.llbuilder) };
            let is_given = unsafe {
                let is_given_model =
                    model_data.is_nth_inst_param_given(cx, i, model, builder.llbuilder);
                builder.select(is_inst_given, true_, is_given_model)
            };

            let inst_val =
                unsafe { inst_data.read_nth_param(i as u32, instance, builder.llbuilder) };
            let model_val =
                unsafe { model_data.read_nth_inst_param(inst_data, i, model, builder.llbuilder) };
            let val = unsafe { builder.select(is_inst_given, inst_val, model_val) };

            match *param {
                OsdiInstanceParam::Builtin(builtin) => {
                    let dst = intern.params.unwrap_index(&ParamKind::ParamSysFun(builtin));
                    let default_val = builtin.default_value();
                    let default_val = cx.const_real(default_val);
                    let val = unsafe { builder.select(is_given, val, default_val) };
                    unsafe {
                        inst_data.store_nth_param(i as u32, instance, val, builder.llbuilder);
                    }
                    builder.params[dst] = BuilderVal::Eager(val);
                }
                OsdiInstanceParam::User(param) => {
                    let dst = intern.params.unwrap_index(&ParamKind::Param(param));
                    builder.params[dst] = BuilderVal::Eager(val);
                    let dst = intern.params.unwrap_index(&ParamKind::ParamGiven { param });
                    builder.params[dst] = BuilderVal::Eager(is_given);
                }
            }
        }

        for (i, param) in model_data.params.keys().copied().enumerate() {
            let i = i as u32;

            if let Some(dst) = intern.params.index(&ParamKind::Param(param)) {
                let loc = model_data.nth_param_loc(cx, i, model);
                builder.params[dst] = BuilderVal::Load(Box::new(loc));
            }

            if let Some(dst) = intern.params.index(&ParamKind::ParamGiven { param }) {
                let is_given =
                    unsafe { model_data.is_nth_param_given(cx, i, model, builder.llbuilder) };
                builder.params[dst] = BuilderVal::Eager(is_given);
            }
        }

        if let Some(dst) = intern.params.index(&ParamKind::Temperature) {
            builder.params[dst] = BuilderVal::Eager(temperature)
        }

        for (node_id, unknown) in module.node_ids.iter_enumerated() {
            if let SimUnknown::KirchhoffLaw(node) = unknown {
                if let Some((dst, val)) =
                    intern.params.index_and_val(&ParamKind::PortConnected { port: *node })
                {
                    if func.dfg.value_dead(*val) {
                        continue;
                    }

                    let id = cx.const_unsigned_int(node_id.into());
                    let is_connected = unsafe { builder.int_cmp(id, connected_terminals, IntSLT) };
                    builder.params[dst] = BuilderVal::Eager(is_connected)
                }
            }
        }

        // store for use in eval() function
        unsafe { inst_data.store_temperature(&builder, instance, temperature) };
        unsafe { inst_data.store_connected_ports(&builder, instance, connected_terminals) };

        let trivial_cb = cx.trivial_callbacks(&[]);

        let err_cap = unsafe { builder.alloca(cx.ty_int()) };

        let flags = unsafe { builder.struct_gep(res, 0) };
        let err_len = unsafe { builder.struct_gep(res, 1) };
        let err_ptr = unsafe { builder.struct_gep(res, 2) };
        let void_ptr_ptr = cx.ptr_ty(cx.ty_void_ptr());
        let err_ptr_void = unsafe { builder.ptrcast(err_ptr, void_ptr_ptr) };

        let err_ptr_ty = cx.ptr_ty(tys.osdi_init_error);
        let nullptr = cx.const_null_ptr(err_ptr_ty);
        let zero = cx.const_unsigned_int(0);

        unsafe {
            builder.store(err_ptr, nullptr);
            builder.store(err_len, zero);
            builder.store(err_cap, zero);
            builder.store(flags, zero);
        }

        let invalid_param_err = Self::invalid_param_err(cx);
        builder.callbacks = general_callbacks(intern, &mut builder, ret_flags, handle, simparam);
        for (call_id, call) in intern.callbacks.iter_enumerated() {
            let cb = match call {
                CallBackKind::ParamInfo(ParamInfoKind::Invalid, param) => {
                    if let Some(id) =
                        inst_data.params.get_index_of(&OsdiInstanceParam::User(*param))
                    {
                        let err_param = cx.const_unsigned_int(id as u32);
                        CallbackFun {
                            fun_ty: invalid_param_err.0,
                            fun: invalid_param_err.1,
                            state: vec![err_ptr_void, err_len, err_cap, err_param]
                                .into_boxed_slice(),
                            num_state: 0,
                        }
                    } else {
                        trivial_cb.clone()
                    }
                }
                CallBackKind::CollapseHint(node1, node2) => {
                    let node1 = SimUnknown::KirchhoffLaw(*node1);
                    let node2 = node2.map(SimUnknown::KirchhoffLaw);
                    let info = module.mir.collapse.index_and_val(&(node1, node2));
                    let (idx, extra_indecies) = if let Some(info) = info {
                        info
                    } else {
                        continue;
                    };
                    let idx = cx.const_unsigned_int(idx.into());
                    let mut state = vec![instance, idx];
                    for &idx in extra_indecies.iter() {
                        state.push(instance);
                        let idx = cx.const_unsigned_int(idx.into());
                        state.push(idx)
                    }
                    CallbackFun {
                        fun_ty: mark_collapsed.1,
                        fun: mark_collapsed.0,
                        state: state.into_boxed_slice(),
                        num_state: 2,
                    }
                }
                _ => continue,
            };

            builder.callbacks[call_id] = Some(cb);
        }

        unsafe {
            builder.build_consts();
            builder.build_cfg(&postorder);
        }
        let exit_bb = *postorder
            .iter()
            .find(|bb| {
                func.layout
                    .last_inst(**bb)
                    .map_or(true, |term| !func.dfg.insts[term].is_terminator())
            })
            .unwrap();

        // store parameters
        for (i, param) in inst_data.params.keys().enumerate() {
            let val = match param {
                OsdiInstanceParam::Builtin(_) => continue,
                OsdiInstanceParam::User(param) => {
                    intern.outputs[&PlaceKind::Param(*param)].unwrap_unchecked()
                }
            };

            let inst = func.dfg.value_def(val).unwrap_inst();
            let bb = func.layout.inst_block(inst).unwrap();
            builder.select_bb_before_terminator(bb);

            unsafe {
                let val = builder.values[val].get(&builder);
                inst_data.store_nth_param(i as u32, instance, val, builder.llbuilder);
            }
        }

        builder.select_bb(exit_bb);

        for (idx, (collapse, _)) in module.mir.collapse.iter_enumerated() {
            if let (SimUnknown::Implicit(equation), None) = collapse {
                let should_collapse = PlaceKind::CollapseImplicitEquation(*equation);
                let outputs = &module.mir.init_inst_intern.outputs;
                let should_collapse = outputs[&should_collapse].unwrap_unchecked();
                unsafe {
                    let should_collapse = builder.values[should_collapse].get(&builder);
                    let idx = cx.const_unsigned_int(idx.into());

                    let llcx = cx.llcx;
                    let llbuilder = &*builder.llbuilder;

                    let else_bb = LLVMAppendBasicBlockInContext(llcx, builder.fun, UNNAMED);
                    let then_bb = LLVMAppendBasicBlockInContext(llcx, builder.fun, UNNAMED);
                    LLVMBuildCondBr(llbuilder, should_collapse, then_bb, else_bb);
                    LLVMPositionBuilderAtEnd(llbuilder, then_bb);
                    inst_data.store_is_collapsible(cx, builder.llbuilder, instance, idx);
                    LLVMBuildBr(llbuilder, else_bb);
                    LLVMPositionBuilderAtEnd(llbuilder, else_bb);
                }
            }
        }

        if module.mir.bound_step == BoundStepKind::Setup {
            let bound_step_ptr = unsafe { inst_data.bound_step_ptr(&builder, instance) };
            unsafe { builder.store(bound_step_ptr, cx.const_real(f64::INFINITY)) };

            let func_ref = intern.callbacks.unwrap_index(&CallBackKind::BoundStep);
            let ty_real_ptr = cx.ptr_ty(cx.ty_real());
            let fun = builder
                .cx
                .get_func_by_name("bound_step")
                .expect("stdlib function bound_step is missing");
            let fun_ty = cx.ty_func(&[ty_real_ptr, cx.ty_real()], cx.ty_void());
            let cb = CallbackFun { fun_ty, fun, state: Box::new([bound_step_ptr]), num_state: 0 };
            builder.callbacks[func_ref] = Some(cb);
        }

        unsafe { builder.ret_void() }

        for (&val, &slot) in module.mir.init_inst_cache_vals.iter() {
            let inst = func.dfg.value_def(val).unwrap_inst();
            let bb = func.layout.inst_block(inst).unwrap();
            builder.select_bb_before_terminator(bb);
            let inst = func.dfg.value_def(val).unwrap_inst();
            let bb = func.layout.inst_block(inst).unwrap();
            builder.select_bb_before_terminator(bb);
            unsafe {
                let val = builder.values[val].get(&builder);
                inst_data.store_cache_slot(module, builder.llbuilder, slot, instance, val)
            }
        }

        llfunc
    }
}
