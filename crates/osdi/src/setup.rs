use hir_lower::{CallBackKind, ParamInfoKind, ParamKind, PlaceKind};

use llvm::IntPredicate::IntSLT;
use llvm::{
    LLVMAppendBasicBlockInContext, LLVMBuildRetVoid, LLVMCreateBuilderInContext,
    LLVMDisposeBuilder, LLVMGetParam, LLVMPositionBuilderAtEnd, UNNAMED,
};
use mir::ControlFlowGraph;
use mir_llvm::{Builder, CallbackFun, CodegenCx};

use crate::compilation_unit::OsdiCompilationUnit;
use crate::inst_data::OsdiInstanceParam;

impl<'a, 'll> OsdiCompilationUnit<'a, 'll> {
    fn mark_collapsed(&self, cx: &mut CodegenCx<'_, 'll>) -> (&'ll llvm::Value, &'ll llvm::Type) {
        let inst_data = self.inst_data();
        let inst_data_ptr = cx.ptr_ty(inst_data.ty);
        let fn_type = cx.ty_func(&[inst_data_ptr, cx.ty_int()], cx.ty_void());
        let name = &format!("collapse_{}", &self.sym);
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

    pub fn setup_model(&self, cx: &mut CodegenCx<'_, 'll>) -> &'ll llvm::Value {
        let name = &format!("setup_model_{}", &self.sym);
        let simparam_ptr_ty = cx.ptr_ty(self.tys().osdi_sim_paras);

        let fun_ty = cx.ty_func(
            &[
                cx.ty_void_ptr(),
                cx.ty_void_ptr(),
                simparam_ptr_ty,
                cx.ptr_ty(self.tys().osdi_init_info),
            ],
            cx.ty_void(),
        );
        let llfunc = cx.declare_int_c_fn(name, fun_ty);

        let func = &self.mir.init_model_func;
        let intern = &self.mir.init_model_intern;

        let mut cfg = ControlFlowGraph::new();
        cfg.compute(func);
        let mut builder = Builder::new(cx, func, llfunc);
        let postorder: Vec<_> = cfg.postorder(func).collect();

        let model_data = self.model_data();
        let inst_data = self.inst_data();

        let handle = unsafe { llvm::LLVMGetParam(llfunc, 0) };
        let model = unsafe {
            let raw = llvm::LLVMGetParam(llfunc, 1);
            llvm::LLVMBuildPointerCast(
                builder.llbuilder,
                raw,
                builder.cx.ptr_ty(model_data.ty),
                UNNAMED,
            )
        };
        let simparam = unsafe { llvm::LLVMGetParam(llfunc, 2) };

        builder.params = vec![None; intern.params.len()].into();

        for (i, param) in model_data.params.keys().copied().enumerate() {
            let i = i as u32;

            let dst = intern.params.unwrap_index(&ParamKind::Param(param));
            let val = unsafe { model_data.read_nth_param(i, model, builder.llbuilder) };
            builder.params[dst] = Some(val);

            let dst = intern.params.unwrap_index(&ParamKind::ParamGiven { param });
            let is_given =
                unsafe { model_data.is_nth_param_given(builder.cx, i, model, builder.llbuilder) };
            builder.params[dst] = Some(is_given);
        }

        // let trivial_cb = builder.cx.trivial_callbacks(&[]);

        let res = unsafe { llvm::LLVMGetParam(llfunc, 3) };

        let err_cap = unsafe { builder.alloca(builder.cx.ty_int()) };

        let flags = unsafe { builder.struct_gep(res, 0) };
        let err_len = unsafe { builder.struct_gep(res, 1) };
        let err_ptr = unsafe { builder.struct_gep(res, 2) };

        let err_ptr_ty = builder.cx.ptr_ty(self.tys().osdi_init_error);
        let nullptr = builder.cx.const_null_ptr(err_ptr_ty);
        let zero = builder.cx.const_unsigned_int(0);

        unsafe {
            builder.store(err_ptr, nullptr);
            builder.store(err_len, zero);
            builder.store(err_cap, zero);
            builder.store(flags, zero);
        }

        let void_ptr_ptr = builder.cx.ptr_ty(builder.cx.ty_void_ptr());
        let err_ptr_void = unsafe { builder.ptrcast(err_ptr, void_ptr_ptr) };

        let invalid_param_err = Self::invalid_param_err(builder.cx);

        let ret_flags = unsafe { builder.alloca(builder.cx.ty_int()) };
        unsafe { builder.store(ret_flags, builder.cx.const_int(0)) };

        builder.callbacks =
            self.general_callbacks(intern, &mut builder, ret_flags, handle, simparam);
        for (call_id, call) in intern.callbacks.iter_enumerated() {
            if let CallBackKind::ParamInfo(ParamInfoKind::Invalid, param) = call {
                if !self.module.params[param].is_instance {
                    let id =
                        model_data.params.get_index_of(param).unwrap() + inst_data.params.len();
                    let err_param = builder.cx.const_unsigned_int(id as u32);
                    let cb = CallbackFun {
                        fun_ty: invalid_param_err.0,
                        fun: invalid_param_err.1,
                        state: vec![err_ptr_void, err_len, err_cap, err_param].into_boxed_slice(),
                    };

                    builder.callbacks[call_id] = Some(cb);
                }
            }
        }

        unsafe {
            builder.build_consts();
            builder.build_cfg(&postorder);
        }

        // store parameters
        builder.select_bb(postorder[0]);
        for (i, param) in model_data.params.keys().enumerate() {
            let val = intern.outputs[&PlaceKind::Param(*param)].unwrap_unchecked();
            let val = builder.values[val].unwrap();
            unsafe {
                model_data.store_nth_param(i as u32, model, val, builder.llbuilder);
            }
        }

        unsafe { builder.ret_void() }

        llfunc
    }

    pub fn setup_instance(&self, cx: &mut CodegenCx<'_, 'll>) -> &'ll llvm::Value {
        let name = &format!("setup_instance_{}", &self.sym);

        let ty_void_ptr = cx.ty_void_ptr();

        let simparam_ptr_ty = cx.ptr_ty(self.tys().osdi_sim_paras);
        let fun_ty = cx.ty_func(
            &[
                ty_void_ptr,
                ty_void_ptr,
                ty_void_ptr,
                cx.ty_real(),
                cx.ty_int(),
                simparam_ptr_ty,
                cx.ptr_ty(self.tys().osdi_init_info),
            ],
            cx.ty_void(),
        );
        let llfunc = cx.declare_int_c_fn(name, fun_ty);

        let func = &self.mir.init_inst_func;
        let cfg = &self.mir.init_inst_cfg;
        let intern = &self.mir.init_inst_intern;
        let mut builder = Builder::new(cx, func, llfunc);
        let postorder: Vec<_> = cfg.postorder(func).collect();

        // builder.callbacks = callbacks(&self.intern.callbacks, builder.cx);
        let inst_data = self.inst_data();
        let model_data = self.model_data();

        let handle = unsafe { llvm::LLVMGetParam(llfunc, 0) };
        let instance = unsafe {
            let raw = llvm::LLVMGetParam(llfunc, 1);
            llvm::LLVMBuildPointerCast(
                builder.llbuilder,
                raw,
                builder.cx.ptr_ty(inst_data.ty),
                UNNAMED,
            )
        };
        let model = unsafe {
            let raw = llvm::LLVMGetParam(llfunc, 2);
            llvm::LLVMBuildPointerCast(
                builder.llbuilder,
                raw,
                builder.cx.ptr_ty(model_data.ty),
                UNNAMED,
            )
        };
        let temperature = unsafe { llvm::LLVMGetParam(llfunc, 3) };
        let connected_terminals = unsafe { llvm::LLVMGetParam(llfunc, 4) };
        let simparam = unsafe { llvm::LLVMGetParam(llfunc, 5) };
        let res = unsafe { llvm::LLVMGetParam(llfunc, 6) };

        let ret_flags = unsafe { builder.alloca(builder.cx.ty_int()) };
        unsafe { builder.store(ret_flags, builder.cx.const_int(0)) };

        builder.params = vec![None; intern.params.len()].into();

        let true_ = builder.cx.const_bool(true);

        for (i, param) in inst_data.params.keys().enumerate() {
            let i = i as u32;

            let is_inst_given =
                unsafe { inst_data.is_nth_param_given(builder.cx, i, instance, builder.llbuilder) };

            let is_given = unsafe {
                let is_given_model =
                    model_data.is_nth_inst_param_given(builder.cx, i, model, builder.llbuilder);
                builder.select(is_inst_given, true_, is_given_model)
            };

            let val = unsafe { inst_data.read_nth_param(i as u32, instance, builder.llbuilder) };

            match *param {
                OsdiInstanceParam::Builtin(builtin) => {
                    let dst = intern.params.unwrap_index(&ParamKind::ParamSysFun(builtin));

                    let default_val = builtin.default_value();
                    let default_val = builder.cx.const_real(default_val);
                    let val = unsafe {
                        let model_val =
                            model_data.read_nth_inst_param(inst_data, i, model, builder.llbuilder);
                        builder.select(is_inst_given, val, model_val)
                    };

                    let val = unsafe { builder.select(is_given, val, default_val) };
                    builder.params[dst] = Some(val);
                }
                OsdiInstanceParam::User(param) => {
                    let dst = intern.params.unwrap_index(&ParamKind::Param(param));
                    let val = unsafe {
                        let model_val =
                            model_data.read_nth_inst_param(inst_data, i, model, builder.llbuilder);
                        builder.select(is_inst_given, val, model_val)
                    };
                    builder.params[dst] = Some(val);

                    let dst = intern.params.unwrap_index(&ParamKind::ParamGiven { param });

                    builder.params[dst] = Some(is_given);
                }
            }
        }

        for (i, param) in model_data.params.keys().copied().enumerate() {
            let i = i as u32;

            if let Some(dst) = intern.params.index(&ParamKind::Param(param)) {
                let val = unsafe { model_data.read_nth_param(i, model, builder.llbuilder) };
                builder.params[dst] = Some(val);
            }

            if let Some(dst) = intern.params.index(&ParamKind::ParamGiven { param }) {
                let is_given = unsafe {
                    model_data.is_nth_param_given(builder.cx, i, model, builder.llbuilder)
                };
                builder.params[dst] = Some(is_given);
            }
        }

        if let Some(dst) = intern.params.index(&ParamKind::Temperature) {
            builder.params[dst] = Some(temperature)
        }

        for (node_id, node) in self.node_ids.iter_enumerated() {
            if u32::from(node_id) >= self.num_terminals {
                break;
            }
            if let Some((dst, val)) =
                intern.params.index_and_val(&ParamKind::PortConnected { port: *node })
            {
                if func.dfg.value_dead(*val) {
                    continue;
                }

                let id = builder.cx.const_unsigned_int(node_id.into());
                let is_connected = unsafe { builder.int_cmp(id, connected_terminals, IntSLT) };
                builder.params[dst] = Some(is_connected)
            }
        }

        // store for use in eval() function
        unsafe { inst_data.store_temperature(&builder, instance, temperature) };
        unsafe { inst_data.store_connected_ports(&builder, instance, connected_terminals) };

        let trivial_cb = builder.cx.trivial_callbacks(&[]);

        let err_cap = unsafe { builder.alloca(builder.cx.ty_int()) };

        let flags = unsafe { builder.struct_gep(res, 0) };
        let err_len = unsafe { builder.struct_gep(res, 1) };
        let err_ptr = unsafe { builder.struct_gep(res, 2) };

        let err_ptr_ty = builder.cx.ptr_ty(self.tys().osdi_init_error);
        let nullptr = builder.cx.const_null_ptr(err_ptr_ty);
        let zero = builder.cx.const_unsigned_int(0);

        unsafe {
            builder.store(err_ptr, nullptr);
            builder.store(err_len, zero);
            builder.store(err_cap, zero);
            builder.store(flags, zero);
        }

        let invalid_param_err = Self::invalid_param_err(builder.cx);
        let mark_collapsed = self.mark_collapsed(builder.cx);
        builder.callbacks =
            self.general_callbacks(intern, &mut builder, ret_flags, handle, simparam);
        for (call_id, call) in intern.callbacks.iter_enumerated() {
            let cb = match call {
                CallBackKind::ParamInfo(ParamInfoKind::Invalid, param) => {
                    if let Some(id) =
                        inst_data.params.get_index_of(&OsdiInstanceParam::User(*param))
                    {
                        let err_param = builder.cx.const_unsigned_int(id as u32);
                        CallbackFun {
                            fun_ty: invalid_param_err.0,
                            fun: invalid_param_err.1,
                            state: vec![err_ptr, err_len, err_cap, err_param].into_boxed_slice(),
                        }
                    } else {
                        trivial_cb.clone()
                    }
                }
                CallBackKind::CollapseHint(node1, node2) => {
                    let idx = self.mir.collapse.unwrap_index(&(*node1, *node2));
                    let idx = builder.cx.const_unsigned_int(idx.into());
                    CallbackFun {
                        fun_ty: mark_collapsed.1,
                        fun: mark_collapsed.0,
                        state: vec![instance, idx].into_boxed_slice(),
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

        // store parameters
        builder.select_bb(postorder[0]);
        for (i, param) in inst_data.params.keys().enumerate() {
            let val = match param {
                OsdiInstanceParam::Builtin(fun) => intern.params.raw[&ParamKind::ParamSysFun(*fun)],
                OsdiInstanceParam::User(param) => {
                    intern.outputs[&PlaceKind::Param(*param)].unwrap_unchecked()
                }
            };

            let val = builder.values[val].unwrap();
            unsafe {
                inst_data.store_nth_param(i as u32, instance, val, builder.llbuilder);
            }
        }

        unsafe { builder.ret_void() }

        for (&val, &slot) in self.mir.init_inst_cache_vals.iter() {
            let inst = func.dfg.value_def(val).unwrap_inst();
            let bb = func.layout.inst_block(inst).unwrap();
            builder.select_bb_before_terminator(bb);
            let val = builder.values[val].unwrap();
            unsafe { inst_data.store_cache_slot(builder.llbuilder, slot, instance, val) }
        }

        llfunc
    }
}
