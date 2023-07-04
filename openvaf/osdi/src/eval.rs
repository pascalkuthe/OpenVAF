use ahash::AHashMap;
use hir_lower::{CallBackKind, CurrentKind, LimitState, ParamKind, PlaceKind};
use llvm::IntPredicate::{IntNE, IntULT};
use llvm::{
    LLVMAppendBasicBlockInContext, LLVMBuildAlloca, LLVMBuildAnd, LLVMBuildBr, LLVMBuildCall2,
    LLVMBuildCondBr, LLVMBuildICmp, LLVMBuildInBoundsGEP2, LLVMBuildIntCast2, LLVMBuildLoad2,
    LLVMBuildOr, LLVMBuildRet, LLVMBuildStore, LLVMCreateBuilderInContext, LLVMDisposeBuilder,
    LLVMGetParam, LLVMPositionBuilderAtEnd, UNNAMED,
};
use log::info;
use mir_llvm::{Builder, BuilderVal, CallbackFun, MemLoc};
use sim_back::{BoundStepKind, SimUnknown};
use typed_index_collections::TiVec;

use crate::bitfield::{is_flag_set, is_flag_set_mem, is_flag_unset};
use crate::compilation_unit::{general_callbacks, OsdiCompilationUnit};
use crate::inst_data::OsdiInstanceParam;
use crate::metadata::osdi_0_3::{
    ANALYSIS_IC, CALC_OP, CALC_REACT_JACOBIAN, CALC_REACT_LIM_RHS, CALC_REACT_RESIDUAL,
    CALC_RESIST_JACOBIAN, CALC_RESIST_LIM_RHS, CALC_RESIST_RESIDUAL, ENABLE_LIM, EVAL_RET_FLAG_LIM,
    INIT_LIM,
};
use crate::metadata::OsdiLimFunction;
use crate::OsdiLimId;

impl<'ll> OsdiCompilationUnit<'_, '_, 'll> {
    pub fn eval_prototype(&self) -> &'ll llvm::Value {
        let name = &format!("eval_{}", &self.module.sym);
        let cx = &self.cx;

        let ty_ptr = cx.ty_ptr();

        let fun_ty = cx.ty_func(&[ty_ptr, ty_ptr, ty_ptr, ty_ptr], cx.ty_int());
        cx.declare_ext_fn(name, fun_ty)
    }

    pub fn eval(&self) -> &'ll llvm::Value {
        let llfunc = self.eval_prototype();
        let OsdiCompilationUnit { inst_data, model_data, cx, module, .. } = self;

        let func = &module.mir.eval_func;
        let cfg = &module.mir.eval_cfg;
        let intern = &module.mir.eval_intern;

        let mut builder = Builder::new(cx, func, llfunc);
        let postorder: Vec<_> = cfg.postorder(func).collect();

        let handle = unsafe { llvm::LLVMGetParam(llfunc, 0) };
        let instance = unsafe { llvm::LLVMGetParam(llfunc, 1) };
        let model = unsafe { llvm::LLVMGetParam(llfunc, 2) };
        let sim_info = unsafe { llvm::LLVMGetParam(llfunc, 3) };
        let sim_info_ty = self.tys.osdi_sim_info;

        // let simparam_ty = self.tys.osdi_sim_paras;
        let simparam = unsafe { builder.struct_gep(sim_info_ty, sim_info, 0) };

        const ABSTIME_OFFSET: u32 = 1;

        let prev_result = unsafe {
            let ptr = builder.struct_gep(sim_info_ty, sim_info, 2);
            builder.load(cx.ty_ptr(), ptr)
        };

        let prev_state = unsafe {
            let ptr = builder.struct_gep(sim_info_ty, sim_info, 3);
            builder.load(cx.ty_ptr(), ptr)
        };

        let next_state = unsafe {
            let ptr = builder.struct_gep(sim_info_ty, sim_info, 3);
            builder.load(cx.ty_ptr(), ptr)
        };

        let flags = MemLoc::struct_gep(sim_info, sim_info_ty, cx.ty_int(), 5, cx);

        let ret_flags = unsafe { builder.alloca(cx.ty_int()) };
        unsafe { builder.store(ret_flags, cx.const_int(0)) };

        let connected_ports = unsafe { inst_data.load_connected_ports(&builder, instance) };
        let prev_solve: AHashMap<_, _> = module
            .node_ids
            .iter_enumerated()
            .map(|(node_id, node)| unsafe {
                let val = inst_data.read_node_voltage(
                    cx,
                    node_id,
                    instance,
                    prev_result,
                    builder.llbuilder,
                );
                (node, val)
            })
            .collect();

        let state_idx: TiVec<LimitState, _> = (0..intern.lim_state.len())
            .map(|i| unsafe { inst_data.read_state_idx(cx, i.into(), instance, builder.llbuilder) })
            .collect();

        let true_ = cx.const_bool(true);
        let mut params: TiVec<_, _> = intern
            .params
            .raw
            .iter()
            .map(|(kind, val)| {
                if func.dfg.value_dead(*val) && !inst_data.eval_outputs.contains_key(val) {
                    return BuilderVal::Undef;
                }

                let val = unsafe {
                    match *kind {
                        ParamKind::Param(param) => {
                            return inst_data
                                .param_loc(cx, OsdiInstanceParam::User(param), instance)
                                .unwrap_or_else(|| model_data.param_loc(cx, param, model).unwrap())
                                .into()
                        }
                        ParamKind::Voltage { hi, lo } => {
                            let hi = prev_solve[&SimUnknown::KirchoffLaw(hi)];
                            if let Some(lo) = lo {
                                let lo = prev_solve[&SimUnknown::KirchoffLaw(lo)];
                                llvm::LLVMBuildFSub(builder.llbuilder, hi, lo, UNNAMED)
                            } else {
                                hi
                            }
                        }
                        // TODO support abstime
                        ParamKind::Current(CurrentKind::Port(_)) => cx.const_real(0.0),
                        ParamKind::Abstime => {
                            let loc = MemLoc::struct_gep(
                                sim_info,
                                sim_info_ty,
                                cx.ty_double(),
                                ABSTIME_OFFSET,
                                cx,
                            );
                            return loc.into();
                        }

                        ParamKind::Current(kind) => prev_solve
                            .get(&SimUnknown::Current(kind))
                            .copied()
                            .unwrap_or_else(|| {
                                info!("current probe {kind:?} always returns zero");
                                cx.const_real(0.0)
                            }),
                        ParamKind::ImplicitUnknown(equation) => prev_solve
                            .get(&SimUnknown::Implicit(equation))
                            .copied()
                            .unwrap_or_else(|| {
                                info!("implicit equation {equation} collapsed to zero");
                                cx.const_real(0.0)
                            }),
                        ParamKind::Temperature => {
                            return inst_data.temperature_loc(cx, instance).into()
                        }
                        ParamKind::ParamGiven { param } => {
                            let inst_given = inst_data.is_param_given(
                                cx,
                                OsdiInstanceParam::User(param),
                                instance,
                                builder.llbuilder,
                            );
                            match inst_given {
                                Some(inst_given) => {
                                    let model_given = model_data.is_inst_param_given(
                                        inst_data,
                                        cx,
                                        OsdiInstanceParam::User(param),
                                        model,
                                        builder.llbuilder,
                                    );

                                    builder.select(inst_given, true_, model_given)
                                }
                                None => model_data
                                    .is_param_given(cx, param, model, builder.llbuilder)
                                    .unwrap(),
                            }
                        }
                        ParamKind::PortConnected { port } => {
                            let id = module.node_ids.unwrap_index(&SimUnknown::KirchoffLaw(port));
                            let id = cx.const_unsigned_int(id.into());
                            builder.int_cmp(id, connected_ports, IntULT)
                        }
                        ParamKind::ParamSysFun(param) => inst_data
                            .read_param(
                                OsdiInstanceParam::Builtin(param),
                                instance,
                                builder.llbuilder,
                            )
                            .unwrap(),
                        ParamKind::HiddenState(_) => unreachable!(), // TODO  hidden state
                        ParamKind::EnableIntegration => {
                            let flags = flags.read(builder.llbuilder);
                            let is_not_dc =
                                is_flag_set(cx, CALC_REACT_JACOBIAN, flags, builder.llbuilder);
                            let is_not_ic =
                                is_flag_unset(cx, ANALYSIS_IC, flags, builder.llbuilder);
                            LLVMBuildAnd(builder.llbuilder, is_not_dc, is_not_ic, UNNAMED)
                        }
                        ParamKind::PrevState(state) => {
                            let idx =
                                inst_data.read_state_idx(cx, state, instance, builder.llbuilder);
                            return MemLoc {
                                ptr: prev_state,
                                ptr_ty: cx.ty_double(),
                                ty: cx.ty_double(),
                                indices: vec![idx].into_boxed_slice(),
                            }
                            .into();
                        }
                        ParamKind::NewState(state) => {
                            let idx =
                                inst_data.read_state_idx(cx, state, instance, builder.llbuilder);

                            return MemLoc {
                                ptr: next_state,
                                ptr_ty: cx.ty_double(),
                                ty: cx.ty_double(),
                                indices: vec![idx].into_boxed_slice(),
                            }
                            .into();
                        }
                        ParamKind::EnableLim => {
                            is_flag_set_mem(cx, ENABLE_LIM, &flags, builder.llbuilder)
                        }
                    }
                };
                BuilderVal::Eager(val)
            })
            .collect();

        let cache_vals = (0..module.mir.cache_slots.len()).map(|i| unsafe {
            let slot = i.into();
            let val = inst_data.load_cache_slot(module, builder.llbuilder, slot, instance);
            BuilderVal::Eager(val)
        });

        params.extend(cache_vals);
        builder.params = params;

        builder.callbacks = general_callbacks(intern, &mut builder, ret_flags, handle, simparam);

        let ty_real_ptr = cx.ty_ptr();
        if module.mir.bound_step == BoundStepKind::Eval {
            let bound_step_ptr = unsafe { inst_data.bound_step_ptr(&builder, instance) };
            unsafe { builder.store(bound_step_ptr, cx.const_real(f64::INFINITY)) };

            let func_ref = intern.callbacks.unwrap_index(&CallBackKind::BoundStep);
            let fun = builder
                .cx
                .get_func_by_name("bound_step")
                .expect("stdlib function bound_step is missing");
            let fun_ty = cx.ty_func(&[ty_real_ptr, cx.ty_double()], cx.ty_void());
            let cb = CallbackFun { fun_ty, fun, state: Box::new([bound_step_ptr]), num_state: 0 };
            builder.callbacks[func_ref] = Some(cb);
        }

        let store_delay_fun = builder
            .cx
            .get_func_by_name("store_delay")
            .expect("stdlib function store_delay is missing");
        let store_delay_ty = cx.ty_func(&[cx.ty_ptr(), ty_real_ptr, cx.ty_double()], cx.ty_void());

        for (func, kind) in intern.callbacks.iter_enumerated() {
            let cb = match *kind {
                CallBackKind::BuiltinLimit { name, num_args } => {
                    let id = module
                        .lim_table
                        .unwrap_index(&OsdiLimFunction { name, num_args: num_args - 2 });
                    self.lim_func(id, num_args - 2, &flags, ret_flags)
                }
                CallBackKind::StoreLimit(state) => {
                    let fun = builder
                        .cx
                        .get_func_by_name("store_lim")
                        .expect("stdlib function store_lim is missing");
                    let fun_ty =
                        cx.ty_func(&[cx.ty_ptr(), cx.ty_int(), cx.ty_double()], cx.ty_double());
                    CallbackFun {
                        fun_ty,
                        fun,
                        state: Box::new([sim_info, state_idx[state]]),
                        num_state: 0,
                    }
                }
                CallBackKind::LimDiscontinuity => {
                    let fun = builder
                        .cx
                        .get_func_by_name("lim_discontinuity")
                        .expect("stdlib function lim_discontinuity is missing");
                    let fun_ty = cx.ty_func(&[cx.ty_ptr()], cx.ty_void());
                    CallbackFun { fun_ty, fun, state: Box::new([ret_flags]), num_state: 0 }
                }
                CallBackKind::Analysis => {
                    let fun = builder
                        .cx
                        .get_func_by_name("analysis")
                        .expect("stdlib function analysis is missing");
                    let fun_ty = cx.ty_func(&[cx.ty_ptr(), cx.ty_ptr()], cx.ty_int());
                    CallbackFun { fun_ty, fun, state: Box::new([sim_info]), num_state: 0 }
                }
                CallBackKind::StoreDelayTime(eq) => {
                    let slot = if let Some(&slot) = self.module.mir.eval_cache_vals.get(&eq) {
                        inst_data.cache_slot_ptr(builder.llbuilder, slot, instance).0
                    } else {
                        continue;
                    };

                    CallbackFun {
                        fun_ty: store_delay_ty,
                        fun: store_delay_fun,
                        state: Box::new([sim_info, slot]),
                        num_state: 0,
                    }
                }
                _ => continue,
            };
            builder.callbacks[func] = Some(cb);
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
        builder.select_bb(exit_bb);

        unsafe {
            for reactive in [false, true] {
                let matrix = &module.mir.matrix;
                let residual = &module.mir.residual;
                let lim_rhs = &module.mir.lim_rhs;
                let (matrix, matrix_flag, residual, residual_flag, lim_rhs, lim_rhs_flag) =
                    if reactive {
                        (
                            &matrix.reactive,
                            CALC_REACT_JACOBIAN,
                            &residual.reactive,
                            CALC_REACT_RESIDUAL,
                            &lim_rhs.reactive,
                            CALC_REACT_LIM_RHS,
                        )
                    } else {
                        (
                            &matrix.resistive,
                            CALC_RESIST_JACOBIAN,
                            &residual.resistive,
                            CALC_RESIST_RESIDUAL,
                            &lim_rhs.resistive,
                            CALC_RESIST_LIM_RHS,
                        )
                    };

                let store_matrix = |builder: &Builder<'_, '_, 'll>| {
                    for (id, entry) in module.matrix_ids.iter_enumerated() {
                        let entry = entry.to_middle(&module.node_ids);
                        if let Some(val) = matrix.raw.get(&entry) {
                            inst_data.store_jacobian(id, instance, *val, builder, reactive)
                        }
                    }
                };

                Self::build_store_results(&builder, llfunc, &flags, matrix_flag, &store_matrix);

                let store_residual = |builder: &Builder<'_, '_, 'll>| {
                    for (id, node) in module.node_ids.iter_enumerated() {
                        if let Some(val) = residual.raw.get(node) {
                            let val = builder.values[*val].get(builder);
                            inst_data.store_residual(
                                id,
                                instance,
                                val,
                                builder.llbuilder,
                                reactive,
                            );
                        }
                    }
                };

                Self::build_store_results(&builder, llfunc, &flags, residual_flag, &store_residual);

                let store_lim_rhs = |builder: &Builder<'_, '_, 'll>| {
                    for (id, node) in module.node_ids.iter_enumerated() {
                        if let Some(val) = lim_rhs.get(node) {
                            let val = builder.values[*val].get(builder);
                            inst_data.store_lim_rhs(id, instance, val, builder.llbuilder, reactive);
                        }
                    }
                };

                Self::build_store_results(&builder, llfunc, &flags, lim_rhs_flag, &store_lim_rhs);
            }

            let store_opvars = |builder: &Builder<'_, '_, 'll>| {
                for (i, var) in inst_data.opvars.keys().enumerate() {
                    let val = intern.outputs[&PlaceKind::Var(*var)].unwrap_unchecked();
                    inst_data.store_nth_opvar(i as u32, instance, val, builder);
                }
            };

            Self::build_store_results(&builder, llfunc, &flags, CALC_OP, &store_opvars);

            let ret_flags = builder.load(cx.ty_int(), ret_flags);
            builder.ret(ret_flags);
        }

        llfunc
    }

    unsafe fn build_store_results(
        builder: &Builder<'_, '_, 'll>,
        llfunc: &'ll llvm::Value,
        flags: &MemLoc<'ll>,
        flag: u32,
        store_val: &dyn Fn(&Builder<'_, '_, 'll>),
    ) {
        let cx = builder.cx;
        let bb = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
        let next_bb = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);

        let is_set = is_flag_set_mem(cx, flag, flags, builder.llbuilder);
        LLVMBuildCondBr(builder.llbuilder, is_set, bb, next_bb);

        LLVMPositionBuilderAtEnd(builder.llbuilder, bb);
        store_val(builder);
        LLVMBuildBr(builder.llbuilder, next_bb);

        LLVMPositionBuilderAtEnd(builder.llbuilder, next_bb);
    }

    fn lim_func(
        &self,
        id: OsdiLimId,
        num_args: u32,
        flags_loc: &MemLoc<'ll>,
        ret_flags_ptr: &'ll llvm::Value,
    ) -> CallbackFun<'ll> {
        let OsdiCompilationUnit { cx, tys, .. } = self;
        let table = self.lim_dispatch_table();

        let double = cx.ty_double();
        let c_bool = cx.ty_c_bool();
        let int = cx.ty_int();

        let mut args = vec![cx.ty_ptr(), cx.ty_ptr(), double, double];
        args.resize(num_args as usize + 4, double);
        let fun_ty = cx.ty_func(&args, double);
        let name = &format!("lim_{}_{id}", &self.module.sym);
        let llfunc = cx.declare_int_fn(name, fun_ty);

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let exit = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let val_changed_bb = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);
            LLVMPositionBuilderAtEnd(llbuilder, entry);

            let mut flags = LLVMGetParam(llfunc, 0);
            flags = flags_loc.read_with_ptr(llbuilder, flags);
            let mut init = is_flag_set(cx, INIT_LIM, flags, llbuilder);
            init = LLVMBuildIntCast2(llbuilder, init, c_bool, llvm::False, UNNAMED);

            let mut val_changed = LLVMBuildAlloca(llbuilder, c_bool, UNNAMED);
            LLVMBuildStore(llbuilder, cx.const_c_bool(false), val_changed);

            let func_ptr_ptr = LLVMBuildInBoundsGEP2(
                llbuilder,
                tys.osdi_lim_function,
                table,
                [cx.const_unsigned_int(id.into()), cx.const_int(2)].as_ptr(),
                2,
                UNNAMED,
            );

            let func_ptr = LLVMBuildLoad2(llbuilder, cx.ty_ptr(), func_ptr_ptr, UNNAMED);
            let mut lim_fn_args = vec![c_bool, cx.ty_ptr(), double, double];
            lim_fn_args.extend((0..num_args).map(|_| double));
            let lim_fn_ty = cx.ty_func(&lim_fn_args, double);
            let mut args = vec![init, val_changed];
            args.extend((2..4 + num_args).map(|i| LLVMGetParam(llfunc, i)));
            let res = LLVMBuildCall2(
                llbuilder,
                lim_fn_ty,
                func_ptr,
                args.as_ptr(),
                args.len() as u32,
                UNNAMED,
            );

            val_changed = LLVMBuildLoad2(llbuilder, c_bool, val_changed, UNNAMED);
            val_changed =
                LLVMBuildICmp(llbuilder, IntNE, val_changed, cx.const_c_bool(false), UNNAMED);
            LLVMBuildCondBr(llbuilder, val_changed, val_changed_bb, exit);

            LLVMPositionBuilderAtEnd(llbuilder, val_changed_bb);
            let ret_flags_ptr = LLVMGetParam(llfunc, 1);
            let mut ret_flags = LLVMBuildLoad2(llbuilder, int, ret_flags_ptr, UNNAMED);
            ret_flags = LLVMBuildOr(
                llbuilder,
                ret_flags,
                cx.const_unsigned_int(EVAL_RET_FLAG_LIM),
                UNNAMED,
            );
            LLVMBuildStore(llbuilder, ret_flags, ret_flags_ptr);
            LLVMBuildBr(llbuilder, exit);

            LLVMPositionBuilderAtEnd(llbuilder, exit);
            LLVMBuildRet(llbuilder, res);

            LLVMDisposeBuilder(llbuilder);
        }

        CallbackFun {
            fun_ty,
            fun: llfunc,
            state: Box::new([flags_loc.ptr, ret_flags_ptr]),
            num_state: 0,
        }
    }
}
