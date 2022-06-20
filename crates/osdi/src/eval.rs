use ahash::AHashMap;
use hir_lower::{CurrentKind, ParamKind, PlaceKind};
use llvm::IntPredicate::{IntNE, IntULT};
use llvm::{
    LLVMAppendBasicBlockInContext, LLVMBuildAnd, LLVMBuildBr, LLVMBuildCondBr, LLVMBuildICmp,
    LLVMPositionBuilderAtEnd, UNNAMED,
};
use mir_llvm::{Builder, BuilderVal};
use sim_back::SimUnkown;
use typed_index_collections::TiVec;

use crate::compilation_unit::{general_callbacks, OsdiCompilationUnit};
use crate::inst_data::OsdiInstanceParam;
use crate::metadata::osdi_0_3::{
    CALC_OP, CALC_REACT_JACOBIAN, CALC_REACT_RESIDUAL, CALC_RESIST_JACOBIAN, CALC_RESIST_RESIDUAL,
};

impl<'ll> OsdiCompilationUnit<'_, '_, 'll> {
    pub fn eval_prototype(&self) -> &'ll llvm::Value {
        let name = &format!("eval_{}", &self.module.sym);
        let cx = &self.cx;

        let ty_void_ptr = cx.ty_void_ptr();
        let simparam_ptr_ty = cx.ptr_ty(self.tys.osdi_sim_paras);

        let fun_ty = cx.ty_func(
            &[
                ty_void_ptr,
                ty_void_ptr,
                ty_void_ptr,
                cx.ty_int(),
                cx.ptr_ty(cx.ty_real()),
                simparam_ptr_ty,
            ],
            cx.ty_int(),
        );
        cx.declare_ext_fn(name, fun_ty)
    }

    pub fn eval(&mut self) -> &'ll llvm::Value {
        let llfunc = self.eval_prototype();
        let OsdiCompilationUnit { inst_data, model_data, cx, module, .. } = self;
        // unsafe {
        //     let build = LLVMCreateBuilderInContext(cx.llcx);

        //     let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
        //     LLVMPositionBuilderAtEnd(build, entry);

        //     LLVMBuildRet(build, cx.const_int(0));
        //     LLVMDisposeBuilder(build);
        //     return llfunc;
        // }

        let func = &module.mir.eval_func;
        let cfg = &module.mir.eval_cfg;
        let intern = &module.mir.eval_intern;

        let mut builder = Builder::new(cx, func, llfunc);
        let postorder: Vec<_> = cfg.postorder(func).collect();

        // builder.callbacks = callbacks(&self.intern.callbacks, builder.cx);

        let handle = unsafe { llvm::LLVMGetParam(llfunc, 0) };
        let instance = unsafe {
            let raw = llvm::LLVMGetParam(llfunc, 1);
            builder.ptrcast(raw, builder.cx.ptr_ty(inst_data.ty))
        };
        let model = unsafe {
            let raw = llvm::LLVMGetParam(llfunc, 2);
            builder.ptrcast(raw, builder.cx.ptr_ty(model_data.ty))
        };
        let flags = unsafe { llvm::LLVMGetParam(llfunc, 3) };
        let prev_result = unsafe { llvm::LLVMGetParam(llfunc, 4) };
        let simparam = unsafe { llvm::LLVMGetParam(llfunc, 5) };
        let ret_flags = unsafe { builder.alloca(builder.cx.ty_int()) };
        unsafe { builder.store(ret_flags, builder.cx.const_int(0)) };

        let connected_ports = unsafe { inst_data.load_connected_ports(&builder, instance) };
        let prev_solve: AHashMap<_, _> = module
            .node_ids
            .iter_enumerated()
            .map(|(node_id, node)| unsafe {
                let val = inst_data.read_node_voltage(
                    builder.cx,
                    node_id,
                    instance,
                    prev_result,
                    builder.llbuilder,
                );
                (node, val)
            })
            .collect();

        let true_ = builder.cx.const_bool(true);
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
                                .param_loc(builder.cx, OsdiInstanceParam::User(param), instance)
                                .unwrap_or_else(|| {
                                    model_data.param_loc(builder.cx, param, model).unwrap()
                                })
                                .into()
                        }
                        ParamKind::Voltage { hi, lo } => {
                            let hi = prev_solve[&SimUnkown::KirchoffLaw(hi)];
                            if let Some(lo) = lo {
                                let lo = prev_solve[&SimUnkown::KirchoffLaw(lo)];
                                llvm::LLVMBuildFSub(builder.llbuilder, hi, lo, UNNAMED)
                            } else {
                                hi
                            }
                        }
                        // TODO support abstime
                        ParamKind::Current(CurrentKind::Port(_)) | ParamKind::Abstime => {
                            builder.cx.const_real(0.0)
                        }
                        ParamKind::Current(kind) => prev_solve[&SimUnkown::Current(kind)],
                        ParamKind::ImplicitUnkown(equation) => {
                            prev_solve[&SimUnkown::Implicit(equation)]
                        }
                        ParamKind::Temperature => inst_data.load_temperature(&builder, instance),
                        ParamKind::ParamGiven { param } => {
                            let inst_given = inst_data.is_param_given(
                                builder.cx,
                                OsdiInstanceParam::User(param),
                                instance,
                                builder.llbuilder,
                            );
                            match inst_given {
                                Some(inst_given) => {
                                    let model_given = model_data.is_inst_param_given(
                                        inst_data,
                                        builder.cx,
                                        OsdiInstanceParam::User(param),
                                        model,
                                        builder.llbuilder,
                                    );

                                    builder.select(inst_given, true_, model_given)
                                }
                                None => model_data
                                    .is_param_given(builder.cx, param, model, builder.llbuilder)
                                    .unwrap(),
                            }
                        }
                        ParamKind::PortConnected { port } => {
                            let id = module.node_ids.unwrap_index(&SimUnkown::KirchoffLaw(port));
                            let id = builder.cx.const_unsigned_int(id.into());
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
                        ParamKind::EnableIntegration => builder.cx.const_bool(true), // TODO integration check to support IC
                    }
                };
                BuilderVal::Eager(val)
            })
            .collect();

        let cache_vals = (0..module.mir.init_inst_cache_slots.len()).map(|i| unsafe {
            let slot = i.into();
            let val = inst_data.load_cache_slot(module, builder.llbuilder, slot, instance);
            BuilderVal::Eager(val)
        });

        params.extend(cache_vals);
        builder.params = params;

        builder.callbacks = general_callbacks(intern, &mut builder, ret_flags, handle, simparam);

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
                let (matrix, matrix_flag, residual, residual_flag) = if reactive {
                    (&matrix.reactive, CALC_REACT_JACOBIAN, &residual.reactive, CALC_REACT_RESIDUAL)
                } else {
                    (
                        &matrix.resistive,
                        CALC_RESIST_JACOBIAN,
                        &residual.resistive,
                        CALC_RESIST_RESIDUAL,
                    )
                };

                let store_matrix = |builder: &mut Builder<'_, '_, 'll>| {
                    for (id, entry) in module.matrix_ids.iter_enumerated() {
                        let entry = entry.to_middle(&module.node_ids);
                        if let Some(val) = matrix.raw.get(&entry) {
                            inst_data.store_jacobian(id, instance, *val, builder, reactive)
                        }
                    }
                };

                Self::build_store_results(&mut builder, llfunc, flags, matrix_flag, &store_matrix);

                let store_residual = |builder: &mut Builder<'_, '_, 'll>| {
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

                Self::build_store_results(
                    &mut builder,
                    llfunc,
                    flags,
                    residual_flag,
                    &store_residual,
                );
            }

            let store_opvars = |builder: &mut Builder<'_, '_, 'll>| {
                for (i, var) in inst_data.opvars.keys().enumerate() {
                    let val = intern.outputs[&PlaceKind::Var(*var)].unwrap_unchecked();
                    inst_data.store_nth_opvar(i as u32, instance, val, builder);
                }
            };

            Self::build_store_results(&mut builder, llfunc, flags, CALC_OP, &store_opvars);

            let ret_flags = builder.load(builder.cx.ty_int(), ret_flags);
            builder.ret(ret_flags);
        }

        llfunc
    }

    unsafe fn build_store_results(
        builder: &mut Builder<'_, '_, 'll>,
        llfunc: &'ll llvm::Value,
        flags: &'ll llvm::Value,
        flag: u32,
        store_val: &dyn Fn(&mut Builder<'_, '_, 'll>),
    ) {
        let bb = LLVMAppendBasicBlockInContext(builder.cx.llcx, llfunc, UNNAMED);
        let next_bb = LLVMAppendBasicBlockInContext(builder.cx.llcx, llfunc, UNNAMED);

        let flag = builder.cx.const_unsigned_int(flag);
        let and = LLVMBuildAnd(builder.llbuilder, flags, flag, UNNAMED);
        let is_set = LLVMBuildICmp(builder.llbuilder, IntNE, and, builder.cx.const_int(0), UNNAMED);
        LLVMBuildCondBr(builder.llbuilder, is_set, bb, next_bb);

        LLVMPositionBuilderAtEnd(builder.llbuilder, bb);
        store_val(builder);
        LLVMBuildBr(builder.llbuilder, next_bb);

        LLVMPositionBuilderAtEnd(builder.llbuilder, next_bb);
    }
}
