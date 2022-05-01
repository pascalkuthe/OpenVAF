use std::iter::once;

use hir_def::db::HirDefDB;
use hir_def::Type;
use hir_lower::{CallBackKind, ParamKind};
use lasso::Rodeo;
use mir::{ControlFlowGraph, FuncRef, Function, Param, Value};
use mir_llvm::{Builder, CallbackFun, CodegenCx, LLVMBackend, ModuleLlvm};
use salsa::ParallelDatabase;
use target::spec::Target;
use typed_index_collections::TiVec;
use typed_indexmap::{TiMap, TiSet};

use crate::compilation_db::CompilationDB;
use crate::middle::EvalMir;

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

pub fn callbacks<'ll>(
    cb: &TiSet<FuncRef, CallBackKind>,
    cx: &mut CodegenCx<'_, 'll>,
) -> TiVec<FuncRef, Option<CallbackFun<'ll>>> {
    cb.raw
        .iter()
        .map(|kind| match kind {
            CallBackKind::SimParam => Some(sim_param_stub(cx)),
            CallBackKind::SimParamOpt => Some(sim_param_opt_stub(cx)),
            CallBackKind::SimParamStr => Some(sim_param_str_stub(cx)),
            CallBackKind::Derivative(_) | CallBackKind::NodeDerivative(_) => {
                Some(cx.const_callback(&[cx.ty_real()], cx.const_real(0.0)))
            }
            CallBackKind::ParamInfo(_, _) | CallBackKind::CollapseHint(_, _) => None,
        })
        .collect()
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum FuncKind {
    InstanceInit,
    Eval,
}

impl EvalMir {
    pub fn to_bin(&self, db: &CompilationDB, literals: &Rodeo, target: &Target) {
        let db1 = db.snapshot();
        let db2 = db.snapshot();
        // rayon_core::join(
        // || {
        let backend = LLVMBackend::new(&[], target, llvm::OptLevel::Aggressive);
        self.func_to_bin(
            &self.init_inst_func,
            &self.init_inst_cfg,
            &self.eval_intern.params,
            db1,
            literals,
            &backend,
            FuncKind::InstanceInit,
        );
        // },
        // || {
        let backend = LLVMBackend::new(&[], target, llvm::OptLevel::Aggressive);
        self.func_to_bin(
            &self.eval_func,
            &self.eval_cfg,
            &self.eval_intern.params,
            db2,
            literals,
            &backend,
            FuncKind::Eval,
        );
        // },
        // );
    }

    #[allow(clippy::too_many_arguments)]
    fn func_to_bin(
        &self,
        func: &Function,
        cfg: &ControlFlowGraph,
        params: &TiMap<Param, ParamKind, Value>,
        db: salsa::Snapshot<CompilationDB>,
        literals: &Rodeo,
        backend: &LLVMBackend,
        kind: FuncKind,
    ) -> ModuleLlvm {
        let name = &format!("{:?}", kind);

        let module = unsafe { backend.new_module(name).unwrap() };
        let mut cx = unsafe { backend.new_ctx(literals, &module) };

        let ret_ty = match kind {
            FuncKind::Eval => {
                let output_len = self.matrix.resistive.len()
                    + self.matrix.reactive.len()
                    + self.residual.resistive.len()
                    + self.residual.reactive.len();
                let ret_arr_ty = cx.ty_array(cx.ty_real(), output_len as u32);
                cx.ptr_ty(ret_arr_ty)
            }
            FuncKind::InstanceInit => {
                let ret_tys: Vec<_> =
                    self.init_inst_cache_slots.raw.iter().map(|(_, ty)| lltype(ty, &cx)).collect();

                let ret_struct_ty = cx.struct_ty(name, &ret_tys);
                cx.ptr_ty(ret_struct_ty)
            }
        };

        let mut arg_tys: Vec<_> = once(ret_ty)
            .chain(params.raw.iter().filter_map(|(pkind, val)| {
                if func.dfg.value_dead(*val) {
                    return None;
                }
                let ty = match pkind {
                    ParamKind::Param(param) => lltype(&db.param_data(*param).ty, &cx),
                    ParamKind::Voltage { .. }
                    | ParamKind::Current(_)
                    | ParamKind::Temperature
                    | ParamKind::ParamSysFun(_) => cx.ty_real(),
                    ParamKind::ParamGiven { .. } | ParamKind::PortConnected { .. } => cx.ty_bool(),
                    ParamKind::HiddenState(_) => unreachable!(),
                };
                Some(ty)
            }))
            .collect();

        if kind == FuncKind::Eval {
            arg_tys.extend(self.init_inst_cache_slots.raw.iter().map(|(_, ty)| lltype(ty, &cx)));
        }

        let fun_ty = cx.ty_func(&arg_tys, cx.ty_void());
        let llfunc = cx.declare_ext_fn(name, fun_ty);

        let mut builder = Builder::new(&mut cx, func, llfunc);
        let postorder: Vec<_> = cfg.postorder(func).collect();

        builder.callbacks = callbacks(&self.eval_intern.callbacks, builder.cx);

        let mut i = 1;
        builder.params = params
            .raw
            .iter()
            .map(|(_, val)| {
                if func.dfg.value_dead(*val) {
                    None
                } else {
                    let res = unsafe { llvm::LLVMGetParam(llfunc, i) };
                    i += 1;
                    Some(res)
                }
            })
            .collect();

        if kind == FuncKind::Eval {
            builder.params.extend(
                (i..(self.init_inst_cache_slots.len() as u32 + i))
                    .map(|i| unsafe { Some(llvm::LLVMGetParam(llfunc, i)) }),
            );
        }

        unsafe {
            builder.build_consts();
            builder.build_cfg(&postorder);
            let dst = llvm::LLVMGetParam(llfunc, 0);

            match kind {
                FuncKind::Eval => {
                    builder.select_bb(postorder[0]);

                    for (i, val) in self.eval_outputs.iter().enumerate() {
                        let dst = builder
                            .gep(dst, &[builder.cx.const_usize(0), builder.cx.const_usize(i)]);
                        let val = builder.values[val].unwrap();
                        builder.store(dst, val);
                    }

                    builder.ret_void();
                }

                FuncKind::InstanceInit => {
                    builder.select_bb(postorder[0]);
                    builder.ret_void();
                    for (&val, &i) in self.init_inst_cache_vals.iter() {
                        let inst = func.dfg.value_def(val).unwrap_inst();
                        let bb = func.layout.inst_block(inst).unwrap();
                        builder.select_bb_before_terminator(bb);
                        let dst = builder.struct_gep(dst, i.into());
                        let val_ = builder.values[val];
                        builder.store(dst, val_.unwrap());
                    }
                }
            };
        }

        drop(builder);

        debug_assert!(module.verify_and_print(), "Invalid code generated");
        module.optimize(backend);
        module.emit_obect("foo.o".as_ref()).expect("code generation failed!");

        module
    }
}
