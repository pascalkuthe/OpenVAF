use std::iter::once;

use hir_def::db::HirDefDB;
use hir_def::Type;
use hir_lower::{CallBackKind, ParamKind};
use lasso::Rodeo;
use mir::FuncRef;
use mir_llvm::{Builder, CallbackFun, CodegenCx, LLVMBackend, ModuleLlvm};
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::compilation_db::CompilationDB;
use crate::middle::AnalogBlockMir;

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
        Type::EmptyArray => cx.zst(),
        Type::Bool => cx.ty_bool(),
        Type::Void => cx.ty_void(),
        Type::Err => unreachable!(),
    }
}

impl AnalogBlockMir {
    pub fn to_bin(
        &self,
        db: &CompilationDB,
        name: &str,
        literals: &Rodeo,
        backend: &LLVMBackend,
    ) -> ModuleLlvm {
        let module = unsafe { backend.new_module(name).unwrap() };
        let mut cx = unsafe { backend.new_ctx(literals, &module) };

        let ret_arr_ty = cx.ty_array(cx.ty_real(), self.matrix.entrys.len() as u32);
        let ret_ty = cx.ptr_ty(ret_arr_ty);
        let arg_tys: Vec<_> = once(ret_ty)
            .chain(self.intern.params.raw.iter().filter_map(|(pkind, val)| {
                if self.func.dfg.value_dead(*val) {
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

        let fun_ty = cx.ty_func(&arg_tys, cx.ty_void());
        let llfunc = cx.declare_ext_fn(name, fun_ty);

        let mut builder = Builder::new(&mut cx, &self.func, llfunc);
        let postorder: Vec<_> = self.cfg.postorder(&self.func).collect();

        builder.callbacks = callbacks(&self.intern.callbacks, builder.cx);

        let mut i = 1;
        builder.params = self
            .intern
            .params
            .raw
            .iter()
            .map(|(_, val)| {
                if self.func.dfg.value_dead(*val) {
                    None
                } else {
                    let res = unsafe { llvm::LLVMGetParam(llfunc, i) };
                    i += 1;
                    Some(res)
                }
            })
            .collect();

        unsafe {
            builder.build_consts();
            builder.build_cfg(&postorder);
            builder.select_bb(postorder[0]);
            let dst = llvm::LLVMGetParam(llfunc, 0);
            for (i, val) in self.matrix.entrys.raw.values().enumerate() {
                let dst = builder.gep(dst, &[builder.cx.const_usize(0), builder.cx.const_usize(i)]);
                let val = builder.values[*val].unwrap();
                builder.store(dst, val);
            }

            builder.ret_void();
        }

        drop(builder);
        debug_assert!(module.verify_and_print(), "Invalid code generated");
        module
        // module.optimize(&backend);
        // module.emit_obect("foo.o".as_ref()).expect("code generation failed!");
    }
}
