use llvm::{
    LLVMAppendBasicBlockInContext, LLVMBuildCall2, LLVMBuildFAdd, LLVMBuildFDiv, LLVMBuildFMul,
    LLVMBuildFSub, LLVMBuildGEP2, LLVMBuildRetVoid, LLVMBuildStore, LLVMCreateBuilderInContext,
    LLVMDisposeBuilder, LLVMGetParam, LLVMPositionBuilderAtEnd, LLVMSetFastMath,
    LLVMSetPartialFastMath, UNNAMED,
};
use sim_back::dae::{NoiseSource, NoiseSourceKind};
use stdx::iter::zip;
use typed_index_collections::TiVec;

use crate::compilation_unit::OsdiCompilationUnit;

#[derive(Debug, Clone, Copy)]
pub enum JacobianLoadType {
    Tran,
    Resist,
    React,
}

impl JacobianLoadType {
    const fn dst_reactive(self) -> bool {
        matches!(self, JacobianLoadType::React)
    }

    const fn read_resistive(self) -> bool {
        matches!(self, JacobianLoadType::Resist | JacobianLoadType::Tran)
    }

    const fn read_reactive(self) -> bool {
        matches!(self, JacobianLoadType::React | JacobianLoadType::Tran)
    }

    const fn name(self) -> &'static str {
        match self {
            JacobianLoadType::Tran => "tran",
            JacobianLoadType::Resist => "resist",
            JacobianLoadType::React => "react",
        }
    }
}

impl<'ll> OsdiCompilationUnit<'_, '_, 'll> {
    pub fn load_noise(&self) -> &'ll llvm::Value {
        let OsdiCompilationUnit { cx, module, .. } = self;
        let void_ptr = cx.ty_ptr();
        let f64_ptr_ty = cx.ty_ptr();
        let fun_ty =
            cx.ty_func(&[void_ptr, void_ptr, cx.ty_double(), f64_ptr_ty, f64_ptr_ty], cx.ty_void());
        let name = &format!("load_noise_{}", module.sym);
        let llfunc = cx.declare_int_c_fn(name, fun_ty);

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);
            LLVMPositionBuilderAtEnd(llbuilder, entry);
            let inst = LLVMGetParam(llfunc, 0);
            let model = LLVMGetParam(llfunc, 1);
            let freq = LLVMGetParam(llfunc, 2);
            let dst = LLVMGetParam(llfunc, 3);
            let dst_ln = LLVMGetParam(llfunc, 4);

            for (i, (src, eval_outputs)) in
                zip(&module.dae_system.noise_sources, &self.inst_data.noise).enumerate()
            {
                let fac = self.load_eval_output(eval_outputs.factor, inst, model, llbuilder);
                let pwr = match src.kind {
                    NoiseSourceKind::WhiteNoise { .. } => {
                        let pwr =
                            self.load_eval_output(eval_outputs.args[0], inst, model, llbuilder);
                        let pwr = LLVMBuildFMul(llbuilder, pwr, fac, UNNAMED);
                        LLVMSetFastMath(pwr);
                        pwr
                    }
                    NoiseSourceKind::FlickerNoise { .. } => {
                        let mut pwr =
                            self.load_eval_output(eval_outputs.args[0], inst, model, llbuilder);
                        let exp =
                            self.load_eval_output(eval_outputs.args[1], inst, model, llbuilder);
                        let (ty, fun) = self
                            .cx
                            .intrinsic("llvm.pow.f64")
                            .unwrap_or_else(|| unreachable!("intrinsic {} not found", name));
                        let freq_exp =
                            LLVMBuildCall2(llbuilder, ty, fun, [freq, exp].as_ptr(), 2, UNNAMED);
                        LLVMSetPartialFastMath(freq_exp);
                        pwr = LLVMBuildFDiv(llbuilder, pwr, freq_exp, UNNAMED);
                        LLVMSetFastMath(pwr);
                        pwr = LLVMBuildFMul(llbuilder, pwr, fac, UNNAMED);
                        LLVMSetFastMath(pwr);
                        pwr
                    }
                    NoiseSourceKind::NoiseTable { .. } => unimplemented!("noise tabels"),
                };
                let (ty, fun) = self
                    .cx
                    .intrinsic("llvm.log.f64")
                    .unwrap_or_else(|| unreachable!("intrinsic {} not found", name));
                let ln_pwr = LLVMBuildCall2(llbuilder, ty, fun, [pwr].as_ptr(), 1, UNNAMED);
                let dst = LLVMBuildGEP2(
                    llbuilder,
                    cx.ty_double(),
                    dst,
                    [cx.const_unsigned_int(i as u32)].as_ptr(),
                    1,
                    UNNAMED,
                );
                LLVMBuildStore(llbuilder, pwr, dst);
                let dst_ln = LLVMBuildGEP2(
                    llbuilder,
                    cx.ty_double(),
                    dst_ln,
                    [cx.const_unsigned_int(i as u32)].as_ptr(),
                    1,
                    UNNAMED,
                );
                LLVMBuildStore(llbuilder, ln_pwr, dst_ln);
            }

            // TODO noise
            LLVMBuildRetVoid(llbuilder);
            LLVMDisposeBuilder(llbuilder);
        }

        llfunc
    }

    pub fn load_residual(&self, reactive: bool) -> &'ll llvm::Value {
        let OsdiCompilationUnit { inst_data, cx, module, .. } = self;
        let ptr_ty = cx.ty_ptr();
        let fun_ty = cx.ty_func(&[ptr_ty, ptr_ty, ptr_ty], cx.ty_void());
        let name =
            &format!("load_residual_{}_{}", if reactive { "react" } else { "resist" }, module.sym);
        let llfunc = cx.declare_int_c_fn(name, fun_ty);

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);

            LLVMPositionBuilderAtEnd(llbuilder, entry);

            // get params
            let inst = LLVMGetParam(llfunc, 0);
            let dst = LLVMGetParam(llfunc, 2);

            for node in module.dae_system.unknowns.indices() {
                if let Some(contrib) = inst_data.read_residual(node, inst, llbuilder, reactive) {
                    inst_data.store_contrib(cx, node, inst, dst, contrib, llbuilder, false);
                }
            }

            LLVMBuildRetVoid(llbuilder);
            LLVMDisposeBuilder(llbuilder);
        }

        llfunc
    }

    pub fn load_lim_rhs(&self, reactive: bool) -> &'ll llvm::Value {
        let OsdiCompilationUnit { inst_data, cx, module, .. } = self;
        let void_ptr = cx.ty_ptr();
        let f64_ptr_ty = cx.ty_ptr();
        let fun_ty = cx.ty_func(&[void_ptr, void_ptr, f64_ptr_ty], cx.ty_void());
        let name =
            &format!("load_lim_rhs_{}_{}", if reactive { "react" } else { "resist" }, module.sym);
        let llfunc = cx.declare_int_c_fn(name, fun_ty);

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);

            LLVMPositionBuilderAtEnd(llbuilder, entry);

            // get params
            let inst = LLVMGetParam(llfunc, 0);
            let dst = LLVMGetParam(llfunc, 2);

            for node in module.dae_system.unknowns.indices() {
                if let Some(contrib) = inst_data.read_lim_rhs(node, inst, llbuilder, reactive) {
                    inst_data.store_contrib(cx, node, inst, dst, contrib, llbuilder, true);
                }
            }

            LLVMBuildRetVoid(llbuilder);
            LLVMDisposeBuilder(llbuilder);
        }

        llfunc
    }

    #[allow(clippy::too_many_arguments)]
    pub fn load_spice_rhs_(
        &self,
        tran: bool,
        llbuilder: &llvm::Builder<'ll>,
        inst: &'ll llvm::Value,
        model: &'ll llvm::Value,
        dst: &'ll llvm::Value,
        prev_solve: &'ll llvm::Value,
        alpha: &'ll llvm::Value,
    ) {
        let dae_system = &self.module.dae_system;
        let mut node_derivatives = TiVec::from(vec![Vec::new(); dae_system.unknowns.len()]);
        for (id, entry) in dae_system.jacobian.iter_enumerated() {
            node_derivatives[entry.row].push(id)
        }

        unsafe {
            for node in dae_system.unknowns.indices() {
                let mut res = None;
                for &entry in &node_derivatives[node] {
                    let node_deriv = dae_system.jacobian[entry].col;
                    let ddx = if let Some(ddx) =
                        self.load_jacobian_entry(entry, inst, model, llbuilder, tran)
                    {
                        ddx
                    } else {
                        continue;
                    };

                    let voltage = self
                        .inst_data
                        .read_node_voltage(self.cx, node_deriv, inst, prev_solve, llbuilder);
                    let val = LLVMBuildFMul(llbuilder, ddx, voltage, UNNAMED);
                    LLVMSetFastMath(val);
                    res = match res {
                        Some(old) => {
                            let val = LLVMBuildFAdd(llbuilder, old, val, UNNAMED);
                            LLVMSetFastMath(val);
                            Some(val)
                        }
                        None => Some(val),
                    }
                }

                let OsdiCompilationUnit { inst_data, cx, .. } = self;
                if !tran {
                    if let Some(contrib) = inst_data.read_residual(node, inst, llbuilder, false) {
                        let val = LLVMBuildFSub(
                            llbuilder,
                            res.unwrap_or_else(|| cx.const_real(0.0)),
                            contrib,
                            UNNAMED,
                        );
                        LLVMSetFastMath(val);
                        res = Some(val);
                    }
                }
                if let Some(mut res) = res {
                    if let Some(lim_rhs) = inst_data.read_lim_rhs(node, inst, llbuilder, tran) {
                        res = LLVMBuildFAdd(llbuilder, res, lim_rhs, UNNAMED);
                    }
                    if tran {
                        res = LLVMBuildFMul(llbuilder, res, alpha, UNNAMED);
                        LLVMSetFastMath(res);
                    }
                    inst_data.store_contrib(cx, node, inst, dst, res, llbuilder, false);
                }
            }
        }
    }

    pub fn load_spice_rhs(&self, tran: bool) -> &'ll llvm::Value {
        let OsdiCompilationUnit { cx, module, .. } = self;
        let f64_ty = cx.ty_double();
        let ptr_ty = cx.ty_ptr();
        let mut args = vec![ptr_ty, ptr_ty, ptr_ty, ptr_ty];
        if tran {
            args.push(f64_ty);
        }
        let fun_ty = cx.ty_func(&args, cx.ty_void());
        let name = &format!("load_spice_rhs_{}_{}", if tran { "tran" } else { "dc" }, &module.sym);
        let llfunc = cx.declare_int_c_fn(name, fun_ty);

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);
            LLVMPositionBuilderAtEnd(llbuilder, entry);

            // get params
            let inst = LLVMGetParam(llfunc, 0);
            let model = LLVMGetParam(llfunc, 1);
            let dst = LLVMGetParam(llfunc, 2);
            let prev_solve = LLVMGetParam(llfunc, 3);
            let alpha = if tran { LLVMGetParam(llfunc, 4) } else { prev_solve };

            self.load_spice_rhs_(false, llbuilder, inst, model, dst, prev_solve, alpha);
            if tran {
                self.load_spice_rhs_(true, llbuilder, inst, model, dst, prev_solve, alpha);
            }

            LLVMBuildRetVoid(llbuilder);
            LLVMDisposeBuilder(llbuilder);
        }

        llfunc
    }

    pub fn load_jacobian(&self, kind: JacobianLoadType) -> &'ll llvm::Value {
        let OsdiCompilationUnit { cx, module, .. } = *self;
        let args_ = [cx.ty_ptr(), cx.ty_ptr(), cx.ty_double()];
        let args = if kind.read_reactive() { &args_ } else { &args_[0..2] };
        let fun_ty = cx.ty_func(args, cx.ty_void());
        let name = &format!("load_jacobian_{}_{}", kind.name(), &module.sym,);
        let llfunc = cx.declare_int_c_fn(name, fun_ty);

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);

            LLVMPositionBuilderAtEnd(llbuilder, entry);
            // get params
            let inst = LLVMGetParam(llfunc, 0);
            let model = LLVMGetParam(llfunc, 1);
            let alpha = if kind.read_reactive() { LLVMGetParam(llfunc, 2) } else { inst };

            for entry in module.dae_system.jacobian.keys() {
                let mut res = None;
                if kind.read_resistive() {
                    res = self.load_jacobian_entry(entry, inst, model, llbuilder, false);
                }

                if kind.read_reactive() {
                    if let Some(mut val) =
                        self.load_jacobian_entry(entry, inst, model, llbuilder, true)
                    {
                        val = LLVMBuildFMul(llbuilder, val, alpha, UNNAMED);
                        LLVMSetFastMath(val);
                        val = match res {
                            Some(resist) => {
                                let val = LLVMBuildFAdd(llbuilder, resist, val, UNNAMED);
                                LLVMSetFastMath(val);
                                val
                            }
                            None => val,
                        };
                        res = Some(val)
                    }
                }

                if let Some(res) = res {
                    self.inst_data.store_jacobian_contrib(
                        self.cx,
                        entry,
                        inst,
                        llbuilder,
                        kind.dst_reactive(),
                        res,
                    );
                }
            }

            LLVMBuildRetVoid(llbuilder);
            LLVMDisposeBuilder(llbuilder);
        }

        llfunc
    }
}
