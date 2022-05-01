use llvm::{
    LLVMAppendBasicBlockInContext, LLVMBuildFAdd, LLVMBuildFMul, LLVMBuildFSub,
    LLVMBuildPointerCast, LLVMBuildRetVoid, LLVMCreateBuilderInContext, LLVMDisposeBuilder,
    LLVMGetParam, LLVMPositionBuilderAtEnd, LLVMSetFastMath, UNNAMED,
};
use mir_llvm::CodegenCx;
use sim_back::matrix::MatrixEntry;

use crate::compilation_unit::{OsdiCompilationUnit, OsdiMatrixEntry};

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

impl<'ll> OsdiCompilationUnit<'_, 'll> {
    pub fn load_noise(&self, cx: &mut CodegenCx<'_, 'll>) -> &'ll llvm::Value {
        let void_ptr = cx.ty_void_ptr();
        let f64_ty = cx.ty_real();
        let f64_ptr_ty = cx.ptr_ty(f64_ty);
        let fun_ty =
            cx.ty_func(&[void_ptr, void_ptr, cx.ty_real(), f64_ptr_ty, f64_ptr_ty], cx.ty_void());
        let name = &format!("load_noise_{}", self.sym);
        let llfunc = cx.declare_int_c_fn(name, fun_ty);

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);
            LLVMPositionBuilderAtEnd(llbuilder, entry);

            // TODO noise
            LLVMBuildRetVoid(llbuilder);
            LLVMDisposeBuilder(llbuilder);
        }

        llfunc
    }
    pub fn load_residual(&self, cx: &mut CodegenCx<'_, 'll>, reactive: bool) -> &'ll llvm::Value {
        let void_ptr = cx.ty_void_ptr();
        let f64_ty = cx.ty_real();
        let f64_ptr_ty = cx.ptr_ty(f64_ty);
        let fun_ty = cx.ty_func(&[void_ptr, void_ptr, f64_ptr_ty], cx.ty_void());
        let name =
            &format!("load_residual_{}_{}", if reactive { "react" } else { "resist" }, self.sym);
        let llfunc = cx.declare_int_c_fn(name, fun_ty);
        let inst_data = self.inst_data();

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);

            LLVMPositionBuilderAtEnd(llbuilder, entry);

            // get params
            let inst = LLVMGetParam(llfunc, 0);
            let inst = LLVMBuildPointerCast(llbuilder, inst, cx.ptr_ty(inst_data.ty), UNNAMED);
            let dst = LLVMGetParam(llfunc, 2);

            let nodes = if reactive {
                self.mir.residual.reactive.raw.keys()
            } else {
                self.mir.residual.resistive.raw.keys()
            };
            for node in nodes {
                let node = self.node_ids.unwrap_index(node);
                if let Some(contrib) = inst_data.read_residual(node, inst, llbuilder, reactive) {
                    inst_data.store_contrib(cx, node, inst, dst, contrib, llbuilder);
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
        cx: &mut CodegenCx<'_, 'll>,
        tran: bool,
        llbuilder: &mut llvm::Builder<'ll>,
        inst: &'ll llvm::Value,
        model: &'ll llvm::Value,
        dst: &'ll llvm::Value,
        prev_solve: &'ll llvm::Value,
        alpha: &'ll llvm::Value,
    ) {
        let inst_data = self.inst_data();

        let residual = &self.mir.residual;
        let matrix = &self.mir.matrix;

        let (nodes, matrix) = if tran {
            (residual.reactive.raw.keys(), &matrix.reactive)
        } else {
            (residual.resistive.raw.keys(), &matrix.resistive)
        };

        unsafe {
            for node in nodes {
                let node_id = self.node_ids.unwrap_index(node);
                let mut res = None;

                for (node_deriv_id, node_deriv) in self.node_ids.iter_enumerated() {
                    if !matrix.contains_key(&MatrixEntry { row: *node, col: *node_deriv }) {
                        continue;
                    }
                    let matrix_entry = self
                        .matrix_ids
                        .unwrap_index(&OsdiMatrixEntry { row: node_id, col: node_deriv_id });

                    let matrix_entry = inst_data
                        .load_jacobian(cx, self, matrix_entry, inst, model, llbuilder, tran)
                        .unwrap();

                    let voltage =
                        inst_data.read_node_voltage(cx, node_deriv_id, inst, prev_solve, llbuilder);
                    let val = LLVMBuildFMul(llbuilder, matrix_entry, voltage, UNNAMED);
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
                if !tran {
                    if let Some(contrib) = inst_data.read_residual(node_id, inst, llbuilder, false)
                    {
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
                    if tran {
                        res = LLVMBuildFMul(llbuilder, res, alpha, UNNAMED);
                        LLVMSetFastMath(res);
                    }
                    inst_data.store_contrib(cx, node_id, inst, dst, res, llbuilder);
                }
            }
        }
    }

    pub fn load_spice_rhs(&self, cx: &mut CodegenCx<'_, 'll>, tran: bool) -> &'ll llvm::Value {
        let void_ptr = cx.ty_void_ptr();
        let f64_ty = cx.ty_real();
        let f64_ptr_ty = cx.ptr_ty(f64_ty);
        let mut args = vec![void_ptr, void_ptr, f64_ptr_ty, f64_ptr_ty];
        if tran {
            args.push(f64_ty);
        }
        let fun_ty = cx.ty_func(&args, cx.ty_void());
        let name = &format!("load_spice_rhs_{}_{}", if tran { "tran" } else { "dc" }, &self.sym);
        let llfunc = cx.declare_int_c_fn(name, fun_ty);

        let inst_data = self.inst_data();
        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);
            LLVMPositionBuilderAtEnd(llbuilder, entry);

            // get params
            let inst = LLVMGetParam(llfunc, 0);
            let inst = LLVMBuildPointerCast(llbuilder, inst, cx.ptr_ty(inst_data.ty), UNNAMED);
            let model = LLVMGetParam(llfunc, 1);
            let model =
                LLVMBuildPointerCast(llbuilder, model, cx.ptr_ty(self.model_data().ty), UNNAMED);
            let dst = LLVMGetParam(llfunc, 2);
            let prev_solve = LLVMGetParam(llfunc, 3);
            let alpha = if tran { LLVMGetParam(llfunc, 4) } else { prev_solve };

            self.load_spice_rhs_(cx, false, llbuilder, inst, model, dst, prev_solve, alpha);
            if tran {
                self.load_spice_rhs_(cx, true, llbuilder, inst, model, dst, prev_solve, alpha);
            }

            LLVMBuildRetVoid(llbuilder);
            LLVMDisposeBuilder(llbuilder);
        }

        llfunc
    }

    pub fn load_jacobian(
        &self,
        cx: &mut CodegenCx<'_, 'll>,
        kind: JacobianLoadType,
    ) -> &'ll llvm::Value {
        let args_ = [cx.ty_void_ptr(), cx.ty_void_ptr(), cx.ty_real()];
        let args = if kind.read_reactive() { &args_ } else { &args_[0..2] };
        let fun_ty = cx.ty_func(args, cx.ty_void());
        let name = &format!("load_jacobian_{}_{}", kind.name(), &self.sym,);
        let llfunc = cx.declare_int_c_fn(name, fun_ty);
        let inst_data = self.inst_data();
        let model_data = self.model_data();

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);

            LLVMPositionBuilderAtEnd(llbuilder, entry);
            // get params
            let inst = LLVMGetParam(llfunc, 0);
            let inst = LLVMBuildPointerCast(llbuilder, inst, cx.ptr_ty(inst_data.ty), UNNAMED);
            let model = LLVMGetParam(llfunc, 1);
            let model = LLVMBuildPointerCast(llbuilder, model, cx.ptr_ty(model_data.ty), UNNAMED);
            let alpha = if kind.read_reactive() { LLVMGetParam(llfunc, 2) } else { inst };

            for (id, entry) in self.matrix_ids.iter_enumerated() {
                let entry = entry.to_middle(&self.node_ids);
                let mut res = None;
                if kind.read_resistive() && self.mir.matrix.resistive.contains_key(&entry) {
                    res = inst_data.load_jacobian(cx, self, id, inst, model, llbuilder, false);
                }

                if kind.read_reactive() && self.mir.matrix.reactive.contains_key(&entry) {
                    let mut val = inst_data
                        .load_jacobian(cx, self, id, inst, model, llbuilder, true)
                        .unwrap();
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

                if let Some(res) = res {
                    inst_data.store_jacobian_contrib(
                        cx,
                        id,
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
