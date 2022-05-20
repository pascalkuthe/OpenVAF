use llvm::IntPredicate::IntNE;
use llvm::{
    LLVMAddCase, LLVMAppendBasicBlockInContext, LLVMBuildAnd, LLVMBuildBr, LLVMBuildCondBr,
    LLVMBuildICmp, LLVMBuildPointerCast, LLVMBuildRet, LLVMBuildSwitch, LLVMCreateBuilderInContext,
    LLVMDisposeBuilder, LLVMGetParam, LLVMPositionBuilderAtEnd, UNNAMED,
};
use mir_llvm::CodegenCx;

use crate::compilation_unit::OsdiCompilationUnit;
use crate::metadata::osdi_0_3::{ACCESS_FLAG_INSTANCE, ACCESS_FLAG_SET};

impl<'ll> OsdiCompilationUnit<'_, 'll> {
    pub fn access_function(&self, cx: &mut CodegenCx<'_, 'll>) -> &'ll llvm::Value {
        let void_ptr = cx.ty_void_ptr();
        let uint32_t = cx.ty_int();
        let fun_ty = cx.ty_func(&[void_ptr, void_ptr, uint32_t, uint32_t], void_ptr);
        let name = &format!("access_{}", &self.sym,);
        let llfunc = cx.declare_int_c_fn(name, fun_ty);
        let inst_data = self.inst_data();
        let model_data = self.model_data();

        unsafe {
            let entry = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let err_exit = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let model_bb = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let inst_bb = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let opvar_bb = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
            let llbuilder = LLVMCreateBuilderInContext(cx.llcx);

            LLVMPositionBuilderAtEnd(llbuilder, entry);

            // get params
            let inst = LLVMGetParam(llfunc, 0);
            let inst = LLVMBuildPointerCast(llbuilder, inst, cx.ptr_ty(inst_data.ty), UNNAMED);
            let model = LLVMGetParam(llfunc, 1);
            let model = LLVMBuildPointerCast(llbuilder, model, cx.ptr_ty(model_data.ty), UNNAMED);
            let param_id = LLVMGetParam(llfunc, 2);
            let flags = LLVMGetParam(llfunc, 3);

            let access_flag_instance = cx.const_unsigned_int(ACCESS_FLAG_INSTANCE);
            let access_flag_set = cx.const_unsigned_int(ACCESS_FLAG_SET);
            let zero = cx.const_unsigned_int(0);

            // check various flags
            let flags_and_instance = LLVMBuildAnd(llbuilder, flags, access_flag_instance, UNNAMED);
            let instance_flag_set =
                LLVMBuildICmp(llbuilder, IntNE, flags_and_instance, zero, UNNAMED);

            let flags_and_set = LLVMBuildAnd(llbuilder, flags, access_flag_set, UNNAMED);
            let write_flag_set = LLVMBuildICmp(llbuilder, IntNE, flags_and_set, zero, UNNAMED);

            LLVMBuildCondBr(llbuilder, instance_flag_set, inst_bb, model_bb);

            // inst params
            LLVMPositionBuilderAtEnd(llbuilder, inst_bb);
            let switch_inst =
                LLVMBuildSwitch(llbuilder, param_id, opvar_bb, inst_data.params.len() as u32);

            for param_idx in 0..inst_data.params.len() {
                let bb = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
                LLVMPositionBuilderAtEnd(llbuilder, bb);
                let case = cx.const_unsigned_int(param_idx as u32);
                LLVMAddCase(switch_inst, case, bb);

                let (ptr, _) = inst_data.nth_param_ptr(param_idx as u32, inst, llbuilder);
                let ptr = LLVMBuildPointerCast(llbuilder, ptr, void_ptr, UNNAMED);

                // set the write flag if given
                let write = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
                let ret = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
                LLVMBuildCondBr(llbuilder, write_flag_set, write, ret);
                LLVMPositionBuilderAtEnd(llbuilder, write);
                inst_data.set_nth_param_given(cx, param_idx as u32, inst, llbuilder);
                LLVMBuildBr(llbuilder, ret);

                // return the poiner
                LLVMPositionBuilderAtEnd(llbuilder, ret);
                LLVMBuildRet(llbuilder, ptr);
            }

            LLVMPositionBuilderAtEnd(llbuilder, model_bb);
            let switch_model =
                LLVMBuildSwitch(llbuilder, param_id, opvar_bb, model_data.params.len() as u32);

            // inst param model default values
            for param_idx in 0..inst_data.params.len() {
                let bb = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
                LLVMPositionBuilderAtEnd(llbuilder, bb);
                let case = cx.const_unsigned_int(param_idx as u32);
                LLVMAddCase(switch_model, case, bb);

                let (ptr, _) =
                    model_data.nth_inst_param_ptr(inst_data, param_idx as u32, model, llbuilder);
                let ptr = LLVMBuildPointerCast(llbuilder, ptr, void_ptr, UNNAMED);

                // set the write flag if given
                let write = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
                let ret = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
                LLVMBuildCondBr(llbuilder, write_flag_set, write, ret);
                LLVMPositionBuilderAtEnd(llbuilder, write);
                model_data.set_nth_param_given(cx, param_idx as u32, model, llbuilder);
                LLVMBuildBr(llbuilder, ret);

                // return the poiner
                LLVMPositionBuilderAtEnd(llbuilder, ret);
                LLVMBuildRet(llbuilder, ptr);
            }

            // model params
            for param_idx in 0..model_data.params.len() {
                let bb = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
                LLVMPositionBuilderAtEnd(llbuilder, bb);
                let case = cx.const_unsigned_int((inst_data.params.len() + param_idx) as u32);
                LLVMAddCase(switch_model, case, bb);

                let (ptr, _) = model_data.nth_param_ptr(param_idx as u32, model, llbuilder);
                let ptr = LLVMBuildPointerCast(llbuilder, ptr, void_ptr, UNNAMED);

                // set the write flag if given
                let write = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
                let ret = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
                LLVMBuildCondBr(llbuilder, write_flag_set, write, ret);
                LLVMPositionBuilderAtEnd(llbuilder, write);
                model_data.set_nth_param_given(cx, param_idx as u32, model, llbuilder);
                LLVMBuildBr(llbuilder, ret);

                // return the poiner
                LLVMPositionBuilderAtEnd(llbuilder, ret);
                LLVMBuildRet(llbuilder, ptr);
            }

            // opvars
            LLVMPositionBuilderAtEnd(llbuilder, opvar_bb);
            let switch_opvar =
                LLVMBuildSwitch(llbuilder, param_id, err_exit, inst_data.opvars.len() as u32);

            for opvar_idx in 0..inst_data.opvars.len() {
                let bb = LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED);
                LLVMPositionBuilderAtEnd(llbuilder, bb);
                let case = cx.const_unsigned_int(
                    (model_data.params.len() + inst_data.params.len() + opvar_idx) as u32,
                );
                LLVMAddCase(switch_opvar, case, bb);

                let (ptr, _) =
                    inst_data.nth_opvar_ptr(cx, self, opvar_idx as u32, inst, model, llbuilder);
                let ptr = LLVMBuildPointerCast(llbuilder, ptr, void_ptr, UNNAMED);

                LLVMBuildRet(llbuilder, ptr);
            }

            //return NULL on unkown id
            LLVMPositionBuilderAtEnd(llbuilder, err_exit);
            LLVMBuildRet(llbuilder, cx.const_null_ptr(void_ptr));

            LLVMDisposeBuilder(llbuilder);
        }

        llfunc
    }
}
