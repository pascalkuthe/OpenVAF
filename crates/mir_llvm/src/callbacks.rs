use llvm::UNNAMED;

use crate::CodegenCx;

#[derive(Clone)]
pub struct CallbackFun<'ll> {
    pub fun_ty: &'ll llvm::Type,
    pub fun: &'ll llvm::Value,
    /// Some Callbacks need to read/modify some state (typicaly passed as pointers)
    /// outside of the arguments provided in Verilog-A.
    /// These arguments are always passed before any arguments specified in the CFG
    pub state: Box<[&'ll llvm::Value]>,
}

impl<'ll> CodegenCx<'_, 'll> {
    pub fn const_callback(
        &mut self,
        args: &[&'ll llvm::Type],
        val: &'ll llvm::Value,
    ) -> CallbackFun<'ll> {
        let name = self.local_callback_name();
        let fun_ty = self.ty_func(args, self.val_ty(val));
        let fun = self.declare_int_fn(&name, fun_ty);
        unsafe {
            let bb = llvm::LLVMAppendBasicBlockInContext(self.llcx, fun, UNNAMED);
            let builder = llvm::LLVMCreateBuilderInContext(self.llcx);
            llvm::LLVMPositionBuilderAtEnd(builder, bb);
            llvm::LLVMBuildRet(builder, val);
            llvm::LLVMDisposeBuilder(builder);
        }

        CallbackFun { fun_ty, fun, state: Box::new([]) }
    }

    pub fn trivial_callbacks(&mut self, args: &[&'ll llvm::Type]) -> CallbackFun<'ll> {
        let name = self.local_callback_name();
        let fun_ty = self.ty_func(args, self.ty_void());
        let fun = self.declare_int_fn(&name, fun_ty);
        unsafe {
            let bb = llvm::LLVMAppendBasicBlockInContext(self.llcx, fun, UNNAMED);
            let builder = llvm::LLVMCreateBuilderInContext(self.llcx);
            llvm::LLVMPositionBuilderAtEnd(builder, bb);
            llvm::LLVMBuildRetVoid(builder);
            llvm::LLVMDisposeBuilder(builder);
        }

        CallbackFun { fun_ty, fun, state: Box::new([]) }
    }

    pub fn const_return(&mut self, args: &[&'ll llvm::Type], idx: usize) -> CallbackFun<'ll> {
        let name = self.local_callback_name();
        let fun_ty = self.ty_func(args, args[idx]);
        let fun = self.declare_int_fn(&name, fun_ty);
        unsafe {
            let bb = llvm::LLVMAppendBasicBlockInContext(self.llcx, fun, UNNAMED);
            let builder = llvm::LLVMCreateBuilderInContext(self.llcx);
            llvm::LLVMPositionBuilderAtEnd(builder, bb);
            let val = llvm::LLVMGetParam(fun, idx as u32);
            llvm::LLVMBuildRet(builder, val);
            llvm::LLVMDisposeBuilder(builder);
        }
        CallbackFun { fun_ty, fun, state: Box::new([]) }
    }

    pub fn local_callback_name(&mut self) -> String {
        Self::generate_local_symbol_name(&mut self.local_gen_sym_counter, "cb")
    }
}
