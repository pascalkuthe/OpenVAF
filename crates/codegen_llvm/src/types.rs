use cfg::Const;
use libc::c_uint;
use llvm::{False, True, Type, Value};

use crate::CodegenCx;

impl<'a, 'll> CodegenCx<'a, 'll> {
    pub fn ty_real(&self) -> &'ll Type {
        unsafe { llvm::LLVMDoubleTypeInContext(self.llcx) }
    }

    pub fn ty_cmplx(&self) -> &'ll Type {
        unsafe { llvm::LLVMArrayType(self.ty_real(), 2) }
    }

    pub fn ty_int(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt32TypeInContext(self.llcx) }
    }

    pub fn ty_bool(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt1TypeInContext(self.llcx) }
    }

    pub fn ty_str(&self) -> &'ll Type {
        unsafe {
            let char_ty = llvm::LLVMInt8TypeInContext(self.llcx);
            llvm::LLVMPointerType(char_ty, llvm::AddressSpace::DATA)
        }
    }

    pub fn ty_void(&self) -> &'ll Type {
        unsafe { llvm::LLVMVoidTypeInContext(self.llcx) }
    }

    pub fn ty_func(&self, args: &[&'ll Type], ret: &'ll Type) -> &'ll Type {
        unsafe { llvm::LLVMFunctionType(ret, args.as_ptr(), args.len() as c_uint, False) }
    }

    pub fn ty_variadic_func(&self, args: &[&'ll Type], ret: &'ll Type) -> &'ll Type {
        unsafe { llvm::LLVMFunctionType(ret, args.as_ptr(), args.len() as c_uint, True) }
    }

    pub fn ty_array(&self, ty: &'ll Type, len: u32) -> &'ll Type {
        unsafe { llvm::LLVMArrayType(ty, len) }
    }

    pub fn zst(&self) -> &'ll Type {
        unsafe { llvm::LLVMIntTypeInContext(self.llcx, 0) }
    }

    pub fn const_val(&mut self, val: &Const) -> &'ll Value {
        match *val {
            Const::Real(val) => self.const_real(val),
            Const::Int(val) => self.const_int(val),
            Const::Bool(val) => self.const_bool(val),
            // Const::Complex(ref val) => self.const_cmplx(val),
            Const::String(val) => self.const_str(val),
            Const::Zst => self.const_zst(),
        }
    }

    pub fn const_zst(&self) -> &'ll Value {
        self.const_undef(self.zst())
    }

    pub fn const_int(&self, val: i32) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_int(), val as u64, True) }
    }

    pub fn const_bool(&self, val: bool) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_bool(), val as u64, False) }
    }

    pub fn const_real(&self, val: f64) -> &'ll Value {
        unsafe { llvm::LLVMConstReal(self.ty_real(), val) }
    }

    // pub fn const_cmplx(&self, val: &Complex64) -> &'ll Value {
    //     let real = self.const_real(val.real);
    //     let imag = self.const_real(val.imag);
    //     let vals = [real, imag];
    //     unsafe { llvm::LLVMConstArray(self.ty_real(), vals.as_mut_ptr(), 2) }
    // }

    pub fn const_null(&self, t: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMConstNull(t) }
    }

    pub fn const_undef(&self, t: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMGetUndef(t) }
    }

    pub fn val_ty(&self, v: &'ll Value) -> &'ll Type {
        unsafe { llvm::LLVMTypeOf(v) }
    }
}
