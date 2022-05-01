use std::ffi::CString;

use libc::c_uint;
use llvm::{False, True, Type, Value};
use mir::Const;

use crate::CodegenCx;

impl<'a, 'll> CodegenCx<'a, 'll> {
    pub fn ty_real(&self) -> &'ll Type {
        unsafe { llvm::LLVMDoubleTypeInContext(self.llcx) }
    }

    pub fn ty_cmplx(&self) -> &'ll Type {
        unsafe { llvm::LLVMArrayType(self.ty_real(), 2) }
    }

    pub fn ty_aint(&self, bits: u32) -> &'ll Type {
        unsafe { llvm::LLVMIntTypeInContext(self.llcx, bits) }
    }

    pub fn ty_int(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt32TypeInContext(self.llcx) }
    }

    pub fn ty_int64(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt64TypeInContext(self.llcx) }
    }

    pub fn ty_long(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt64TypeInContext(self.llcx) }
    }

    pub fn ty_isize(&self) -> &'ll Type {
        unsafe { llvm::LLVMIntTypeInContext(self.llcx, self.target.pointer_width) }
    }

    pub fn ty_bool(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt1TypeInContext(self.llcx) }
    }

    pub fn ty_c_bool(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt8TypeInContext(self.llcx) }
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

    pub fn ty_void_ptr(&self) -> &'ll Type {
        // its really an opaque pointer so who cares...
        // TOOD move to opaque pointers
        self.ptr_ty(self.ty_c_bool())
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

    pub fn ptr_ty(&self, elem: &'ll Type) -> &'ll Type {
        unsafe { llvm::LLVMPointerType(elem, llvm::AddressSpace::DATA) }
    }

    pub fn fat_ptr(&self, elem: &'ll Type, always_use_long: bool) -> &'ll Type {
        let ptr = self.ptr_ty(elem);

        let len = if always_use_long { self.ty_long() } else { self.ty_isize() };
        self.struct_ty("fat_ptr", &[ptr, len])
    }

    pub fn const_val(&mut self, val: &Const) -> &'ll Value {
        match *val {
            Const::Float(val) => self.const_real(val.into()),
            Const::Int(val) => self.const_int(val),
            Const::Bool(val) => self.const_bool(val),
            // Const::Complex(ref val) => self.const_cmplx(val),
            Const::Str(val) => self.const_str(val),
        }
    }

    /// # Safety
    /// indicies must be valid and inbounds for the provided ptr
    /// The pointer must be a constant address
    pub unsafe fn const_gep(
        &self,
        ptr: &'ll llvm::Value,
        indicies: &[&'ll llvm::Value],
    ) -> &'ll llvm::Value {
        llvm::LLVMConstGEP(ptr, indicies.as_ptr(), indicies.len() as u32)
    }

    pub fn const_int(&self, val: i32) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_int(), val as u64, True) }
    }

    pub fn const_unsigned_int(&self, val: u32) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_int(), val as u64, True) }
    }

    pub fn const_isize(&self, val: isize) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_isize(), val as u64, True) }
    }

    pub fn const_usize(&self, val: usize) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_isize(), val as u64, False) }
    }

    pub fn const_bool(&self, val: bool) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_bool(), val as u64, False) }
    }

    pub fn const_c_bool(&self, val: bool) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_c_bool(), val as u64, False) }
    }

    pub fn const_u8(&self, val: u8) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_c_bool(), val as u64, False) }
    }

    pub fn const_real(&self, val: f64) -> &'ll Value {
        unsafe { llvm::LLVMConstReal(self.ty_real(), val) }
    }

    pub fn const_arr(&self, elem_ty: &'ll Type, vals: &[&'ll Value]) -> &'ll Value {
        unsafe { llvm::LLVMConstArray(elem_ty, vals.as_ptr(), vals.len() as u32) }
    }

    pub fn const_anon_struct(&self, vals: &[&'ll Value]) -> &'ll Value {
        unsafe {
            llvm::LLVMConstStructInContext(self.llcx, vals.as_ptr(), vals.len() as u32, False)
        }
    }

    pub fn const_struct(&self, ty: &'ll Type, vals: &[&'ll Value]) -> &'ll Value {
        unsafe { llvm::LLVMConstNamedStruct(ty, vals.as_ptr(), vals.len() as u32) }
    }

    pub fn const_null(&self, t: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMConstNull(t) }
    }

    pub fn const_null_ptr(&self, t: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMConstPointerNull(t) }
    }

    pub fn const_undef(&self, t: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMGetUndef(t) }
    }

    pub fn elem_ty(&self, v: &'ll Type) -> &'ll Type {
        unsafe { llvm::LLVMGetElementType(v) }
    }

    pub fn val_ty(&self, v: &'ll Value) -> &'ll Type {
        unsafe { llvm::LLVMTypeOf(v) }
    }

    pub fn struct_ty(&self, name: &str, elements: &[&'ll Type]) -> &'ll Type {
        let name = CString::new(name).unwrap();
        unsafe {
            let ty = llvm::LLVMStructCreateNamed(self.llcx, name.as_ptr());
            llvm::LLVMStructSetBody(ty, elements.as_ptr(), elements.len() as u32, False);
            ty
        }
    }
}
