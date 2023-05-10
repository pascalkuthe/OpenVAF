use std::ffi::CString;

use libc::c_uint;
use llvm::{False, LLVMInt8TypeInContext, True, Type, Value};
use mir::Const;

use crate::CodegenCx;

pub struct Types<'ll> {
    pub double: &'ll Type,
    pub char: &'ll Type,
    pub int: &'ll Type,
    pub size: &'ll Type,
    pub ptr: &'ll Type,
    pub fat_ptr: &'ll Type,
    pub bool: &'ll Type,
    pub void: &'ll Type,
    pub null_ptr_val: &'ll llvm::Value,
}

impl<'ll> Types<'ll> {
    pub fn new(llcx: &'ll llvm::Context, pointer_width: u32) -> Types<'ll> {
        unsafe {
            let char = LLVMInt8TypeInContext(llcx);
            // we are using opaque pointers, with old llvm version that plain
            // means always using char pointers, with newer llvm version the
            // type is ignored anyway
            let ptr = llvm::LLVMPointerType(char, llvm::AddressSpace::DATA);
            let size = llvm::LLVMIntTypeInContext(llcx, pointer_width);
            Types {
                double: llvm::LLVMDoubleTypeInContext(llcx),
                char,
                int: llvm::LLVMInt32TypeInContext(llcx),
                size,
                ptr,
                fat_ptr: ty_struct(llcx, "fat_ptr", &[ptr, llvm::LLVMInt64TypeInContext(llcx)]),
                bool: llvm::LLVMInt1TypeInContext(llcx),
                void: llvm::LLVMVoidTypeInContext(llcx),
                null_ptr_val: llvm::LLVMConstPointerNull(ptr),
            }
        }
    }
}

fn ty_struct<'ll>(llcx: &'ll llvm::Context, name: &str, elements: &[&'ll Type]) -> &'ll Type {
    let name = CString::new(name).unwrap();
    unsafe {
        let ty = llvm::LLVMStructCreateNamed(llcx, name.as_ptr());
        llvm::LLVMStructSetBody(ty, elements.as_ptr(), elements.len() as u32, False);
        ty
    }
}

impl<'a, 'll> CodegenCx<'a, 'll> {
    #[inline(always)]
    pub fn ty_double(&self) -> &'ll Type {
        self.tys.double
    }
    #[inline(always)]
    pub fn ty_int(&self) -> &'ll Type {
        self.tys.int
    }
    #[inline(always)]
    pub fn ty_char(&self) -> &'ll Type {
        self.tys.char
    }
    #[inline(always)]
    pub fn ty_size(&self) -> &'ll Type {
        self.tys.size
    }
    #[inline(always)]
    pub fn ty_bool(&self) -> &'ll Type {
        self.tys.bool
    }
    #[inline(always)]
    pub fn ty_c_bool(&self) -> &'ll Type {
        self.tys.char
    }
    #[inline(always)]
    pub fn ty_ptr(&self) -> &'ll Type {
        self.tys.ptr
    }
    #[inline(always)]
    pub fn ty_void(&self) -> &'ll Type {
        self.tys.void
    }
    #[inline(always)]
    pub fn ty_fat_ptr(&self) -> &'ll Type {
        self.tys.fat_ptr
    }
    pub fn ty_aint(&self, bits: u32) -> &'ll Type {
        unsafe { llvm::LLVMIntTypeInContext(self.llcx, bits) }
    }

    pub fn ty_struct(&self, name: &str, elements: &[&'ll Type]) -> &'ll Type {
        ty_struct(self.llcx, name, elements)
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

    pub fn const_val(&self, val: &Const) -> &'ll Value {
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
        elem_ty: &'ll llvm::Type,
        ptr: &'ll llvm::Value,
        indicies: &[&'ll llvm::Value],
    ) -> &'ll llvm::Value {
        llvm::LLVMConstInBoundsGEP2(elem_ty, ptr, indicies.as_ptr(), indicies.len() as u32)
    }

    pub fn const_int(&self, val: i32) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_int(), val as u64, True) }
    }

    pub fn const_unsigned_int(&self, val: u32) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_int(), val as u64, True) }
    }

    pub fn const_isize(&self, val: isize) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_size(), val as u64, True) }
    }

    pub fn const_usize(&self, val: usize) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(self.ty_size(), val as u64, False) }
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
        unsafe { llvm::LLVMConstReal(self.ty_double(), val) }
    }

    pub fn const_arr(&self, elem_ty: &'ll Type, vals: &[&'ll Value]) -> &'ll Value {
        unsafe { llvm::LLVMConstArray(elem_ty, vals.as_ptr(), vals.len() as u32) }
    }

    pub fn const_struct(&self, ty: &'ll Type, vals: &[&'ll Value]) -> &'ll Value {
        unsafe { llvm::LLVMConstNamedStruct(ty, vals.as_ptr(), vals.len() as u32) }
    }

    pub fn const_null(&self, t: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMConstNull(t) }
    }

    pub fn const_null_ptr(&self) -> &'ll Value {
        self.tys.null_ptr_val
    }

    pub fn const_undef(&self, t: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMGetUndef(t) }
    }

    pub fn val_ty(&self, v: &'ll Value) -> &'ll Type {
        unsafe { llvm::LLVMTypeOf(v) }
    }
}
