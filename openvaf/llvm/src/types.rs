use std::mem::forget;

use ::libc::{c_char, c_uint};

use crate::{Bool, Context, Type, TypeKind};

extern "C" {
    pub fn LLVMGetTypeKind(ty: &Type) -> TypeKind;
    // pub fn LLVMTypeIsSized(Ty: &'a Type) -> LLVMBool;
    // pub fn LLVMGetTypeContext(Ty: &'a Type) -> ContextRef;
    // pub fn LLVMDumpType(Val: &'a Type);
    pub fn LLVMPrintTypeToString(Val: &Type) -> *mut c_char;

    // Core->Types->Integer
    pub fn LLVMInt1TypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    pub fn LLVMInt8TypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMInt16TypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    pub fn LLVMInt32TypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    pub fn LLVMInt64TypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMInt128TypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    pub fn LLVMIntTypeInContext<'a>(ctx: &'a Context, num_bits: c_uint) -> &'a Type;
    // pub fn LLVMGetIntTypeWidth<'a>(ty: &Type) -> c_uint;

    // Core->Types->Floating-Point
    // pub fn LLVMHalfTypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMBFloatTypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMFloatTypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    pub fn LLVMDoubleTypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMX86FP80TypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMFP128TypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMPPCFP128TypeInContext<'a>(ctx: &'a Context) -> &'a Type;

    // Core->Types->Function
    pub fn LLVMFunctionType<'a>(
        ReturnType: &'a Type,
        ParamTypes: *const &'a Type,
        ParamCount: c_uint,
        IsVarArg: Bool,
    ) -> &'a Type;
    pub fn LLVMIsFunctionVarArg<'a>(fun_ty: &'a Type) -> Bool;
    pub fn LLVMGetReturnType<'a>(fun_ty: &'a Type) -> &'a Type;
    pub fn LLVMCountParamTypes<'a>(fun_ty: &'a Type) -> c_uint;
    pub fn LLVMGetParamTypes<'a>(fun_ty: &'a Type, dst: *mut &'a Type);

    // Core->Types->Struct
    pub fn LLVMStructTypeInContext<'a>(
        ctx: &'a Context,
        ElementTypes: *const &'a Type,
        ElementCount: c_uint,
        Packed: Bool,
    ) -> &'a Type;
    pub fn LLVMStructCreateNamed<'a>(ctx: &'a Context, Name: *const c_char) -> &'a Type;
    pub fn LLVMGetStructName<'a>(ty: &'a Type) -> *const c_char;
    pub fn LLVMStructSetBody<'a>(
        struct_ty: &'a Type,
        ElementTypes: *const &'a Type,
        ElementCount: c_uint,
        Packed: Bool,
    );
    pub fn LLVMCountStructElementTypes(struct_ty: &Type) -> c_uint;
    fn LLVMGetStructElementTypes<'a>(struct_ty: &'a Type, dst: *mut &'a Type);
    ///// Get the type of the element at the given index in a structure.
    /////
    ///// Added in LLVM 3.7.
    pub fn LLVMStructGetTypeAtIndex<'a>(struct_ty: &'a Type, i: c_uint) -> &'a Type;
    ///// Determine whether a structure is packed.
    //pub fn LLVMIsPackedStruct(struct_ty: &Type) -> Bool;
    //pub fn LLVMIsOpaqueStruct(struct_ty: &Type) -> Bool;
    //pub fn LLVMIsLiteralStruct(struct_ty: &Type) -> Bool;

    //// Core->Types->Sequential
    // pub fn LLVMGetElementType<'a>(ty: &'a Type) -> &'a Type;
    ///// Get the subtypes of the given type.
    //pub fn LLVMGetSubtypes<'a>(ty: &'a Type, arr: *mut &'a Type);
    ///// Return the number of types in the derived type.
    //pub fn LLVMGetNumContainedTypes<'a>(ty: &'a Type) -> c_uint;
    pub fn LLVMArrayType<'a>(elem: &'a Type, elem_cnt: c_uint) -> &'a Type;
    // pub fn LLVMGetArrayLength(ArrayTy: &'a Type) -> c_uint;
    pub fn LLVMPointerType<'a>(elem: &'a Type, address_space: AddressSpace) -> &'a Type;
    // pub fn LLVMGetPointerAddressSpace(PointerTy: &'a Type) -> c_uint;
    // pub fn LLVMVectorType(ElementType: &'a Type, ElementCount: c_uint) -> &'a Type;
    /// Create a vector type that contains a defined type and has a scalable
    /// number of elements.
    ///
    /// The created type will exist in the context that its element type
    /// exists in.
    // pub fn LLVMScalableVectorType(ElementType: &'a Type, ElementCount: c_uint) -> &'a Type;
    /// Obtain the (possibly scalable) number of elements in a vector type.
    // pub fn LLVMGetVectorSize(VectorTy: &'a Type) -> c_uint;

    // Core->Types->Other
    pub fn LLVMVoidTypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMLabelTypeInContext<'a>(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMX86MMXTypeInContext(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMX86AMXTypeInContext(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMTokenTypeInContext(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMMetadataTypeInContext(ctx: &'a Context) -> &'a Type;
    // pub fn LLVMVoidType() -> &'a Type;
    // pub fn LLVMLabelType() -> &'a Type;
    // pub fn LLVMX86MMXType() -> &'a Type;
    // pub fn LLVMX86AMXType() -> &'a Type;
}

/// An identifier that specifies the address space that some operation
/// should operate on. Special address spaces have an effect on code generation,
/// depending on the target and the address spaces it implements.
#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AddressSpace(pub c_uint);

impl AddressSpace {
    /// The default address space, corresponding to data space.
    pub const DATA: Self = AddressSpace(0);
}

/// # Safety
/// struct_ty must be a valid struct type
pub unsafe fn struct_element_types(struct_ty: &Type) -> Box<[&Type]> {
    let count = LLVMCountStructElementTypes(struct_ty);

    let mut raw_vec: Vec<&Type> = Vec::with_capacity(count as usize);
    let ptr = raw_vec.as_mut_ptr();
    forget(raw_vec);

    LLVMGetStructElementTypes(struct_ty, ptr);
    Vec::from_raw_parts(ptr, count as usize, count as usize).into_boxed_slice()
}
