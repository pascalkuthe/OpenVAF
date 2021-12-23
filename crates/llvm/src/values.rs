use libc::{c_char, c_double, c_uint, c_ulonglong};

use crate::{
    BasicBlock, Bool, CallConv, Context, Linkage, Module, Type, UnnamedAddr, Value, Visibility,
};

// Core->Values
extern "C" {
    // Core->Values->General
    // Get the enumerated kind of a Value instance.
    pub fn LLVMTypeOf(val: &Value) -> &Type;

    // pub fn LLVMGetValueName2(val: &'a Value, Length: *mut ::libc::size_t) -> *const ::libc::c_char;
    // pub fn LLVMSetValueName2(val: &'a Value, Name: *const ::libc::c_char, NameLen: ::libc::size_t);

    // pub fn LLVMDumpValue(Val: &'a Value);
    pub fn LLVMPrintValueToString(val: &Value) -> *mut c_char;
    pub fn LLVMReplaceAllUsesWith<'a>(old_val: &'a Value, new_val: &'a Value);
    /// Determine whether the specified value instance is constant.
    // pub fn LLVMIsConstant(Val: &'a Value) -> LLVMBool;
    // pub fn LLVMIsUndef(Val: &'a Value) -> LLVMBool;
    // /// Determine whether a value instance is poisonous.
    // pub fn LLVMIsPoison(Val: &'a Value) -> LLVMBool;
    // pub fn LLVMIsAMDNode(Val: &'a Value) -> &'a Value;
    // pub fn LLVMIsAMDString(Val: &'a Value) -> &'a Value;

    // Core->Values->Usage
    // pub fn LLVMGetFirstUse(Val: &'a Value) -> LLVMUseRef;
    // pub fn LLVMGetNextUse(U: LLVMUseRef) -> LLVMUseRef;
    // pub fn LLVMGetUser(U: LLVMUseRef) -> &'a Value;
    // pub fn LLVMGetUsedValue(U: LLVMUseRef) -> &'a Value;

    // Core->Values->User value
    // pub fn LLVMGetOperand(Val: &'a Value, Index: ::libc::c_uint) -> &'a Value;
    // pub fn LLVMGetOperandUse(Val: &'a Value, Index: ::libc::c_uint) -> LLVMUseRef;
    // pub fn LLVMSetOperand(User: &'a Value, Index: ::libc::c_uint, Val: &'a Value);
    // pub fn LLVMGetNumOperands(Val: &'a Value) -> ::libc::c_int;

    // Core->Values->Constants
    pub fn LLVMConstNull(ty: &Type) -> &Value;
    pub fn LLVMConstAllOnes(ty: &Type) -> &Value;
    pub fn LLVMGetUndef(ty: &Type) -> &Value;
    /// Obtain a constant value referring to a poison value of a type.
    // pub fn LLVMGetPoison(Ty: TypeRef) -> &'a Value;
    // pub fn LLVMIsNull(Val: &'a Value) -> LLVMBool;
    // pub fn LLVMConstPointerNull(Ty: TypeRef) -> &'a Value;

    // Core->Values->Constants->Scalar
    pub fn LLVMConstInt(ty: &Type, val: c_ulonglong, sign_extend: Bool) -> &Value;
    // pub fn LLVMConstIntOfArbitraryPrecision(
    //     IntTy: TypeRef,
    //     NumWords: ::libc::c_uint,
    //     Words: *const u64,
    // ) -> &'a Value;
    // pub fn LLVMConstIntOfString(
    //     IntTy: TypeRef,
    //     Text: *const ::libc::c_char,
    //     Radix: u8,
    // ) -> &'a Value;
    // pub fn LLVMConstIntOfStringAndSize(
    //     IntTy: TypeRef,
    //     Text: *const ::libc::c_char,
    //     SLen: ::libc::c_uint,
    //     Radix: u8,
    // ) -> &'a Value;
    pub fn LLVMConstReal(RealTy: &Type, val: c_double) -> &Value;
    // pub fn LLVMConstRealOfString(RealTy: TypeRef, Text: *const ::libc::c_char) -> &'a Value;
    // pub fn LLVMConstRealOfStringAndSize(
    //     RealTy: TypeRef,
    //     Text: *const ::libc::c_char,
    //     SLen: ::libc::c_uint,
    // ) -> &'a Value;
    // pub fn LLVMConstIntGetZExtValue(ConstantVal: &'a Value) -> ::libc::c_ulonglong;
    // pub fn LLVMConstIntGetSExtValue(ConstantVal: &'a Value) -> ::libc::c_longlong;
    // pub fn LLVMConstRealGetDouble(
    //     ConstantVal: &'a Value,
    //     losesInfo: *mut LLVMBool,
    // ) -> ::libc::c_double;

    // Core->Values->Constants->Composite
    pub fn LLVMConstStringInContext(
        C: &Context,
        Str: *const c_char,
        Length: c_uint,
        DontNullTerminate: Bool,
    ) -> &Value;
    // pub fn LLVMConstString(
    //     Str: *const ::libc::c_char,
    //     Length: ::libc::c_uint,
    //     DontNullTerminate: LLVMBool,
    // ) -> &'a Value;
    // pub fn LLVMIsConstantString(c: &'a Value) -> LLVMBool;
    // pub fn LLVMGetAsString(C: &'a Value, Length: *mut ::libc::size_t) -> *const ::libc::c_char;
    pub fn LLVMConstStructInContext<'a>(
        C: &'a Context,
        ConstantVals: *mut &'a Value,
        Count: c_uint,
        Packed: Bool,
    ) -> &'a Value;
    pub fn LLVMConstArray<'a>(element_ty: &'a Type, vals: *mut &'a Value, len: c_uint)
        -> &'a Value;
    pub fn LLVMConstNamedStruct<'a>(
        ty: &'a Type,
        ConstantVals: *mut &'a Value,
        Count: c_uint,
    ) -> &'a Value;
    // pub fn LLVMGetElementAsConstant(C: &'a Value, idx: ::libc::c_uint) -> &'a Value;
    // pub fn LLVMConstVector(ScalarConstantVals: *mut &'a Value, Size: ::libc::c_uint) -> &'a Value;

    // Core->Values->Constants->Constant expressions
    // pub fn LLVMGetConstOpcode(ConstantVal: &'a Value) -> Opcode;
    // pub fn LLVMAlignOf(Ty: TypeRef) -> &'a Value;
    // pub fn LLVMSizeOf(Ty: TypeRef) -> &'a Value;
    // pub fn LLVMConstNeg(ConstantVal: &'a Value) -> &'a Value;
    // pub fn LLVMConstNSWNeg(ConstantVal: &'a Value) -> &'a Value;
    // pub fn LLVMConstNUWNeg(ConstantVal: &'a Value) -> &'a Value;
    // pub fn LLVMConstFNeg(ConstantVal: &'a Value) -> &'a Value;
    // pub fn LLVMConstNot(ConstantVal: &'a Value) -> &'a Value;
    // pub fn LLVMConstAdd(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstNSWAdd(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstNUWAdd(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstFAdd(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstSub(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstNSWSub(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstNUWSub(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstFSub(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstMul(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstNSWMul(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstNUWMul(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstFMul(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstUDiv(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstExactUDiv(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstSDiv(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstExactSDiv(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstFDiv(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstURem(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstSRem(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstFRem(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstAnd(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstOr(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstXor(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstICmp(
    //     Predicate: LLVMIntPredicate,
    //     LHSConstant: &'a Value,
    //     RHSConstant: &'a Value,
    // ) -> &'a Value;
    // pub fn LLVMConstFCmp(
    //     Predicate: LLVMRealPredicate,
    //     LHSConstant: &'a Value,
    //     RHSConstant: &'a Value,
    // ) -> &'a Value;
    // pub fn LLVMConstShl(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstLShr(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstAShr(LHSConstant: &'a Value, RHSConstant: &'a Value) -> &'a Value;
    // pub fn LLVMConstGEP<'a>(
    //     ConstantVal: &'a Value,
    //     ConstantIndices: *mut &'a Value,
    //     NumIndices: c_uint,
    // ) -> &'a Value;
    // pub fn LLVMConstGEP2(
    //     Ty: TypeRef,
    //     ConstantVal: &'a Value,
    //     ConstantIndices: *mut &'a Value,
    //     NumIndices: ::libc::c_uint,
    // ) -> &'a Value;
    // pub fn LLVMConstInBoundsGEP(
    //     ConstantVal: &'a Value,
    //     ConstantIndices: *mut &'a Value,
    //     NumIndices: ::libc::c_uint,
    // ) -> &'a Value;
    // pub fn LLVMConstInBoundsGEP2(
    //     Ty: TypeRef,
    //     ConstantVal: &'a Value,
    //     ConstantIndices: *mut &'a Value,
    //     NumIndices: ::libc::c_uint,
    // ) -> &'a Value;
    // pub fn LLVMConstTrunc(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstSExt(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstZExt(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstFPTrunc(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstFPExt(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstUIToFP(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstSIToFP(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstFPToUI(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstFPToSI(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstPtrToInt(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstIntToPtr(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstBitCast(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstAddrSpaceCast(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstZExtOrBitCast(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstSExtOrBitCast(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstTruncOrBitCast(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    pub fn LLVMConstPointerCast<'a>(const_val: &'a Value, ty: &'a Type) -> &'a Value;
    // pub fn LLVMConstIntCast<'a>(
    //     ConstantVal: &'a Value,
    //     ToType: &'a Type,
    //     isSigned: Bool,
    // ) -> &'a Value;
    // pub fn LLVMConstFPCast(ConstantVal: &'a Value, ToType: TypeRef) -> &'a Value;
    // pub fn LLVMConstSelect(
    //     ConstantCondition: &'a Value,
    //     ConstantIfTrue: &'a Value,
    //     ConstantIfFalse: &'a Value,
    // ) -> &'a Value;
    // pub fn LLVMConstExtractElement(
    //     VectorConstant: &'a Value,
    //     IndexConstant: &'a Value,
    // ) -> &'a Value;
    // pub fn LLVMConstInsertElement(
    //     VectorConstant: &'a Value,
    //     ElementValueConstant: &'a Value,
    //     IndexConstant: &'a Value,
    // ) -> &'a Value;
    // pub fn LLVMConstShuffleVector(
    //     VectorAConstant: &'a Value,
    //     VectorBConstant: &'a Value,
    //     MaskConstant: &'a Value,
    // ) -> &'a Value;
    // pub fn LLVMConstExtractValue(
    //     AggConstant: &'a Value,
    //     IdxList: *mut ::libc::c_uint,
    //     NumIdx: ::libc::c_uint,
    // ) -> &'a Value;
    // pub fn LLVMConstInsertValue(
    //     AggConstant: &'a Value,
    //     ElementValueConstant: &'a Value,
    //     IdxList: *mut ::libc::c_uint,
    //     NumIdx: ::libc::c_uint,
    // ) -> &'a Value;
    // pub fn LLVMBlockAddress(F: &'a Value, BB: LLVMBasicBlockRef) -> &'a Value;

    // Core->Values->Constants->Global Values
    // pub fn LLVMGetGlobalParent(global: &'a Value) -> ModuleRef;
    pub fn LLVMIsDeclaration(global: &Value) -> Bool;
    // pub fn LLVMGetLinkage(global: &Value) -> Linkage;
    pub fn LLVMSetLinkage(global: &Value, Linkage: Linkage);
    // pub fn LLVMGetSection(global: &'a Value) -> *const ::libc::c_char;
    // pub fn LLVMSetSection(global: &'a Value, Section: *const ::libc::c_char);
    // pub fn LLVMGetVisibility(global: &'a Value) -> Visibility;
    pub fn LLVMSetVisibility(global: &Value, viz: Visibility);
    // pub fn LLVMGetDLLStorageClass(global: &'a Value) -> LLVMDLLStorageClass;
    // pub fn LLVMSetDLLStorageClass(global: &'a Value, Class: LLVMDLLStorageClass);

    // pub fn LLVMGetUnnamedAddress(global: &'a Value) -> LLVMUnnamedAddr;
    pub fn LLVMSetUnnamedAddress(global: &Value, UnnamedAddr: UnnamedAddr);
    // pub fn LLVMGlobalGetValueType(global: &'a Value) -> TypeRef;

    // pub fn LLVMGetAlignment(V: &'a Value) -> ::libc::c_uint;
    pub fn LLVMSetAlignment(val: &Value, align: c_uint);

    // pub fn LLVMGlobalSetMetadata(global: &'a Value, Kind: ::libc::c_uint, MD: &'a Metadata);
    // pub fn LLVMGlobalEraseMetadata(global: &'a Value, Kind: ::libc::c_uint);
    // pub fn LLVMGlobalClearMetadata(global: &'a Value);
    // pub fn LLVMGlobalCopyAllMetadata(
    //     Value: &'a Value,
    //     NumEntries: *mut ::libc::size_t,
    // ) -> *mut LLVMValueMetadataEntry;
    // pub fn LLVMDisposeValueMetadataEntries(Entries: *mut LLVMValueMetadataEntry);
    // pub fn LLVMValueMetadataEntriesGetKind(
    //     Entries: *mut LLVMValueMetadataEntry,
    //     Index: ::libc::c_uint,
    // ) -> ::libc::c_uint;
    // pub fn LLVMValueMetadataEntriesGetMetadata(
    //     Entries: *mut LLVMValueMetadataEntry,
    //     Index: ::libc::c_uint,
    // ) -> &'a Metadata;

    // // Core->Values->Constants->Global Variables
    pub fn LLVMAddGlobal<'a>(module: &'a Module, ty: &'a Type, name: *const c_char) -> &'a Value;
    // pub fn LLVMAddGlobalInAddressSpace(
    //     M: ModuleRef,
    //     Ty: TypeRef,
    //     Name: *const ::libc::c_char,
    //     AddressSpace: ::libc::c_uint,
    // ) -> &'a Value;
    pub fn LLVMGetNamedGlobal(module: &Module, name: *const c_char) -> Option<&Value>;
    // pub fn LLVMGetFirstGlobal(M: ModuleRef) -> &'a Value;
    // pub fn LLVMGetLastGlobal(M: ModuleRef) -> &'a Value;
    // pub fn LLVMGetNextGlobal(GlobalVar: &'a Value) -> &'a Value;
    // pub fn LLVMGetPreviousGlobal(GlobalVar: &'a Value) -> &'a Value;
    // pub fn LLVMDeleteGlobal(GlobalVar: &'a Value);
    // pub fn LLVMGetInitializer(GlobalVar: &'a Value) -> &'a Value;
    pub fn LLVMSetInitializer<'a>(global: &'a Value, const_val: &'a Value);
    // pub fn LLVMIsThreadLocal(GlobalVar: &'a Value) -> LLVMBool;
    // pub fn LLVMSetThreadLocal(GlobalVar: &'a Value, IsThreadLocal: LLVMBool);
    // pub fn LLVMIsGlobalConstant(GlobalVar: &'a Value) -> LLVMBool;
    pub fn LLVMSetGlobalConstant<'a>(GlobalVar: &'a Value, IsConstant: Bool);
    // pub fn LLVMGetThreadLocalMode(GlobalVar: &'a Value) -> LLVMThreadLocalMode;
    // pub fn LLVMSetThreadLocalMode(GlobalVar: &'a Value, Mode: LLVMThreadLocalMode);
    // pub fn LLVMIsExternallyInitialized(GlobalVar: &'a Value) -> LLVMBool;
    // pub fn LLVMSetExternallyInitialized(GlobalVar: &'a Value, IsExtInit: LLVMBool);

    //// Core->Values->Constants->Global Aliases
    // /// Obtain a GlobalAlias value from a Module by its name.
    // ///
    // /// The returned value corresponds to a llvm::GlobalAlias value.
    //pub fn LLVMGetNamedGlobalAlias(
    //    M: ModuleRef,
    //    Name: *const ::libc::c_char,
    //    NameLen: ::libc::size_t,
    //) -> &'a Value;
    ///// Obtain an iterator to the first GlobalAlias in a Module.
    //pub fn LLVMGetFirstGlobalAlias(M: ModuleRef) -> &'a Value;
    ///// Obtain an iterator to the last GlobalAlias in a Module.
    //pub fn LLVMGetLastGlobalAlias(M: ModuleRef) -> &'a Value;
    ///// Advance a GlobalAlias iterator to the next GlobalAlias.
    /////
    ///// Returns NULL if the iterator was already at the end and there are no more global aliases.
    //pub fn LLVMGetNextGlobalAlias(GA: &'a Value) -> &'a Value;
    ///// Decrement a GlobalAlias iterator to the previous GlobalAlias.
    /////
    ///// Returns NULL if the iterator was already at the beginning and there are no previous global aliases.
    //pub fn LLVMGetPreviousGlobalAlias(GA: &'a Value) -> &'a Value;
    ///// Retrieve the target value of an alias.
    //pub fn LLVMAliasGetAliasee(Alias: &'a Value) -> &'a Value;
    ///// Set the target value of an alias.
    //pub fn LLVMAliasSetAliasee(Alias: &'a Value, Aliasee: &'a Value);

    //pub fn LLVMAddAlias(
    //    M: ModuleRef,
    //    Ty: TypeRef,
    //    Aliasee: &'a Value,
    //    Name: *const ::libc::c_char,
    //) -> &'a Value;

    //..->Function Values
    //// pub fn LLVMDeleteFunction(Fn: &'a Value);
    ///// Check whether the given function has a personality function.
    //// pub fn LLVMHasPersonalityFn(Fn: &'a Value) -> LLVMBool;
    ///// Obtain the personality function attached to the function.
    /////
    ///// Added in LLVM 3.7.
    //pub fn LLVMGetPersonalityFn(Fn: &'a Value) -> &'a Value;
    ///// Set the personality function attached to the function.
    /////
    ///// Added in LLVM 3.7.
    //pub fn LLVMSetPersonalityFn(Fn: &'a Value, PersonalityFn: &'a Value);
    ///// Obtain the intrinsic ID number which matches the given function name.
    //pub fn LLVMLookupIntrinsicID(
    //    Name: *const ::libc::c_char,
    //    NameLen: ::libc::size_t,
    //) -> ::libc::c_uint;
    ///// Obtain the ID number from a function instance.
    //pub fn LLVMGetIntrinsicID(Fn: &'a Value) -> ::libc::c_uint;
    //pub fn LLVMGetIntrinsicDeclaration(
    //    Mod: ModuleRef,
    //    ID: ::libc::c_uint,
    //    ParamTypes: *mut TypeRef,
    //    ParamCount: ::libc::size_t,
    //) -> &'a Value;
    //pub fn LLVMIntrinsicGetType(
    //    Ctx: ContextRef,
    //    ParamTypes: *mut TypeRef,
    //    ParamCount: ::libc::size_t,
    //) -> TypeRef;
    //pub fn LLVMIntrinsicGetName(
    //    ID: ::libc::c_uint,
    //    NameLength: *mut ::libc::size_t,
    //) -> *const ::libc::c_char;
    //pub fn LLVMIntrinsicCopyOverloadedName2(
    //    Mod: ModuleRef,
    //    ID: ::libc::c_uint,
    //    ParamTypes: *mut TypeRef,
    //    ParamCount: ::libc::size_t,
    //    NameLength: *mut ::libc::size_t,
    //) -> *const ::libc::c_char;
    //pub fn LLVMIntrinsicIsOverloaded(ID: ::libc::c_uint) -> LLVMBool;
    //pub fn LLVMGetFunctionCallConv(Fn: &'a Value) -> ::libc::c_uint;
    pub fn LLVMSetFunctionCallConv(fun: &Value, cc: CallConv);
    //pub fn LLVMGetGC(Fn: &'a Value) -> *const ::libc::c_char;
    //pub fn LLVMSetGC(Fn: &'a Value, Name: *const ::libc::c_char);
    //pub fn LLVMAddAttributeAtIndex(F: &'a Value, Idx: LLVMAttributeIndex, A: LLVMAttributeRef);
    //pub fn LLVMGetAttributeCountAtIndex(F: &'a Value, Idx: LLVMAttributeIndex) -> ::libc::c_uint;
    //pub fn LLVMGetAttributesAtIndex(
    //    F: &'a Value,
    //    Idx: LLVMAttributeIndex,
    //    Attrs: *mut LLVMAttributeRef,
    //);
    //pub fn LLVMGetEnumAttributeAtIndex(
    //    F: &'a Value,
    //    Idx: LLVMAttributeIndex,
    //    KindID: ::libc::c_uint,
    //) -> LLVMAttributeRef;
    //pub fn LLVMGetStringAttributeAtIndex(
    //    F: &'a Value,
    //    Idx: LLVMAttributeIndex,
    //    K: *const ::libc::c_char,
    //    KLen: ::libc::c_uint,
    //) -> LLVMAttributeRef;
    //pub fn LLVMRemoveEnumAttributeAtIndex(
    //    F: &'a Value,
    //    Idx: LLVMAttributeIndex,
    //    KindID: ::libc::c_uint,
    //);
    //pub fn LLVMRemoveStringAttributeAtIndex(
    //    F: &'a Value,
    //    Idx: LLVMAttributeIndex,
    //    K: *const ::libc::c_char,
    //    KLen: ::libc::c_uint,
    //);
    //pub fn LLVMAddTargetDependentFunctionAttr(
    //    Fn: &'a Value,
    //    A: *const ::libc::c_char,
    //    V: *const ::libc::c_char,
    //);

    // ..->Function Values->Function Parameters
    // pub fn LLVMCountParams(Fn: &'a Value) -> ::libc::c_uint;
    // pub fn LLVMGetParams(Fn: &'a Value, Params: *mut &'a Value);
    pub fn LLVMGetParam(fun: &Value, index: c_uint) -> &Value;

    // pub fn LLVMGetParamParent(Inst: &'a Value) -> &'a Value;
    // pub fn LLVMGetFirstParam(Fn: &'a Value) -> &'a Value;
    // pub fn LLVMGetLastParam(Fn: &'a Value) -> &'a Value;
    // pub fn LLVMGetNextParam(Arg: &'a Value) -> &'a Value;
    // pub fn LLVMGetPreviousParam(Arg: &'a Value) -> &'a Value;
    // pub fn LLVMSetParamAlignment(Arg: &'a Value, Align: ::libc::c_uint);
    pub fn LLVMSetPartialFastMath(val: &Value);
    pub fn LLVMSetFastMath(val: &Value);

    // Instruction->PHI Nodes
    pub fn LLVMAddIncoming<'a>(
        PhiNode: &'a Value,
        IncomingValues: *const &'a Value,
        IncomingBlocks: *const &'a BasicBlock,
        Count: c_uint,
    );
}
