use ::libc::{c_char, c_uint};

use crate::{BasicBlock, Bool, Builder, Context, IntPredicate, RealPredicate, Type, Value};

// Core->Instruction Builders
extern "C" {
    pub fn LLVMCreateBuilderInContext(ctx: &Context) -> &mut Builder<'_>;
    pub fn LLVMPositionBuilderAtEnd<'a>(builder: &Builder<'a>, block: &'a BasicBlock);
    pub fn LLVMGetInsertBlock<'a>(builder: &Builder<'a>) -> &'a BasicBlock;
    pub fn LLVMDisposeBuilder<'a>(builder: &'a mut Builder<'a>);

    // Terminators
    pub fn LLVMBuildRetVoid<'a>(builder: &Builder<'a>) -> &'a Value;
    pub fn LLVMBuildRet<'a>(builder: &Builder<'a>, val: &'a Value) -> &'a Value;
    // pub fn LLVMBuildAggregateRet(
    //     builder: &Builder<'a>,
    //     RetVals: *mut &'a Value,
    //     N: ::libc::c_uint,
    // ) -> &'a Value;
    pub fn LLVMBuildBr<'a>(builder: &Builder<'a>, dst: &'a BasicBlock) -> &'a Value;
    pub fn LLVMBuildCondBr<'a>(
        builder: &Builder<'a>,
        cond: &'a Value,
        then_bb: &'a BasicBlock,
        else_bb: &'a BasicBlock,
    ) -> &'a Value;

    // Arithmetic
    pub fn LLVMBuildAdd<'a>(
        arg1: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildFAdd<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildSub<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildFSub<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildMul<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;

    pub fn LLVMBuildFMul<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildSDiv<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildFDiv<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;

    pub fn LLVMBuildSRem<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildFRem<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildShl<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildLShr<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildAShr<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildAnd<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildOr<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildXor<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    // pub fn LLVMBuildBinOp<'a>(
    //     B: &Builder<'a>,
    //     Op: Opcode,
    //     LHS: &'a Value,
    //     RHS: &'a Value,
    //     Name: *const c_char,
    // ) -> &'a Value;
    pub fn LLVMBuildNeg<'a>(builder: &Builder<'a>, V: &'a Value, Name: *const c_char) -> &'a Value;
    pub fn LLVMBuildFNeg<'a>(builder: &Builder<'a>, V: &'a Value, Name: *const c_char)
        -> &'a Value;
    pub fn LLVMBuildNot<'a>(builder: &Builder<'a>, V: &'a Value, Name: *const c_char) -> &'a Value;

    // Memory
    // pub fn LLVMBuildMalloc(builder: &Builder<'a>, ty: &'a Type, Name: *const c_char) -> &'a Value;
    // pub fn LLVMBuildArrayMalloc(
    //     builder: &Builder<'a>,
    //     ty: &'a Type,
    //     Val: &'a Value,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildMemSet(
    //     B: &Builder,
    //     Ptr: &'a Value,
    //     Val: &'a Value,
    //     Len: &'a Value,
    //     Align: ::libc::c_uint,
    // ) -> &'a Value;
    // pub fn LLVMBuildMemCpy(
    //     B: &Builder,
    //     Dst: &'a Value,
    //     DstAlign: ::libc::c_uint,
    //     Src: &'a Value,
    //     SrcAlign: ::libc::c_uint,
    //     Size: &'a Value,
    // ) -> &'a Value;
    // pub fn LLVMBuildMemMove(
    //     B: &Builder,
    //     Dst: &'a Value,
    //     DstAlign: ::libc::c_uint,
    //     Src: &'a Value,
    //     SrcAlign: ::libc::c_uint,
    //     Size: &'a Value,
    // ) -> &'a Value;
    pub fn LLVMBuildAlloca<'a>(
        builder: &Builder<'a>,
        ty: &'a Type,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildArrayAlloca<'a>(
        builder: &Builder<'a>,
        ty: &'a Type,
        Val: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildFree<'a>(builder: &Builder<'a>, PointerVal: &'a Value) -> &'a Value;
    pub fn LLVMBuildLoad2<'a>(
        builder: &Builder<'a>,
        ty: &'a Type,
        PointerVal: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildStore<'a>(builder: &Builder<'a>, Val: &'a Value, Ptr: &'a Value) -> &'a Value;

    // pub fn LLVMBuildGlobalString(B: &Builder, Str: *const c_char, Name: *const c_char)
    //     -> &'a Value;
    // pub fn LLVMBuildGlobalStringPtr(
    //     B: &Builder,
    //     Str: *const c_char,
    //     Name: *const c_char,
    // ) -> &'a Value;

    // Casts
    // pub fn LLVMBuildTrunc(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildZExt(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildSExt(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildFPToUI(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildFPToSI(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    pub fn LLVMBuildUIToFP<'a>(
        builder: &Builder<'a>,
        Val: &'a Value,
        Destty: &'a Type,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildSIToFP<'a>(
        builder: &Builder<'a>,
        Val: &'a Value,
        Destty: &'a Type,
        Name: *const c_char,
    ) -> &'a Value;

    pub fn LLVMBuildGEP2<'a>(
        B: &Builder<'a>,
        Ty: &'a Type,
        Pointer: &'a Value,
        Indices: *const &'a Value,
        NumIndices: c_uint,
        Name: *const c_char,
    ) -> &'a Value;

    pub fn LLVMBuildStructGEP2<'a>(
        builder: &Builder<'a>,
        ty: &'a Type,
        ptr: &'a Value,
        idx: u32,
        Name: *const c_char,
    ) -> &'a Value;
    // pub fn LLVMBuildFPTrunc(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildFPExt(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildPtrToInt(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildIntToPtr(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildBitCast(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildAddrSpaceCast(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildZExtOrBitCast(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildSExtOrBitCast(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildTruncOrBitCast(
    //     builder: &Builder<'a>,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    // pub fn LLVMBuildCast<'a>(
    //     B: &Builder,
    //     Op: Opcode,
    //     Val: &'a Value,
    //     Destty: &'a Type,
    //     Name: *const c_char,
    // ) -> &'a Value;
    pub fn LLVMBuildPointerCast<'a>(
        builder: &Builder<'a>,
        Val: &'a Value,
        Destty: &'a Type,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildIntCast2<'a>(
        builder: &Builder<'a>,
        Val: &'a Value,
        Destty: &'a Type,
        IsSigned: Bool,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildFPCast<'a>(
        builder: &Builder<'a>,
        Val: &'a Value,
        Destty: &'a Type,
        Name: *const c_char,
    ) -> &'a Value;

    // Comparisons
    pub fn LLVMBuildICmp<'a>(
        builder: &Builder<'a>,
        Op: IntPredicate,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildFCmp<'a>(
        builder: &Builder<'a>,
        Op: RealPredicate,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;

    // Miscellaneous instructions
    pub fn LLVMBuildPhi<'a>(builder: &Builder<'a>, ty: &'a Type, Name: *const c_char) -> &'a Value;
    pub fn LLVMBuildCall2<'a>(
        builder: &Builder<'a>,
        ty: &'a Type,
        Fn: &'a Value,
        Args: *const &'a Value,
        NumArgs: c_uint,
        Name: *const c_char,
    ) -> &'a Value;

    pub fn LLVMBuildIsNull<'a>(
        builder: &Builder<'a>,
        Val: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildIsNotNull<'a>(
        builder: &Builder<'a>,
        Val: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
    pub fn LLVMBuildPtrDiff<'a>(
        builder: &Builder<'a>,
        LHS: &'a Value,
        RHS: &'a Value,
        Name: *const c_char,
    ) -> &'a Value;
}
