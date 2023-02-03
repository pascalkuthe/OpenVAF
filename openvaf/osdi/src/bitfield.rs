use std::mem::size_of;

use llvm::IntPredicate::{IntEQ, IntNE};
use llvm::{
    LLVMBuildAnd, LLVMBuildGEP2, LLVMBuildICmp, LLVMBuildLoad2, LLVMBuildOr, LLVMBuildStore,
    UNNAMED,
};
use mir_llvm::{CodegenCx, MemLoc};

type Word = u32;

const WORD_BYTES: u32 = size_of::<Word>() as u32;
const WORD_BITS: u32 = WORD_BYTES * 8;

fn word_index_and_mask(pos: u32) -> (u32, u32) {
    let word_index = pos / WORD_BITS;
    let mask = 1 << (pos % WORD_BITS);
    (word_index, mask)
}

fn word_cnt(len: u32) -> u32 {
    (len as u32 + WORD_BITS - 1) / WORD_BITS
}

pub fn arr_ty<'ll>(len: u32, cx: &CodegenCx<'_, 'll>) -> &'ll llvm::Type {
    cx.ty_array(cx.ty_int(), word_cnt(len))
}

pub unsafe fn word_ptr_and_mask<'ll>(
    cx: &CodegenCx<'_, 'll>,
    pos: u32,
    arr_ptr: &'ll llvm::Value,
    arr_ty: &'ll llvm::Type,
    llbuilder: &llvm::Builder<'ll>,
) -> (&'ll llvm::Value, &'ll llvm::Value) {
    let (idx, mask) = word_index_and_mask(pos);
    let zero = cx.const_int(0);
    let pos = cx.const_unsigned_int(idx);
    let word_ptr = LLVMBuildGEP2(llbuilder, arr_ty, arr_ptr, [zero, pos].as_ptr(), 2, UNNAMED);
    let mask = cx.const_unsigned_int(mask);
    (word_ptr, mask)
}

pub unsafe fn is_set<'ll>(
    cx: &CodegenCx<'_, 'll>,
    pos: u32,
    arr_ptr: &'ll llvm::Value,
    arr_ty: &'ll llvm::Type,
    llbuilder: &llvm::Builder<'ll>,
) -> &'ll llvm::Value {
    let (ptr, mask) = word_ptr_and_mask(cx, pos, arr_ptr, arr_ty, llbuilder);
    let word = LLVMBuildLoad2(llbuilder, cx.ty_int(), ptr, UNNAMED);
    let is_set = LLVMBuildAnd(llbuilder, word, mask, UNNAMED);
    let zero = cx.const_int(0);
    LLVMBuildICmp(llbuilder, IntNE, is_set, zero, UNNAMED)
}

pub unsafe fn set_bit<'ll>(
    cx: &CodegenCx<'_, 'll>,
    pos: u32,
    arr_ptr: &'ll llvm::Value,
    arr_ty: &'ll llvm::Type,
    llbuilder: &llvm::Builder<'ll>,
) {
    let (ptr, mask) = word_ptr_and_mask(cx, pos, arr_ptr, arr_ty, llbuilder);
    let mut word = LLVMBuildLoad2(llbuilder, cx.ty_int(), ptr, UNNAMED);
    word = LLVMBuildOr(llbuilder, word, mask, UNNAMED);
    LLVMBuildStore(llbuilder, word, ptr);
}

pub unsafe fn is_flag_set_mem<'ll>(
    cx: &CodegenCx<'_, 'll>,
    flag: u32,
    val: &MemLoc<'ll>,
    llbuilder: &llvm::Builder<'ll>,
) -> &'ll llvm::Value {
    is_flag_set(cx, flag, val.read(llbuilder), llbuilder)
}

// pub unsafe fn is_flag_unset_mem<'ll>(
//     cx: &CodegenCx<'_, 'll>,
//     flag: u32,
//     val: MemLoc<'ll>,
//     llbuilder: &llvm::Builder<'ll>,
// ) -> &'ll llvm::Value {
//     is_flag_unset(cx, flag, val.read(llbuilder), llbuilder)
// }

pub unsafe fn is_flag_set<'ll>(
    cx: &CodegenCx<'_, 'll>,
    flag: u32,
    val: &'ll llvm::Value,
    llbuilder: &llvm::Builder<'ll>,
) -> &'ll llvm::Value {
    let mask = cx.const_unsigned_int(flag);
    let bits = LLVMBuildAnd(llbuilder, mask, val, UNNAMED);
    LLVMBuildICmp(llbuilder, IntNE, bits, cx.const_int(0), UNNAMED)
}

pub unsafe fn is_flag_unset<'ll>(
    cx: &CodegenCx<'_, 'll>,
    flag: u32,
    val: &'ll llvm::Value,
    llbuilder: &llvm::Builder<'ll>,
) -> &'ll llvm::Value {
    let mask = cx.const_unsigned_int(flag);
    let bits = LLVMBuildAnd(llbuilder, mask, val, UNNAMED);
    LLVMBuildICmp(llbuilder, IntEQ, bits, cx.const_int(0), UNNAMED)
}
