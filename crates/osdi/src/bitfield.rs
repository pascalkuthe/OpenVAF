use std::mem::size_of;

use llvm::IntPredicate::IntNE;
use llvm::{
    LLVMBuildAnd, LLVMBuildGEP2, LLVMBuildICmp, LLVMBuildLoad2, LLVMBuildOr, LLVMBuildStore,
    UNNAMED,
};
use mir_llvm::CodegenCx;

type WORD = u32;

const WORD_BYTES: u32 = size_of::<WORD>() as u32;
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
