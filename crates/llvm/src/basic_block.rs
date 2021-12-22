use libc::c_char;

use crate::{BasicBlock, Context, Value};

// Core->Basic Block
extern "C" {
    // pub fn LLVMBasicBlockAsValue(BB: &BasicBlock) -> &'a Value;
    // pub fn LLVMValueIsBasicBlock(Val: &'a Value) -> LLVMBool;
    // pub fn LLVMValueAsBasicBlock(Val: &'a Value) -> & BasicBlock;
    //
    /// Get the string name of a basic block.
    // pub fn LLVMGetBasicBlockName(BB: & BasicBlock) -> *const ::libc::c_char;
    // pub fn LLVMGetBasicBlockParent(BB: & BasicBlock) -> &'a Value;
    // pub fn LLVMGetBasicBlockTerminator(BB: & BasicBlock) -> &'a Value;
    // pub fn LLVMCountBasicBlocks(Fn: &'a Value) -> ::libc::c_uint;
    // pub fn LLVMGetBasicBlocks(Fn: &'a Value, BasicBlocks: *mut & BasicBlock);
    pub fn LLVMGetFirstBasicBlock(fun: &Value) -> &BasicBlock;
    // pub fn LLVMGetLastBasicBlock(Fn: &'a Value) -> & BasicBlock;
    // pub fn LLVMGetNextBasicBlock(BB: & BasicBlock) -> & BasicBlock;
    // pub fn LLVMGetPreviousBasicBlock(BB: & BasicBlock) -> & BasicBlock;
    // pub fn LLVMGetEntryBasicBlock(Fn: &'a Value) -> & BasicBlock;
    /// Insert the given basic block after the insertion point of the given builder.
    // pub fn LLVMInsertExistingBasicBlockAfterInsertBlock(
    //     Builder: LLVMBuilderRef,
    //     BB: & BasicBlock,
    // );
    /// Append the given basic block to the basic block list of the given function.
    // pub fn LLVMAppendExistingBasicBlock(Fn: &'a Value, BB: & BasicBlock);
    pub fn LLVMAppendBasicBlockInContext<'a>(
        ctx: &'a Context,
        fun: &'a Value,
        Name: *const c_char,
    ) -> &'a BasicBlock;
    // pub fn LLVMAppendBasicBlock(Fn: &'a Value, Name: *const ::libc::c_char) -> & BasicBlock;
    // pub fn LLVMInsertBasicBlockInContext(
    //     C: ContextRef,
    //     BB: & BasicBlock,
    //     Name: *const ::libc::c_char,
    // ) -> & BasicBlock;
    // pub fn LLVMInsertBasicBlock(
    //     InsertBeforeBB: & BasicBlock,
    //     Name: *const ::libc::c_char,
    // ) -> & BasicBlock;
    // pub fn LLVMDeleteBasicBlock(BB: & BasicBlock);
    // pub fn LLVMRemoveBasicBlockFromParent(BB: & BasicBlock);
    // pub fn LLVMMoveBasicBlockBefore(BB: & BasicBlock, MovePos: & BasicBlock);
    // pub fn LLVMMoveBasicBlockAfter(BB: & BasicBlock, MovePos: & BasicBlock);
    // pub fn LLVMGetFirstInstruction(BB: & BasicBlock) -> &'a Value;
    // pub fn LLVMGetLastInstruction(BB: & BasicBlock) -> &'a Value;
}
