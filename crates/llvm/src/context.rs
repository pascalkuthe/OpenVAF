use libc::c_char;

use crate::Context;

// Core
extern "C" {
    // pub fn LLVMShutdown();
    pub fn LLVMCreateMessage(Message: *const c_char) -> *mut c_char;
    pub fn LLVMDisposeMessage(Message: *mut c_char);

    pub fn LLVMContextCreate() -> &'static mut Context;
    pub fn LLVMContextDispose(ctx: &'static mut Context);
}
