use libc::{c_char, c_void};

use crate::{Context, DiagnosticHandler, DiagnosticInfo, DiagnosticSeverity};

// Core
extern "C" {
    // pub fn LLVMShutdown();
    pub fn LLVMCreateMessage(Message: *const c_char) -> *mut c_char;
    pub fn LLVMDisposeMessage(Message: *mut c_char);

    pub fn LLVMContextCreate() -> &'static mut Context;
    pub fn LLVMContextDispose(ctx: &'static mut Context);

    pub fn LLVMContextSetDiagnosticHandler(
        ctx: &Context,
        handler: DiagnosticHandler,
        diagnostic_ctx: *mut c_void,
    );

    pub fn LLVMGetDiagInfoDescription(info: &DiagnosticInfo) -> *const c_char;
    pub fn LLVMGetDiagInfoSeverity(info: &DiagnosticInfo) -> DiagnosticSeverity;
}
