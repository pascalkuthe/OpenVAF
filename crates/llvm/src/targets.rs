use std::ffi::{CStr, CString};
use std::mem::MaybeUninit;

use ::libc::c_char;

use crate::support::LLVMString;
use crate::{
    Bool, CodeGenFileType, CodeGenOptLevel, CodeModel, Module, RelocMode, Target, TargetMachine,
};

extern "C" {
    fn LLVMGetTargetFromTriple(
        Triple: *const c_char,
        T: &mut Option<&'static Target>,
        ErrorMessage: *mut *mut c_char,
    ) -> Bool;
    fn LLVMCreateTargetMachine(
        T: &Target,
        Triple: *const c_char,
        CPU: *const c_char,
        Features: *const c_char,
        Level: CodeGenOptLevel,
        Reloc: RelocMode,
        CodeModel: CodeModel,
    ) -> Option<&'static mut TargetMachine>;
    pub fn LLVMDisposeTargetMachine(target_machine: &'static mut TargetMachine);
    /// Create a DataLayout based on the target machine.
    pub fn LLVMTargetMachineEmitToFile(
        target: &TargetMachine,
        module: &Module,
        file_name: *mut c_char,
        codegen: CodeGenFileType,
        ErrorMessage: *mut *mut c_char,
    ) -> Bool;
    // pub fn LLVMTargetMachineEmitToMemoryBuffer(
    //     T: LLVMTargetMachineRef,
    //     M: LLVMModuleRef,
    //     codegen: LLVMCodeGenFileType,
    //     ErrorMessage: *mut *mut ::libc::c_char,
    //     OutMemBuf: *mut LLVMMemoryBufferRef,
    // ) -> LLVMBool;

    /// Normalize a target triple. The result needs to be disposed with LLVMDisposeMessage.
    fn LLVMNormalizeTargetTriple(triple: *const c_char) -> *mut c_char;
    fn LLVMSetTarget(module: &Module, triple: *const c_char);
}

/// # Safety
///
/// This function calls the LLVM C interface and may emit unsafety for invalid inputs.
/// Specifically this function is not thread save!
pub unsafe fn create_target(
    triple: &str,
    cpu: &str,
    features: &str,
    level: CodeGenOptLevel,
    reloc_mode: RelocMode,
    code_model: CodeModel,
) -> Result<&'static mut TargetMachine, LLVMString> {
    let triple_ = LLVMString::create_from_str(&CString::new(triple).unwrap());
    let triple_ = LLVMString::new(LLVMNormalizeTargetTriple(triple_.as_ptr()));
    let mut target = None;
    let mut err_string = MaybeUninit::uninit();

    let code = LLVMGetTargetFromTriple(triple_.as_ptr(), &mut target, err_string.as_mut_ptr());

    if code == 1 {
        return Err(LLVMString::new(err_string.assume_init()));
    }

    let cpu = CString::new(cpu).unwrap();
    let features = CString::new(features).unwrap();
    let target = target.unwrap();

    let target_machine = LLVMCreateTargetMachine(
        target,
        triple_.as_ptr(),
        cpu.as_ptr(),
        features.as_ptr(),
        level,
        reloc_mode,
        code_model,
    );

    target_machine.ok_or_else(|| {
        LLVMString::create_from_str(
            CStr::from_bytes_with_nul(
                format!("error: code gen not available for target \"{}\"\0", triple).as_bytes(),
            )
            .unwrap(),
        )
    })
}

/// # Safety
/// This function calls LLVM raw ffi which is implemented in C and may be unsound
pub unsafe fn set_normalized_target(module: &Module, triple: &str) {
    let triple = LLVMString::create_from_str(&CString::new(triple).unwrap());
    let triple = LLVMString::new(LLVMNormalizeTargetTriple(triple.as_ptr()));
    LLVMSetTarget(module, triple.as_ptr())
}
