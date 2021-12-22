use libc::c_uint;

use crate::module::function_iter;
use crate::util::InvariantOpaque;
use crate::{Bool, Module, PassManager, PassManagerBuilder, Value};

#[repr(C)]
pub struct FunctionPassManager<'a>(InvariantOpaque<'a>);

extern "C" {

    // crate and destroy
    pub fn LLVMPassManagerBuilderCreate() -> &'static mut PassManagerBuilder;
    pub fn LLVMPassManagerBuilderDispose(PMB: &'static mut PassManagerBuilder);

    pub fn LLVMPassManagerBuilderSetOptLevel(PMB: &PassManagerBuilder, OptLevel: c_uint);
    pub fn LLVMPassManagerBuilderSetSizeLevel(PMB: &PassManagerBuilder, SizeLevel: c_uint);

    pub fn LLVMPassManagerBuilderSetDisableUnitAtATime(PMB: &PassManagerBuilder, Value: Bool);
    pub fn LLVMPassManagerBuilderSetDisableUnrollLoops(PMB: &PassManagerBuilder, Value: Bool);
    pub fn LLVMPassManagerBuilderSetDisableSimplifyLibCalls(PMB: &PassManagerBuilder, Value: Bool);
    pub fn LLVMPassManagerBuilderUseInlinerWithThreshold(
        PMB: &PassManagerBuilder,
        threshold: c_uint,
    );
    pub fn LLVMPassManagerBuilderPopulateFunctionPassManager(
        PMB: &PassManagerBuilder,
        PM: &PassManager<'_>,
    );
    pub fn LLVMPassManagerBuilderPopulateModulePassManager(
        PMB: &PassManagerBuilder,
        PM: &PassManager<'_>,
    );
    pub fn LLVMPassManagerBuilderPopulateLTOPassManager(
        PMB: &mut PassManagerBuilder,
        PM: &PassManager<'_>,
        Internalize: Bool,
        RunInliner: Bool,
    );
}

// Core->Pass managers
extern "C" {
    /// Creates a pass manager.
    pub fn LLVMCreatePassManager() -> &'static mut PassManager<'static>;

    /// Creates a function-by-function pass manager
    pub fn LLVMCreateFunctionPassManagerForModule<'a>(M: &'a Module) -> &'a mut PassManager<'a>;

    /// Disposes a pass manager.
    pub fn LLVMDisposePassManager<'a>(PM: &'a mut PassManager<'a>);

    /// Runs a pass manager on a module.
    pub fn LLVMRunPassManager<'a>(PM: &PassManager<'a>, M: &'a Module) -> Bool;

    fn LLVMInitializeFunctionPassManager(FPM: &mut PassManager<'_>) -> Bool;
    fn LLVMRunFunctionPassManager<'a>(FPM: &mut PassManager<'a>, F: &'a Value) -> Bool;
    fn LLVMFinalizeFunctionPassManager(FPM: &mut PassManager<'_>) -> Bool;
}

/// # Safety
/// This function calls the LLVM C Api.
/// If the module or its contents have been incorrectly constructed this can cause UB
/// If the pass manager is not a function pass manager but a global pass manager this will cause UB
pub unsafe fn run_function_pass_manager<'a>(fpm: &mut PassManager<'a>, module: &'a Module) {
    LLVMInitializeFunctionPassManager(fpm);
    for fun in function_iter(module) {
        LLVMRunFunctionPassManager(fpm, fun);
    }
    LLVMFinalizeFunctionPassManager(fpm);
}
