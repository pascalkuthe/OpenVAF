use libc::c_uint;

use crate::module::function_iter;
use crate::util::InvariantOpaque;
use crate::{Bool, Module, OptLevel, PassManager, PassManagerBuilder, Value};

#[repr(C)]
pub struct FunctionPassManager<'a>(InvariantOpaque<'a>);

extern "C" {

    // crate and destroy
    pub fn LLVMPassManagerBuilderCreate() -> &'static mut PassManagerBuilder;
    pub fn LLVMPassManagerBuilderDispose(PMB: &'static mut PassManagerBuilder);

    fn LLVMPassManagerBuilderSetOptLevel(PMB: &PassManagerBuilder, OptLevel: c_uint);
    pub fn LLVMPassManagerBuilderSetSizeLevel(PMB: &PassManagerBuilder, SizeLevel: c_uint);
    fn LLVMPassManagerBuilderSLPVectorize(PMB: &PassManagerBuilder);

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

/// # Safety
/// This should always be save but this low level wrapper purposefully refrains from making Safety
/// guarantees
pub unsafe fn pass_manager_builder_set_opt_lvl(pmb: &PassManagerBuilder, opt_lvl: OptLevel) {
    LLVMPassManagerBuilderSetOptLevel(pmb, opt_lvl as c_uint);
    if opt_lvl > OptLevel::Less {
        LLVMPassManagerBuilderSLPVectorize(pmb);
    }
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
    pub fn LLVMRunPassManager(PM: &PassManager<'static>, M: &Module) -> Bool;

    fn LLVMInitializeFunctionPassManager(FPM: &PassManager<'_>) -> Bool;
    fn LLVMRunFunctionPassManager<'a>(FPM: &PassManager<'a>, F: &'a Value) -> Bool;
    fn LLVMFinalizeFunctionPassManager(FPM: &PassManager<'_>) -> Bool;
}

/// # Safety
/// This function calls the LLVM C Api.
/// If the module or its contents have been incorrectly constructed this can cause UB
/// If the pass manager is not a function pass manager but a global pass manager this will cause UB
pub unsafe fn run_function_pass_manager<'a>(fpm: &PassManager<'a>, module: &'a Module) {
    LLVMInitializeFunctionPassManager(fpm);
    for fun in function_iter(module) {
        LLVMRunFunctionPassManager(fpm, fun);
    }
    LLVMFinalizeFunctionPassManager(fpm);
}
