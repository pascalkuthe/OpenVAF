use std::ffi::CString;
use std::mem::MaybeUninit;
use std::path::Path;
use std::ptr;

use lasso::Rodeo;
use libc::c_void;
use llvm::support::LLVMString;
pub use llvm::OptLevel;
use llvm::{LLVMGetDiagInfoDescription, LLVMGetDiagInfoSeverity};
use target::spec::Target;

mod builder;
mod context;
mod declarations;
mod intrinsics;
mod types;

mod callbacks;
#[cfg(test)]
mod tests;

pub use builder::Builder;
pub use callbacks::CallbackFun;
pub use context::CodegenCx;

pub struct LLVMBackend<'t> {
    target: &'t Target,
    opt_lvl: OptLevel,
    builder: Option<&'static mut llvm::PassManagerBuilder>,
}

impl<'t> LLVMBackend<'t> {
    pub fn new(cg_opts: &[String], target: &'t Target, opt_lvl: OptLevel) -> LLVMBackend<'t> {
        // TODO add target options here if we ever have any
        llvm::initialization::init(cg_opts, &[]);
        let builder = unsafe {
            let builder = llvm::LLVMPassManagerBuilderCreate();
            llvm::pass_manager_builder_set_opt_lvl(builder, opt_lvl);
            llvm::LLVMPassManagerBuilderSetSizeLevel(builder, 0);
            builder
        };

        LLVMBackend { target, opt_lvl, builder: Some(builder) }
    }

    /// # Safety
    ///
    /// This function calls the LLVM-C Api which may not be entirely safe.
    /// Exercise caution!
    pub unsafe fn new_module(&self, name: &str) -> Result<ModuleLlvm, LLVMString> {
        ModuleLlvm::new(name, self.target, self.opt_lvl)
    }

    /// # Safety
    ///
    /// This function calls the LLVM-C Api which may not be entirely safe.
    /// Exercise caution!
    pub unsafe fn new_ctx<'a, 'll>(
        &'a self,
        literals: &'a Rodeo,
        module: &'ll ModuleLlvm,
    ) -> CodegenCx<'a, 'll> {
        CodegenCx::new(literals, module, self.target)
    }
}

impl Drop for LLVMBackend<'_> {
    fn drop(&mut self) {
        if let Some(builder) = self.builder.take() {
            unsafe { llvm::LLVMPassManagerBuilderDispose(builder) }
        }
    }
}

extern "C" fn diagnostic_handler(info: &llvm::DiagnosticInfo, _: *mut c_void) {
    let severity = unsafe { LLVMGetDiagInfoSeverity(info) };
    if !cfg!(debug_assertions) && severity != llvm::DiagnosticSeverity::Error {
        return;
    }
    let msg = unsafe { LLVMString::new(LLVMGetDiagInfoDescription(info)) };
    println!("LLVM({severity:?}): {msg}")
}

pub struct ModuleLlvm {
    llcx: &'static mut llvm::Context,
    // must be a raw pointer because the reference must not outlife self/the context
    llmod_raw: *const llvm::Module,
    tm: &'static mut llvm::TargetMachine,
}

impl ModuleLlvm {
    unsafe fn new(name: &str, target: &Target, lvl: OptLevel) -> Result<ModuleLlvm, LLVMString> {
        let llcx = llvm::LLVMContextCreate();
        let target_data_layout = target.data_layout.clone();

        llvm::LLVMContextSetDiagnosticHandler(llcx, Some(diagnostic_handler), ptr::null_mut());

        let name = CString::new(name).unwrap();
        let llmod = llvm::LLVMModuleCreateWithNameInContext(name.as_ptr(), llcx);

        let data_layout = CString::new(&*target_data_layout).unwrap();
        llvm::LLVMSetDataLayout(llmod, data_layout.as_ptr());
        llvm::set_normalized_target(llmod, &*target.llvm_target);

        let tm = llvm::create_target(
            &target.llvm_target,
            &target.options.cpu,
            &target.options.features,
            lvl,
            llvm::RelocMode::PIC,
            llvm::CodeModel::Default,
        )?;
        let llmod_raw = llmod as _;

        Ok(ModuleLlvm { llcx, llmod_raw, tm })
    }

    pub fn to_str(&self) -> LLVMString {
        unsafe { LLVMString::new(llvm::LLVMPrintModuleToString(self.llmod())) }
    }

    pub fn llmod(&self) -> &llvm::Module {
        unsafe { &*self.llmod_raw }
    }

    pub fn optimize(&self, backend: &LLVMBackend) {
        let builder = backend.builder.as_ref().unwrap();
        let llmod = self.llmod();
        unsafe {
            let fpm = llvm::LLVMCreateFunctionPassManagerForModule(llmod);
            llvm::LLVMPassManagerBuilderPopulateFunctionPassManager(builder, fpm);
            llvm::run_function_pass_manager(fpm, llmod);
            llvm::LLVMDisposePassManager(fpm);

            let mpm = llvm::LLVMCreatePassManager();
            llvm::LLVMPassManagerBuilderPopulateModulePassManager(builder, mpm);
            llvm::LLVMRunPassManager(mpm, llmod);
            llvm::LLVMDisposePassManager(mpm);
        }
    }

    /// Verifies this module and prints out  any errors
    ///
    /// # Returns
    /// Whether this module is valid (ture if valid)
    pub fn verify_and_print(&self) -> bool {
        unsafe {
            llvm::LLVMVerifyModule(self.llmod(), llvm::VerifierFailureAction::PrintMessage, None)
                == llvm::False
        }
    }

    /// Verifies this module and prints out an error for any errors
    ///
    /// # Returns
    /// An error messages in case the module invalid
    pub fn verify(&self) -> Option<LLVMString> {
        unsafe {
            let mut res = MaybeUninit::uninit();
            if llvm::LLVMVerifyModule(
                self.llmod(),
                llvm::VerifierFailureAction::ReturnStatus,
                Some(&mut res),
            ) == llvm::True
            {
                Some(res.assume_init())
            } else {
                None
            }
        }
    }

    pub fn emit_obect(&self, dst: &Path) -> Result<(), LLVMString> {
        let path = CString::new(dst.to_str().unwrap()).unwrap();

        let mut err_string = MaybeUninit::uninit();
        let return_code = unsafe {
            // REVIEW: Why does LLVM need a mutable ptr to path...?

            llvm::LLVMTargetMachineEmitToFile(
                self.tm,
                self.llmod(),
                path.as_ptr(),
                llvm::CodeGenFileType::ObjectFile,
                err_string.as_mut_ptr(),
            )
        };

        if return_code == 1 {
            unsafe {
                return Err(LLVMString::new(err_string.assume_init()));
            }
        }

        Ok(())
    }
}

impl Drop for ModuleLlvm {
    fn drop(&mut self) {
        unsafe {
            llvm::LLVMDisposeTargetMachine(&mut *(self.tm as *mut _));
            llvm::LLVMContextDispose(&mut *(self.llcx as *mut _));
        }
    }
}
