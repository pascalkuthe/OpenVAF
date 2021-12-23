use std::ffi::CString;

use lasso::RodeoResolver;
use llvm::support::LLVMString;
use llvm::CodeGenOptLevel;
use target::spec::Target;

mod builder;
mod context;
mod declarations;
mod intrinsics;
mod types;

pub use builder::Builder;
pub use context::CodegenCx;

pub struct LLVMBackend<'t> {
    target: &'t Target,
    opt_lvl: CodeGenOptLevel,
}

impl<'t> LLVMBackend<'t> {
    pub fn new(
        cg_opts: &[String],
        target: &'t Target,
        opt_lvl: CodeGenOptLevel,
    ) -> LLVMBackend<'t> {
        // TODO add target options here if we ever have any
        llvm::initialization::init(cg_opts, &[]);

        LLVMBackend { target, opt_lvl }
    }

    /// # Safety
    /// This function calls the LLVM-C Api which may not be entirely safe.
    /// Exercise caution!
    pub unsafe fn new_module(&self, name: &str) -> Result<ModuleLlvm, LLVMString> {
        ModuleLlvm::new(name, self.target, self.opt_lvl)
    }

    /// # Safety
    /// This function calls the LLVM-C Api which may not be entirely safe.
    /// Exercise caution!
    pub unsafe fn new_ctx<'a, 'll>(
        &'a self,
        literals: &'a RodeoResolver,
        module: &'ll ModuleLlvm,
    ) -> CodegenCx<'a, 'll> {
        CodegenCx::new(literals, module, self.target)
    }
}

pub struct ModuleLlvm {
    llcx: &'static mut llvm::Context,
    // must be a raw pointer because the reference must not outlife self/the context
    llmod_raw: *const llvm::Module,
    tm: &'static mut llvm::TargetMachine,
}

impl ModuleLlvm {
    unsafe fn new(
        name: &str,
        target: &Target,
        lvl: CodeGenOptLevel,
    ) -> Result<ModuleLlvm, LLVMString> {
        let llcx = llvm::LLVMContextCreate();
        let target_data_layout = target.data_layout.clone();

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
            llvm::RelocMode::Default,
            llvm::CodeModel::Default,
        )?;
        let llmod_raw = llmod as _;

        Ok(ModuleLlvm { llcx, llmod_raw, tm })
    }

    pub fn llmod(&self) -> &llvm::Module {
        unsafe { &*self.llmod_raw }
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
