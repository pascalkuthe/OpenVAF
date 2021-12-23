use std::ffi::CString;

use libc::c_char;
use llvm::{False, Type, Value};

use crate::CodegenCx;

/// Declare a function.
///
/// If there’s a value with the same name already declared, the function will
/// update the declaration and return existing Value instead.
pub fn declare_raw_fn<'ll>(
    cx: &CodegenCx<'_, 'll>,
    name: &str,
    callconv: llvm::CallConv,
    unnamed: llvm::UnnamedAddr,
    ty: &'ll Type,
) -> &'ll Value {
    let name = CString::new(name).unwrap();
    unsafe {
        let llfn = llvm::LLVMAddFunction(cx.llmod, name.as_ptr() as *const c_char, ty);

        llvm::LLVMSetFunctionCallConv(llfn, callconv);
        llvm::LLVMSetUnnamedAddress(llfn, unnamed);
        llfn
    }
}

impl<'a, 'll> CodegenCx<'a, 'll> {
    /// Declare a C ABI function.
    ///
    /// Only use this for foreign function ABIs and glue. For Rust functions use
    /// `declare_fn` instead.
    ///
    /// If there’s a value with the same name already declared, the function will
    /// update the declaration and return existing Value instead.
    pub fn declare_ext_fn(
        &self,
        name: &str,
        // unnamed: llvm::UnnamedAddr,
        fn_type: &'ll Type,
    ) -> &'ll Value {
        declare_raw_fn(self, name, llvm::CallConv::CCallConv, llvm::UnnamedAddr::No, fn_type)
    }

    /// Declare a internal function.
    pub fn declare_int_fn(&self, name: &str, fn_type: &'ll Type) -> &'ll Value {
        // Function addresses are never significant, allowing functions to be merged.
        declare_raw_fn(self, name, llvm::CallConv::FastCallConv, llvm::UnnamedAddr::Global, fn_type)
    }

    /// Declare a global with an intention to define it.
    ///
    /// Use this function when you intend to define a global. This function will
    /// return `None` if the name already has a definition associated with it.
    pub fn define_global(&self, name: &str, ty: &'ll Type) -> Option<&'ll Value> {
        if self.get_defined_value(name).is_some() {
            None
        } else {
            let name = CString::new(name).unwrap();
            let global = unsafe { llvm::LLVMAddGlobal(self.llmod, ty, name.as_ptr()) };
            Some(global)
        }
    }

    /// Declare a private global
    ///
    /// Use this function when you intend to define a global without a name.
    pub fn define_private_global(&self, ty: &'ll Type) -> &'ll Value {
        unsafe {
            let global = llvm::LLVMAddGlobal(self.llmod, ty, llvm::UNNAMED);
            llvm::LLVMSetLinkage(global, llvm::Linkage::PrivateLinkage);
            global
        }
    }

    /// Gets declared value by name.
    pub fn get_declared_value(&self, name: &str) -> Option<&'ll Value> {
        let name = CString::new(name).unwrap();
        unsafe { llvm::LLVMGetNamedGlobal(self.llmod, name.as_ptr()) }
    }

    /// Gets defined or externally defined (AvailableExternally linkage) value by
    /// name.
    pub fn get_defined_value(&self, name: &str) -> Option<&'ll Value> {
        self.get_declared_value(name).and_then(|val| {
            let declaration = unsafe { llvm::LLVMIsDeclaration(val) != False };
            if !declaration {
                Some(val)
            } else {
                None
            }
        })
    }
}
