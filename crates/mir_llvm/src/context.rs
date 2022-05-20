use std::ffi::CString;

use ahash::AHashMap;
use lasso::{Rodeo, Spur};
use libc::{c_char, c_uint};
use llvm::support::LLVMString;
use llvm::{
    LLVMCreateMemoryBufferWithMemoryRange, LLVMGetNamedFunction, LLVMLinkModules2,
    LLVMParseBitcodeInContext2, Type, Value,
};
use target::spec::Target;

pub struct CodegenCx<'a, 'll> {
    pub llmod: &'ll llvm::Module,
    pub llcx: &'ll llvm::Context,

    pub target: &'a Target,
    pub target_cpu: &'a str,
    pub literals: &'a Rodeo,
    str_lit_cache: AHashMap<Spur, &'ll Value>,
    pub(crate) intrinsics: AHashMap<&'static str, (&'ll Type, &'ll Value)>,
    pub(crate) local_gen_sym_counter: usize,
}

impl<'a, 'll> CodegenCx<'a, 'll> {
    pub(crate) fn new(
        literals: &'a Rodeo,
        llvm_module: &'ll crate::ModuleLlvm,
        target: &'a Target,
        target_cpu: &'a str,
    ) -> CodegenCx<'a, 'll> {
        // let ty_isize =
        //     unsafe { llvm::LLVMIntTypeInContext(llvm_module.llcx, target.pointer_width) };
        CodegenCx {
            llmod: llvm_module.llmod(),
            llcx: llvm_module.llcx,
            str_lit_cache: AHashMap::with_capacity(literals.len()),
            literals,
            intrinsics: AHashMap::new(),
            local_gen_sym_counter: 0,
            target_cpu,
            // ty_isize,
            target,
        }
    }

    pub fn get_func_by_name(&self, name: &str) -> Option<&'ll llvm::Value> {
        let name = CString::new(name).unwrap();
        unsafe { LLVMGetNamedFunction(self.llmod, name.as_ptr()) }
    }

    pub fn include_bitcode(&mut self, bitcode: &[u8]) {
        let sym =
            Self::generate_local_symbol_name(&mut self.local_gen_sym_counter, "bitcode_buffer");
        let sym = CString::new(sym).unwrap();
        unsafe {
            let buff = LLVMCreateMemoryBufferWithMemoryRange(
                bitcode.as_ptr() as *const c_char,
                bitcode.len(),
                sym.as_ptr(),
                llvm::False,
            );
            let mut module = None;
            assert!(
                LLVMParseBitcodeInContext2(self.llcx, buff, &mut module) == llvm::False,
                "failed to parse bitcode"
            );
            assert!(
                LLVMLinkModules2(self.llmod, module.unwrap()) == llvm::False,
                "failed to link parsed bitcode"
            );
        }
    }

    pub fn to_str(&self) -> LLVMString {
        unsafe { LLVMString::new(llvm::LLVMPrintModuleToString(self.llmod)) }
    }

    pub fn const_str_uninterned(&mut self, lit: &str) -> &'ll Value {
        let lit = self.literals.get(lit).unwrap();
        self.const_str(lit)
    }

    pub fn const_str(&mut self, lit: Spur) -> &'ll Value {
        if let Some(val) = self.str_lit_cache.get(&lit) {
            return val;
        }

        let val = self.literals.resolve(&lit).as_bytes().to_owned();

        // assert!(!val.contains(&b'\0'));
        // val.push(b'\0');
        let val = unsafe {
            llvm::LLVMConstStringInContext(
                self.llcx,
                val.as_ptr() as *const c_char,
                val.len() as c_uint,
                false as llvm::Bool,
            )
        };
        let sym = Self::generate_local_symbol_name(&mut self.local_gen_sym_counter, "str");
        let ty = self.val_ty(val);
        let global = self
            .define_global(&sym, ty)
            .unwrap_or_else(|| unreachable!("symbol {} already defined", sym));

        unsafe {
            llvm::LLVMSetInitializer(global, val);
            llvm::LLVMSetGlobalConstant(global, llvm::True);
            llvm::LLVMSetLinkage(global, llvm::Linkage::InternalLinkage);
        }
        let res = self.ptrcast(global, self.ty_str());
        self.str_lit_cache.insert(lit, res);
        res
    }

    pub fn ptrcast(&self, val: &'ll Value, ty: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMConstPointerCast(val, ty) }
    }
}

impl CodegenCx<'_, '_> {
    /// Generates a new symbol name with the given prefix. This symbol name must
    /// only be used for definitions with `internal` or `private` linkage.
    pub fn generate_local_symbol_name(local_gen_sym_counter: &mut usize, prefix: &str) -> String {
        let idx = *local_gen_sym_counter;
        *local_gen_sym_counter += 1;
        // Include a '.' character, so there can be no accidental conflicts with
        // user defined names
        let mut name = String::with_capacity(prefix.len() + 6);
        name.push_str(prefix);
        name.push('.');
        base_n::push_str(idx as u128, base_n::ALPHANUMERIC_ONLY, &mut name);
        name
    }
}
