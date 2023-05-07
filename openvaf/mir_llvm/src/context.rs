use std::cell::{Cell, RefCell};
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

use crate::types::Types;

pub struct CodegenCx<'a, 'll> {
    pub llmod: &'ll llvm::Module,
    pub llcx: &'ll llvm::Context,

    pub target: &'a Target,
    // pub target_cpu: &'a str,
    pub literals: &'a Rodeo,
    str_lit_cache: RefCell<AHashMap<Spur, &'ll Value>>,
    pub(crate) intrinsics: RefCell<AHashMap<&'static str, (&'ll Type, &'ll Value)>>,
    pub(crate) local_gen_sym_counter: Cell<u32>,
    pub(crate) tys: Types<'ll>,
}

impl<'a, 'll> CodegenCx<'a, 'll> {
    pub(crate) fn new(
        literals: &'a Rodeo,
        llvm_module: &'ll crate::ModuleLlvm,
        target: &'a Target,
        // target_cpu: &'a str,
    ) -> CodegenCx<'a, 'll> {
        // let ty_isize =
        //     unsafe { llvm::LLVMIntTypeInContext(llvm_module.llcx, target.pointer_width) };
        CodegenCx {
            llmod: llvm_module.llmod(),
            llcx: llvm_module.llcx,
            str_lit_cache: RefCell::new(AHashMap::with_capacity(literals.len())),
            literals,
            intrinsics: RefCell::new(AHashMap::new()),
            local_gen_sym_counter: Cell::new(0),
            // target_cpu,
            target,
            tys: Types::new(llvm_module.llcx, target.pointer_width),
        }
    }

    pub fn get_func_by_name(&self, name: &str) -> Option<&'ll llvm::Value> {
        let name = CString::new(name).unwrap();
        unsafe { LLVMGetNamedFunction(self.llmod, name.as_ptr()) }
    }

    pub fn include_bitcode(&self, bitcode: &[u8]) {
        let sym = self.generate_local_symbol_name("bitcode_buffer");
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

    pub fn const_str_uninterned(&self, lit: &str) -> &'ll Value {
        println!("{lit}");
        let lit = self.literals.get(lit).unwrap();
        self.const_str(lit)
    }

    pub fn const_str(&self, lit: Spur) -> &'ll Value {
        if let Some(val) = self.str_lit_cache.borrow().get(&lit) {
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
        let sym = self.generate_local_symbol_name("str");
        let ty = self.val_ty(val);
        let global = self
            .define_global(&sym, ty)
            .unwrap_or_else(|| unreachable!("symbol {} already defined", sym));

        unsafe {
            llvm::LLVMSetInitializer(global, val);
            llvm::LLVMSetGlobalConstant(global, llvm::True);
            llvm::LLVMSetLinkage(global, llvm::Linkage::Internal);
        }
        self.str_lit_cache.borrow_mut().insert(lit, global);
        global
    }
}

impl CodegenCx<'_, '_> {
    /// Generates a new symbol name with the given prefix. This symbol name must
    /// only be used for definitions with `internal` or `private` linkage.
    pub fn generate_local_symbol_name(&self, prefix: &str) -> String {
        let idx = self.local_gen_sym_counter.get();
        self.local_gen_sym_counter.set(idx + 1);
        // Include a '.' character, so there can be no accidental conflicts with
        // user defined names
        let mut name = String::with_capacity(prefix.len() + 6);
        name.push_str(prefix);
        name.push('.');
        base_n::push_str(idx as u128, base_n::ALPHANUMERIC_ONLY, &mut name);
        name
    }
}
