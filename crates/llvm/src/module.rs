use std::iter;

use libc::{c_char, size_t};

use crate::{Context, Module, Type, Value};

// Core->Modules
extern "C" {
    pub fn LLVMModuleCreateWithNameInContext<'a>(
        ModuleID: *const c_char,
        C: &'a Context,
    ) -> &'a Module;

    /// Set the original source file name of a module to a string Name with length Len.
    pub fn LLVMSetSourceFileName(module: &Module, name: *const c_char, len: size_t);

    pub fn LLVMSetDataLayout(module: &Module, DataLayoutStr: *const c_char);

    // /// Returns the module flags as an array of flag-key-value triples.  The caller is responsible for freeing this array by calling LLVMDisposeModuleFlagsMetadata.
    // pub fn LLVMCopyModuleFlagsMetadata(
    //     module: &Module,
    //     Len: *mut size_t,
    // ) -> *mut LLVMModuleFlagEntry;
    // /// Destroys module flags metadata entries.
    // pub fn LLVMDisposeModuleFlagsMetadata(Entries: *mut LLVMModuleFlagEntry);
    // /// Returns the flag behavior for a module flag entry at a specific index.
    // pub fn LLVMModuleFlagEntriesGetFlagBehavior(
    //     Entries: *mut LLVMModuleFlagEntry,
    //     Index: c_uint,
    // ) -> LLVMModuleFlagBehavior;
    // /// Returns the key for a module flag entry at a specific index.
    // pub fn LLVMModuleFlagEntriesGetKey(
    //     Entries: *mut LLVMModuleFlagEntry,
    //     Index: c_uint,
    //     Len: *mut size_t,
    // ) -> *const c_char;
    // /// Returns the metadata for a module flag entry at a specific index.
    // pub fn LLVMModuleFlagEntriesGetMetadata(
    //     Entries: *mut LLVMModuleFlagEntry,
    //     Index: c_uint,
    // ) -> &'a Metadata;
    // /// Add a module-level flag to the module-level flags metadata if it doesn't already exist.
    // pub fn LLVMGetModuleFlag(module: &Module, Key: *const c_char, KeyLen: size_t) -> &'a Metadata;
    // /// Add a module-level flag to the module-level flags metadata if it doesn't already exist.
    // pub fn LLVMAddModuleFlag(
    //     module: &Module,
    //     Behavior: LLVMModuleFlagBehavior,
    //     Key: *const c_char,
    //     KeyLen: size_t,
    //     Val: &'a Metadata,
    // );

    // pub fn LLVMDumpModule(module: &Module);
    pub fn LLVMPrintModuleToString(module: &Module) -> *mut c_char;

    // pub fn LLVMGetModuleInlineAsm(module: &Module, Len: *mut size_t) -> *const c_char;
    // pub fn LLVMSetModuleInlineAsm2(module: &Module, Asm: *const c_char, Len: size_t);
    // pub fn LLVMAppendModuleInlineAsm(module: &Module, Asm: *const c_char, Len: size_t);
    // pub fn LLVMGetInlineAsm(
    //     Ty: TypeRef,
    //     AsmString: *mut c_char,
    //     AsmStringSize: size_t,
    //     Constraints: *mut c_char,
    //     ConstraintsSize: size_t,
    //     HasSideEffects: LLVMBool,
    //     IsAlignStack: LLVMBool,
    //     Dialect: LLVMInlineAsmDialect,
    //     CanThrow: LLVMBool,
    // ) -> &'a Value;

    //     // pub fn LLVMGetModuleContext(module: &Module) -> &'a Context;
    //     pub fn LLVMGetFirstNamedMetadata(module: &Module) -> NamedMDNodeRef;
    //     pub fn LLVMGetLastNamedMetadata(module: &Module) -> NamedMDNodeRef;
    //     pub fn LLVMGetNextNamedMetadata(NamedMDNode: NamedMDNodeRef) -> NamedMDNodeRef;
    //     pub fn LLVMGetPreviousNamedMetadata(NamedMDNode: NamedMDNodeRef) -> NamedMDNodeRef;
    //     pub fn LLVMGetNamedMetadata(
    //         module: &Module,
    //         name: *const c_char,
    //         NameLen: size_t,
    //     ) -> NamedMDNodeRef;
    //     pub fn LLVMGetOrInsertNamedMetadata(
    //         module: &Module,
    //         name: *const c_char,
    //         NameLen: size_t,
    //     ) -> NamedMDNodeRef;
    //     pub fn LLVMGetNamedMetadataName(
    //         NamedMD: NamedMDNodeRef,
    //         NameLen: *const size_t,
    //     ) -> *const c_char;
    //     pub fn LLVMGetNamedMetadataNumOperands(module: &Module, name: *const c_char) -> c_uint;
    //     pub fn LLVMGetNamedMetadataOperands(module: &Module, name: *const c_char, Dest: *mut &'a Value);
    //     pub fn LLVMAddNamedMetadataOperand(module: &Module, name: *const c_char, Val: &'a Value);
    //     pub fn LLVMGetDebugLocDirectory(Val: &'a Value, Length: *mut c_uint) -> *const c_char;
    //     pub fn LLVMGetDebugLocFilename(Val: &'a Value, Length: *mut c_uint) -> *const c_char;
    //     pub fn LLVMGetDebugLocLine(Val: &'a Value) -> c_uint;
    //     pub fn LLVMGetDebugLocColumn(Val: &'a Value) -> c_uint;
    pub fn LLVMAddFunction<'a>(
        module: &'a Module,
        name: *const c_char,
        FunctionTy: &'a Type,
    ) -> &'a Value;
    // pub fn LLVMGetNamedFunction<'a>(module: &Module, name: *const c_char) -> &'a Value;
    fn LLVMGetFirstFunction(module: &Module) -> Option<&Value>;
    // fn LLVMGetLastFunction<'a>(module: &Module) -> Option<&'a Value>;
    fn LLVMGetNextFunction(fun: &Value) -> Option<&Value>;
    // fn LLVMGetPreviousFunction<'a>(Fn: &'a Value) -> Option<&'a Value>;
}

pub fn function_iter(module: &Module) -> impl Iterator<Item = &Value> + '_ {
    let fun = unsafe { LLVMGetFirstFunction(module) };
    iter::successors(fun, |fun| unsafe { LLVMGetNextFunction(fun) })
}
