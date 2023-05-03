#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]

//! Bindings to LLVM's C API.
//!
//! Refer to the [LLVM documentation](http://llvm.org/docs/) for more
//! information.
//!
//! This is a vendored version of [llvm-sys](https://gitlab.com/taricorp/llvm-sys.rs)
//! adjusted to fit the needs of this project. Furthermore some improvements to llvm made in rustc
//! have been copied here.
//!
//! The buildscript from llvm-sys is replaced with the one from rustc_llvm to allow for faster
//! compile times (no regex/lazy static), cross compilation and dynamic linking
//!
//! Furthermore the types/functions exported here are reduced to only those actually used in OpenVAF to
//! further improve compile times

use std::fmt;

use libc::{c_char, c_uint, c_void};

use crate::util::InvariantOpaque;

mod util;

pub mod attributes;
pub mod basic_block;
pub mod bitcode;
pub mod builder;
pub mod context;
pub mod initialization;
// pub mod lld;
pub mod module;
pub mod pass_manager;
pub mod support;
pub mod targets;
pub mod types;
pub mod values;

pub use attributes::*;
pub use basic_block::*;
pub use bitcode::*;
pub use builder::*;
pub use context::*;
pub use initialization::*;
pub use module::*;
pub use pass_manager::*;
pub use targets::*;
pub use types::*;
pub use values::*;

pub type Bool = c_uint;
pub const True: Bool = 1;
pub const False: Bool = 0;

// Opaque pointer types
// TODO move to opaqute times when stabilized
// BLOCK https://github.com/rust-lang/rust/issues/43467

pub enum MemoryBuffer {}

impl fmt::Debug for MemoryBuffer {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub enum Context {}

impl fmt::Debug for Context {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

#[repr(C)]
pub struct Builder<'a>(InvariantOpaque<'a>);

#[repr(C)]
pub struct PassManager<'a>(InvariantOpaque<'a>);

pub enum Type {}

impl fmt::Debug for Type {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub enum Value {}

impl fmt::Debug for Value {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub enum Attribute {}

impl fmt::Debug for Attribute {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub enum BasicBlock {}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub enum Module {}

impl fmt::Debug for Module {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub enum PassRegistry {}

impl fmt::Debug for PassRegistry {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub enum PassManagerBuilder {}

impl fmt::Debug for PassManagerBuilder {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub enum Target {}

impl fmt::Debug for Target {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}
pub enum DiagnosticInfo {}

impl fmt::Debug for DiagnosticInfo {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

#[derive(Debug)]
pub enum TargetData {}

#[derive(Debug)]
pub enum TargetMachine {}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptLevel {
    None = 0,
    Less = 1,
    Default = 2,
    Aggressive = 3,
}

// Only allow default CodeModel/RelocMode
// If we allow different modes we might need to change
// this for each module as done in rustc

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelocMode {
    Default = 0,
    // Static = 1,
    PIC = 2,
    // DynamicNoPic = 3,
    // ROPI = 4,
    // RWPI = 5,
    // ROPI_RWPI = 6,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CodeModel {
    Default = 0,
    // JITDefault = 1,
    // Tiny = 2,
    // Small = 3,
    // Kernel = 4,
    // Medium = 5,
    // Large = 6,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CodeGenFileType {
    AssemblyFile = 0,
    ObjectFile = 1,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Linkage {
    ExternalLinkage = 0,
    AvailableExternallyLinkage = 1,
    LinkOnceAnyLinkage = 2,
    LinkOnceODRLinkage = 3,
    LinkOnceODRAutoHideLinkage = 4,
    WeakAnyLinkage = 5,
    WeakODRLinkage = 6,
    AppendingLinkage = 7,
    Internal = 8,
    PrivateLinkage = 9,
    DLLImportLinkage = 10,
    DLLExportLinkage = 11,
    ExternalWeakLinkage = 12,
    GhostLinkage = 13,
    CommonLinkage = 14,
    LinkerPrivateLinkage = 15,
    LinkerPrivateWeakLinkage = 16,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Visibility {
    Default = 0,
    Hidden = 1,
    Protected = 2,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnnamedAddr {
    /// Address of the GV is significant.
    No,
    /// Address of the GV is locally insignificant.
    Local,
    /// Address of the GV is globally insignificant.
    Global,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DLLStorageClass {
    Default = 0,
    Import = 1,
    Export = 2,
}

// LLVM CallingConv::ID. Should we wrap this?
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(C)]
pub enum CallConv {
    CCallConv = 0,
    FastCallConv = 8,
    ColdCallConv = 9,
    // X86StdcallCallConv = 64,
    // X86FastcallCallConv = 65,
    // ArmAapcsCallConv = 67,
    // Msp430Intr = 69,
    // X86_ThisCall = 70,
    // PtxKernel = 71,
    // X86_64_SysV = 78,
    // X86_64_Win64 = 79,
    // X86_VectorCall = 80,
    // X86_Intr = 83,
    // AvrNonBlockingInterrupt = 84,
    // AvrInterrupt = 85,
    // AmdGpuKernel = 91,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntPredicate {
    IntEQ = 32,
    IntNE = 33,
    IntUGT = 34,
    IntUGE = 35,
    IntULT = 36,
    IntULE = 37,
    IntSGT = 38,
    IntSGE = 39,
    IntSLT = 40,
    IntSLE = 41,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub enum DiagnosticSeverity {
    Error = 0,
    Warning = 1,
    Remark = 2,
    Note = 3,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RealPredicate {
    RealPredicateFalse = 0,
    RealOEQ = 1,
    RealOGT = 2,
    RealOGE = 3,
    RealOLT = 4,
    RealOLE = 5,
    RealONE = 6,
    RealORD = 7,
    RealUNO = 8,
    RealUEQ = 9,
    RealUGT = 10,
    RealUGE = 11,
    RealULT = 12,
    RealULE = 13,
    RealUNE = 14,
    RealPredicateTrue = 15,
}

pub const LLVMAttributeReturnIndex: ::libc::c_uint = 0;
pub const LLVMAttributeFunctionIndex: ::libc::c_uint = !0; // -1
/// Either LLVMAttributeReturnIndex, LLVMAttributeFunctionIndex, or a parameter
/// number from 1 to N.
pub type LLVMAttributeIndex = ::libc::c_uint;

pub type DiagnosticHandler = Option<extern "C" fn(diag: &DiagnosticInfo, ctx: *mut c_void)>;
pub type LLVMYieldCallback = Option<extern "C" fn(arg1: &Context, ctx: *mut c_void)>;

pub fn get_version() -> (u32, u32, u32) {
    // If RUST_CHECK is set we do not link LLVM and the version is not known, just use dummy values in that case
    (
        option_env!("LLVM_VERSION_MAJOR").map_or(14, |it| it.parse().unwrap()),
        option_env!("LLVM_VERSION_MINOR").map_or(0, |it| it.parse().unwrap()),
        option_env!("LLVM_VERSION_PATCH").map_or(6, |it| it.parse().unwrap()),
    )
}

/// Empty string, to be used where LLVM expects an instruction name, indicating
/// that the instruction is to be left unnamed (i.e. numbered, in textual IR).
// FIXME(eddyb) pass `&CStr` directly to FFI once it's a thin pointer.
pub const UNNAMED: *const c_char = b"\0".as_ptr() as *const c_char;

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeKind {
    Void = 0,
    Half = 1,
    Float = 2,
    Double = 3,
    X86_FP80 = 4,
    FP128 = 5,
    PPC_FP128 = 6,
    Label = 7,
    Integer = 8,
    Function = 9,
    Struct = 10,
    Array = 11,
    Pointer = 12,
    Vector = 13,
    Metadata = 14,
    X86_MMX = 15,
    Token = 16,
    ScalableVector = 17,
    BFloat = 18,
    X86_AMX = 19,
}
