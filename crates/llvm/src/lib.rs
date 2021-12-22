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
//! Furthermore the types exported here are reduced to only those actually used in OpenVAF to
//! further imporve compile times

use libc::{c_uint, c_void};

use crate::util::InvariantOpaque;

mod util;

pub mod basic_block;
pub mod builder;
pub mod context;
pub mod initialization;
pub mod module;
pub mod pass_manager;
pub mod types;
pub mod values;

pub type Bool = c_uint;
pub const True: Bool = 1;
pub const False: Bool = 0;

// Opaque pointer types
// TODO move to opaqute times when stabilized
// BLOCK https://github.com/rust-lang/rust/issues/43467

#[derive(Debug)]
pub enum LLVMMemoryBuffer {}

#[derive(Debug)]
pub enum Context {}

#[repr(C)]
pub struct Builder<'a>(InvariantOpaque<'a>);

#[repr(C)]
pub struct PassManager<'a>(InvariantOpaque<'a>);

#[derive(Debug)]
pub enum Type {}

#[derive(Debug)]
pub enum Value {}

#[derive(Debug)]
pub enum BasicBlock {}

#[derive(Debug)]
pub enum Module {}

#[derive(Debug)]
pub enum PassRegistry {}

#[derive(Debug)]
pub enum PassManagerBuilder {}

#[derive(Debug)]
pub enum OpaqueAttributeRef {}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Linkage {
    ExternalLinkage = 0,
    AvailableExternallyLinkage = 1,
    LinkOnceAnyLinkage = 2,
    LinkOnceODRLinkage = 3,
    WeakAnyLinkage = 4,
    WeakODRLinkage = 5,
    AppendingLinkage = 6,
    InternalLinkage = 7,
    PrivateLinkage = 8,
    ExternalWeakLinkage = 9,
    CommonLinkage = 10,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Visibility {
    Default = 0,
    Hidden = 1,
    Protected = 2,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LLVMUnnamedAddr {
    /// Address of the GV is significant.
    LLVMNoUnnamedAddr,
    /// Address of the GV is locally insignificant.
    LLVMLocalUnnamedAddr,
    /// Address of the GV is globally insignificant.
    LLVMGlobalUnnamedAddr,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LLVMDLLStorageClass {
    LLVMDefaultStorageClass = 0,
    LLVMDLLImportStorageClass = 1,
    LLVMDLLExportStorageClass = 2,
}

// LLVM CallingConv::ID. Should we wrap this?
#[derive(Copy, Clone, PartialEq, Debug)]
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
#[derive(Clone, Copy, Debug, PartialEq)]
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
#[derive(Clone, Copy, Debug, PartialEq)]
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

// pub type LLVMDiagnosticHandler =
//     Option<extern "C" fn(arg1: &LLVMDiagnosticInfo, arg2: *mut c_void)>;
pub type LLVMYieldCallback = Option<extern "C" fn(arg1: &Context, arg2: *mut c_void)>;

pub fn get_version() -> (u32, u32, u32) {
    // Can be called without initializing LLVM
    (
        env!("LLVM_VERSION_MAJOR").parse().unwrap(),
        env!("LLVM_VERSION_MINOR").parse().unwrap(),
        env!("LLVM_VERSION_PATCH").parse().unwrap(),
    )
}
