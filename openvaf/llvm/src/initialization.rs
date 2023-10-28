//! Initialization routines which must be called before using library features.

use std::ffi::CString;
use std::sync::Once;

use ahash::AHashSet;
use libc::{c_char, c_int};

use crate::{Bool, PassRegistry};

extern "C" {
    fn LLVMInitializeCore(R: *mut PassRegistry);
    fn LLVMInitializeTransformUtils(R: *mut PassRegistry);
    fn LLVMInitializeScalarOpts(R: *mut PassRegistry);
    fn LLVMInitializeVectorization(R: *mut PassRegistry);
    fn LLVMInitializeInstCombine(R: *mut PassRegistry);
    // fn LLVMInitializeAggressiveInstCombiner(R: *mut PassRegistry);
    fn LLVMInitializeIPO(R: *mut PassRegistry);
    fn LLVMInitializeAnalysis(R: *mut PassRegistry);
    fn LLVMInitializeCodeGen(R: *mut PassRegistry);
    fn LLVMInitializeTarget(R: *mut PassRegistry);

    fn LLVMGetGlobalPassRegistry() -> *mut PassRegistry;
    fn LLVMIsMultithreaded() -> Bool;
    fn LLVMParseCommandLineOptions(
        argc: c_int,
        argv: *const *const c_char,
        overview: *const c_char,
    );
}

static INIT: Once = Once::new();

pub fn init(cg_opts: &[String], tg_opts: &[String]) {
    unsafe {
        // Before we touch LLVM, make sure that multithreading is enabled.
        if LLVMIsMultithreaded() != 1 {
            panic!("LLVM compiled without support for threads");
        }
        INIT.call_once(|| {
            configure_llvm(cg_opts, tg_opts);
        });
    }
}

pub fn require_inited() {
    if !INIT.is_completed() {
        panic!("LLVM is not initialized");
    }
}

unsafe fn configure_llvm(cg_opts: &[String], tg_opts: &[String]) {
    let n_args = cg_opts.len() + tg_opts.len();
    let mut llvm_c_strs = Vec::with_capacity(n_args + 1);
    let mut llvm_args = Vec::with_capacity(n_args + 1);

    fn llvm_arg_to_arg_name(full_arg: &str) -> &str {
        full_arg.trim().split(|c: char| c == '=' || c.is_whitespace()).next().unwrap_or("")
    }

    let args = cg_opts.iter().chain(tg_opts.iter());

    let user_specified_args: AHashSet<_> =
        args.clone().map(|s| llvm_arg_to_arg_name(s)).filter(|s| !s.is_empty()).collect();

    {
        // This adds the given argument to LLVM. Unless `force` is true
        // user specified arguments are *not* overridden.
        let mut add = |arg: &str, force: bool| {
            if force || !user_specified_args.contains(llvm_arg_to_arg_name(arg)) {
                let s = CString::new(arg).unwrap();
                llvm_args.push(s.as_ptr());
                llvm_c_strs.push(s);
            }
        };

        // Set the llvm "program name" to make usage and invalid argument messages more clear.
        add(concat!(env!("COMPILER_NAME"), " -Cllvm-args=\"...\" with"), true);
        add("-time-passes", false);
        for arg in args {
            add(arg, true);
        }
    }

    // if sess.opts.debugging_opts.llvm_time_trace {
    //     llvm::LLVMTimeTraceProfilerInitialize();
    // }

    let registry = LLVMGetGlobalPassRegistry();
    LLVMInitializeCore(registry);
    LLVMInitializeCodeGen(registry);
    LLVMInitializeScalarOpts(registry);
    LLVMInitializeVectorization(registry);
    LLVMInitializeIPO(registry);
    LLVMInitializeAnalysis(registry);
    LLVMInitializeTransformUtils(registry);
    LLVMInitializeInstCombine(registry);
    LLVMInitializeTarget(registry);

    initialize_available_targets();

    LLVMParseCommandLineOptions(
        llvm_args.len() as c_int,
        llvm_args.as_ptr(),
        b"".as_ptr() as *const c_char,
    );
}

/// Initialize targets enabled by the build script via `cfg(llvm_component = "...")`.
/// N.B., this function can't be moved to `rustc_codegen_llvm` because of the `cfg`s.
pub fn initialize_available_targets() {
    macro_rules! init_target(
        ($cfg:meta, $($method:ident),*) => { {
            #[cfg($cfg)]
            fn init() {
                extern "C" {
                    $(fn $method();)*
                }
                unsafe {
                    $($method();)*
                }
            }
            #[cfg(not($cfg))]
            fn init() { }
            init();
        } }
    );

    // Currently the only supported targets are x86, arm, aarch64 and riscv
    init_target!(
        llvm_component = "x86",
        LLVMInitializeX86TargetInfo,
        LLVMInitializeX86Target,
        LLVMInitializeX86TargetMC,
        LLVMInitializeX86AsmPrinter,
        LLVMInitializeX86AsmParser
    );
    init_target!(
        llvm_component = "arm",
        LLVMInitializeARMTargetInfo,
        LLVMInitializeARMTarget,
        LLVMInitializeARMTargetMC,
        LLVMInitializeARMAsmPrinter,
        LLVMInitializeARMAsmParser
    );
    init_target!(
        llvm_component = "aarch64",
        LLVMInitializeAArch64TargetInfo,
        LLVMInitializeAArch64Target,
        LLVMInitializeAArch64TargetMC,
        LLVMInitializeAArch64AsmPrinter,
        LLVMInitializeAArch64AsmParser
    );
    // init_target!(
    //     llvm_component = "amdgpu",
    //     LLVMInitializeAMDGPUTargetInfo,
    //     LLVMInitializeAMDGPUTarget,
    //     LLVMInitializeAMDGPUTargetMC,
    //     LLVMInitializeAMDGPUAsmPrinter,
    //     LLVMInitializeAMDGPUAsmParser
    // );
    // init_target!(
    //     llvm_component = "avr",
    //     LLVMInitializeAVRTargetInfo,
    //     LLVMInitializeAVRTarget,
    //     LLVMInitializeAVRTargetMC,
    //     LLVMInitializeAVRAsmPrinter,
    //     LLVMInitializeAVRAsmParser
    // );
    // init_target!(
    //     llvm_component = "m68k",
    //     LLVMInitializeM68kTargetInfo,
    //     LLVMInitializeM68kTarget,
    //     LLVMInitializeM68kTargetMC,
    //     LLVMInitializeM68kAsmPrinter,
    //     LLVMInitializeM68kAsmParser
    // );
    // init_target!(
    //     llvm_component = "mips",
    //     LLVMInitializeMipsTargetInfo,
    //     LLVMInitializeMipsTarget,
    //     LLVMInitializeMipsTargetMC,
    //     LLVMInitializeMipsAsmPrinter,
    //     LLVMInitializeMipsAsmParser
    // );
    // init_target!(
    //     llvm_component = "powerpc",
    //     LLVMInitializePowerPCTargetInfo,
    //     LLVMInitializePowerPCTarget,
    //     LLVMInitializePowerPCTargetMC,
    //     LLVMInitializePowerPCAsmPrinter,
    //     LLVMInitializePowerPCAsmParser
    // );
    // init_target!(
    //     llvm_component = "systemz",
    //     LLVMInitializeSystemZTargetInfo,
    //     LLVMInitializeSystemZTarget,
    //     LLVMInitializeSystemZTargetMC,
    //     LLVMInitializeSystemZAsmPrinter,
    //     LLVMInitializeSystemZAsmParser
    // );
    // init_target!(
    //     llvm_component = "jsbackend",
    //     LLVMInitializeJSBackendTargetInfo,
    //     LLVMInitializeJSBackendTarget,
    //     LLVMInitializeJSBackendTargetMC
    // );
    // init_target!(
    //     llvm_component = "msp430",
    //     LLVMInitializeMSP430TargetInfo,
    //     LLVMInitializeMSP430Target,
    //     LLVMInitializeMSP430TargetMC,
    //     LLVMInitializeMSP430AsmPrinter,
    //     LLVMInitializeMSP430AsmParser
    // );
    init_target!(
        llvm_component = "riscv",
        LLVMInitializeRISCVTargetInfo,
        LLVMInitializeRISCVTarget,
        LLVMInitializeRISCVTargetMC,
        LLVMInitializeRISCVAsmPrinter,
        LLVMInitializeRISCVAsmParser
    );
    // init_target!(
    //     llvm_component = "sparc",
    //     LLVMInitializeSparcTargetInfo,
    //     LLVMInitializeSparcTarget,
    //     LLVMInitializeSparcTargetMC,
    //     LLVMInitializeSparcAsmPrinter,
    //     LLVMInitializeSparcAsmParser
    // );
    // init_target!(
    //     llvm_component = "nvptx",
    //     LLVMInitializeNVPTXTargetInfo,
    //     LLVMInitializeNVPTXTarget,
    //     LLVMInitializeNVPTXTargetMC,
    //     LLVMInitializeNVPTXAsmPrinter
    // );
    // init_target!(
    //     llvm_component = "hexagon",
    //     LLVMInitializeHexagonTargetInfo,
    //     LLVMInitializeHexagonTarget,
    //     LLVMInitializeHexagonTargetMC,
    //     LLVMInitializeHexagonAsmPrinter,
    //     LLVMInitializeHexagonAsmParser
    // );
    // init_target!(
    //     llvm_component = "webassembly",
    //     LLVMInitializeWebAssemblyTargetInfo,
    //     LLVMInitializeWebAssemblyTarget,
    //     LLVMInitializeWebAssemblyTargetMC,
    //     LLVMInitializeWebAssemblyAsmPrinter,
    //     LLVMInitializeWebAssemblyAsmParser
    // );
    // init_target!(
    //     llvm_component = "bpf",
    //     LLVMInitializeBPFTargetInfo,
    //     LLVMInitializeBPFTarget,
    //     LLVMInitializeBPFTargetMC,
    //     LLVMInitializeBPFAsmPrinter,
    //     LLVMInitializeBPFAsmParser
    // );
}
