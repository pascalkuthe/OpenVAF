//! OpenVAF MIR
//!
//! The OpenVAF MIR represents the bodys of Verilog-A items as [SSA].
//! This allows very efficent implementations of the various algorithms in the backend.
//! The implementation in this crate is heavly inspired by the IR in [`cranelift`].
//! However the focus of the MIR is on traditional algorithms pefromed at middle instead of the
//! codegeneration. As a result the implementation is simplified:
//!
//! * The MIR only represent Verilog-A allowing dropping support for atomics etc.
//! * The MIR does not map to actual hardware opcodes as dirct codegeneration is not a goal
//! * The MIR is untyped. All opcodes have fixed argument/return types. Instructions must be
//! constructed with correct types.
//!
//! Compared to the HIR the MIR is completly decoupled from the AST (and HIR) which allows for much
//! faster compile times. This break comes quite naturally as the various algorithms that operate on the
//! MIR are not concerned with language-level concepts.
//! Translation from HIR to MIR and mappings from MIR objects to the language-level HIR objects are
//! found in the `hir_lower` crate which is the only bridge between the various MIR crates and the HIR.
//!
//! [`cranelift`]: https://github.com/bytecodealliance/wasmtime/tree/main/cranelift
//! [SSA]: https://en.wikipedia.org/wiki/Static_single_assignment_form

mod dfg;
mod entities;
mod immediates;
mod instructions;
mod layout;

pub mod builder;
pub mod cursor;

pub mod write;

use core::fmt;

use cranelift_entity::SecondaryMap;
pub use entities::{Block, FuncRef, Inst, Value};
pub use immediates::Ieee64;
pub use instructions::{InstructionData, InstructionFormat, Opcode, ValueList, ValueListPool};
pub use lasso::{Interner, Spur};
use stdx::impl_display;

pub use crate::dfg::{DataFlowGraph, ValueDef, Values};
pub use crate::layout::Layout;
use crate::write::DummyResolver;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    name: String,
    params: u16,
    returns: u16,
}

impl_display! {
    match FunctionSignature{
        FunctionSignature{name, params, returns} => "%{}({}) -> {}", name, params, returns;
    }
}

///
/// Functions can be cloned, but it is not a very fast operation.
/// The clone will have all the same entity numbers as the original.
#[derive(Clone, Default)]
pub struct Function {
    pub name: String,

    /// Data flow graph containing the primary definition of all instructions, blocks and values.
    pub dfg: DataFlowGraph,

    /// Layout of blocks and instructions in the function body.
    pub layout: Layout,

    /// Source locations.
    ///
    /// Track the original source location for each instruction. The source locations are not
    /// interpreted, only preserved.
    pub srclocs: SourceLocs,
}

impl Function {
    /// Clear all data structures in this function.
    pub fn clear(&mut self) {
        self.dfg.clear();
        self.layout.clear();
        self.srclocs.clear();
    }

    pub fn new() -> Function {
        Self {
            name: String::new(),
            dfg: DataFlowGraph::new(),
            layout: Layout::new(),
            srclocs: SecondaryMap::new(),
        }
    }

    pub fn with_name(name: String) -> Function {
        let mut res = Function::new();
        res.name = name;
        res
    }

    pub fn print<'a>(&'a self, resolver: &'a dyn lasso::Resolver) -> PrintableFunction {
        PrintableFunction { fun: self, resolver }
    }

    pub fn to_debug_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[doc(hidden)]
pub struct PrintableFunction<'a> {
    fun: &'a Function,
    resolver: &'a dyn lasso::Resolver,
}

impl fmt::Display for PrintableFunction<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write::write_function(fmt, self.fun, self.resolver)
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write::write_function(fmt, self, &DummyResolver)
    }
}

/// Source locations for instructions.
pub type SourceLocs = SecondaryMap<Inst, SourceLoc>;

/// A source location.
///
/// This is an opaque 32-bit number attached to each IR instruction.
///
/// The default source location uses the all-ones bit pattern `!0`. It is used for instructions
/// that can't be given a real source location.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourceLoc(i32);

impl SourceLoc {
    /// Create a new source location with the given bits.
    pub fn new(bits: i32) -> Self {
        Self(bits)
    }

    /// Is this the default source location?
    pub fn is_default(self) -> bool {
        self == Default::default()
    }

    /// Read the bits of this source location.
    pub fn bits(self) -> i32 {
        self.0
    }
}

impl Default for SourceLoc {
    fn default() -> Self {
        Self(!0)
    }
}

impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_default() {
            write!(f, "@-")
        } else {
            write!(f, "@{:04x}", self.0)
        }
    }
}
