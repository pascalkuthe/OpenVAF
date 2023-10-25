//! OpenVAF MIR
//!
//! The OpenVAF MIR represents the bodys of Verilog-A items as [SSA].
//! This allows very efficient implementations of the various algorithms in the backend.
//! The implementation in this crate is heavly inspired by the IR in [`cranelift`] and [`llvm`].
//! However the focus of the MIR is on traditional algorithms pefromed at middle instead of the
//! codegeneration. As a result the implementation is simplified:
//!
//! * The MIR only represent Verilog-A allowing dropping support for atomics etc.
//! * The MIR does not map to actual hardware opcodes as dirct codegeneration is not a goal
//! * The MIR is untyped. All opcodes have fixed argument/return types. Instructions must be
//! constructed with correct types.
//!
//! Compared to the HIR the MIR is completely decoupled from the AST (and HIR) which allows for much
//! faster compile times. This break comes quite naturally as the various algorithms that operate on the
//! MIR are not concerned with language-level concepts.
//! Translation from HIR to MIR and mappings from MIR objects to the language-level HIR objects are
//! found in the `hir_lower` crate which is the only bridge between the various MIR crates and the HIR.
//!
//! [`cranelift`]: https://github.com/bytecodealliance/wasmtime/tree/main/cranelift
//! [`llvm`]: https://github.com/llvm/llvm-project
//! [SSA]: https://en.wikipedia.org/wiki/Static_single_assignment_form

mod dfg;
mod dominators;
mod entities;
mod instructions;
mod layout;
mod serialize;

pub mod builder;
pub mod cursor;
pub mod flowgraph;
pub mod write;

use ahash::AHashMap;
use bitset::HybridBitSet;
use core::fmt;
pub use lasso::{Interner, Spur};
use stdx::{impl_debug, impl_display, impl_idx_from};
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

pub use crate::dfg::consts::*;
pub use crate::dfg::{Const, DataFlowGraph, DfgValues, InstUseIter, UseCursor, UseIter, ValueDef};
pub use crate::dominators::DominatorTree;
pub use crate::entities::{AnyEntity, Block, FuncRef, Inst, Param, Use, Value};
pub use crate::flowgraph::ControlFlowGraph;
pub use crate::instructions::{
    InstructionData, InstructionFormat, Opcode, PhiMap, PhiNode, ValueList, ValueListPool,
};
pub use crate::layout::{InstCursor, InstIter, Layout};
use crate::write::DummyResolver;
pub use stdx::Ieee64;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FunctionSignature {
    pub name: String,
    pub params: u16,
    pub returns: u16,
    pub has_sideeffects: bool,
}

impl_display! {
    match FunctionSignature{
        FunctionSignature{name, params, returns, has_sideeffects} => "{}fn %{}({}) -> {}", if *has_sideeffects{""}else{"const "}, name, params, returns;
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
            srclocs: TiVec::new(),
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

    /// Adds a signature which can later be used to declare an external function import.
    pub fn import_function(&mut self, signature: FunctionSignature) -> FuncRef {
        self.dfg.signatures.push_and_get_key(signature)
    }

    pub fn update_phi_edges(&mut self, bb: Block, old_pred: Block, new_pred: Block) {
        for inst in self.layout.block_insts(bb) {
            if let InstructionData::PhiNode(PhiNode { ref mut blocks, .. }) = self.dfg.insts[inst] {
                let pos = blocks.remove(old_pred, &mut self.dfg.phi_forest, &()).unwrap();
                blocks.insert(new_pred, pos, &mut self.dfg.phi_forest, &());
            } else {
                break;
            }
        }
    }

    /// Split the block containing `before` in two.
    ///
    /// Insert `new_block` after the old block and move `before` and the following instructions to
    /// `new_block`:
    ///
    /// ```text
    /// old_block:
    ///     i1
    ///     i2
    ///     i3 << before
    ///     i4
    /// ```
    /// becomes:
    ///
    /// ```text
    /// old_block:
    ///     i1
    ///     i2
    /// new_block:
    ///     i3 << before
    ///     i4
    /// ```
    pub fn split_block(&mut self, new_block: Block, before: Inst) {
        let old_block = self.layout.inst_block(before).unwrap();
        self.layout.split_block(new_block, before);
        if let Some(term) = self.layout.block_terminator(new_block) {
            match self.dfg.insts[term] {
                InstructionData::Jump { destination } => {
                    self.update_phi_edges(destination, old_block, new_block)
                }
                InstructionData::Branch { then_dst, else_dst, .. } => {
                    self.update_phi_edges(then_dst, old_block, new_block);
                    self.update_phi_edges(else_dst, old_block, new_block);
                }
                _ => (),
            }
        }
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
pub type SourceLocs = TiVec<Inst, SourceLoc>;

/// A source location.
///
/// This is an opaque 32-bit number attached to each IR instruction.
///
/// The default source location uses the all-ones bit pattern `!0`. It is used for instructions
/// that can't be given a real source location.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct SourceLoc(pub i32);

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

impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_default() {
            write!(f, "@-")
        } else {
            write!(f, "@{:04x}", self.0)
        }
    }
}

impl Function {
    pub fn remove_opt_barriers(&mut self) {
        for inst in self.dfg.insts.iter() {
            if let InstructionData::Unary { opcode: Opcode::OptBarrier, arg } = self.dfg.insts[inst]
            {
                if self.layout.inst_block(inst).is_some() {
                    let res = self.dfg.first_result(inst);
                    self.dfg.replace_uses(res, arg);
                    self.layout.remove_inst(inst)
                }
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct KnownDerivatives {
    pub unknowns: TiSet<Unknown, Value>,
    pub ddx_calls: AHashMap<FuncRef, (HybridBitSet<Unknown>, HybridBitSet<Unknown>)>,
    // pub standin_calls: AHashMap<FuncRef, u32>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Unknown(pub u32);
impl_idx_from!(Unknown(u32));

impl_debug!(match Unknown{Unknown(raw) => "unknown{}",raw;});
