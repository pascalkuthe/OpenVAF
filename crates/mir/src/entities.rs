//! OpenVAF IR entity references.
//!
//! Instructions in Cranelift IR need to reference other entities in the function. This can be other
//! parts of the function like basic blocks or stack slots, or it can be external entities
//! that are declared in the function preamble in the text format.
//!
//! These entity references in instruction operands are not implemented as Rust references both
//! because Rust's ownership and mutability rules make it difficult, and because 64-bit pointers
//! take up a lot of space, and we want a compact in-memory representation. Instead, entity
//! references are structs wrapping a `u32` index into a table in the `Function` main data
//! structure. There is a separate index type for each entity type, so we don't lose type safety.
//!
//! The `entities` module defines public types for the entity references along with constants
//! representing an invalid reference. We prefer to use `Option<EntityRef>` whenever possible, but
//! unfortunately that type is twice as large as the 32-bit index type on its own. Thus, compact
//! data structures use the `PackedOption<EntityRef>` representation, while function arguments and
//! return values prefer the more Rust-like `Option<EntityRef>` variant.
//!
//! The entity references all implement the `Display` trait in a way that matches the textual IR
//! format.

use core::u32;
use cranelift_entity::entity_impl;
use std::fmt;

/// An opaque reference to a [basic block](https://en.wikipedia.org/wiki/Basic_block) in a
/// [`Function`](super::function::Function).
///
/// You can get a `Block` using
/// [`FunctionBuilder::create_block`](https://docs.rs/cranelift-frontend/*/cranelift_frontend/struct.FunctionBuilder.html#method.create_block)
///
/// While the order is stable, it is arbitrary and does not necessarily resemble the layout order.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Block(u32);
entity_impl!(Block, "block");

impl Block {
    /// Create a new block reference from its number. This corresponds to the `blockNN` representation.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(Self(n))
        } else {
            None
        }
    }
}

/// An opaque reference to an SSA value.
///
/// You can get a constant `Value` from the following
/// [`InstBuilder`](super::InstBuilder) instructions:
///
/// - [`iconst`](super::InstBuilder::iconst) for integer constants
/// - [`f32const`](super::InstBuilder::f32const) for 32-bit float constants
/// - [`f64const`](super::InstBuilder::f64const) for 64-bit float constants
/// - [`bconst`](super::InstBuilder::bconst) for boolean constants
/// - [`vconst`](super::InstBuilder::vconst) for vector constants
/// - [`null`](super::InstBuilder::null) for null reference constants
///
/// Any `InstBuilder` instruction that has an output will also return a `Value`.
///
/// While the order is stable, it is arbitrary.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Value(u32);
entity_impl!(Value, "v");

impl Value {
    /// Create a value from its number representation.
    /// This is the number in the `vNN` notation.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX / 2 {
            Some(Self(n))
        } else {
            None
        }
    }
}

/// An opaque reference to an instruction in a [`Function`](super::Function).
///
/// Most usage of `Inst` is internal. `Inst`ructions are returned by
/// [`InstBuilder`](super::InstBuilder) instructions that do not return a
/// [`Value`], such as control flow and trap instructions.
///
/// If you look around the API, you can find many inventive uses for `Inst`,
/// such as [annotating specific instructions with a comment][inst_comment]
/// or [performing reflection at compile time](super::DataFlowGraph::analyze_branch)
/// on the type of instruction.
///
/// [inst_comment]: https://github.com/bjorn3/rustc_codegen_cranelift/blob/0f8814fd6da3d436a90549d4bb19b94034f2b19c/src/pretty_clif.rs
///
/// While the order is stable, it is arbitrary and does not necessarily resemble the layout order.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Inst(u32);
entity_impl!(Inst, "inst");

/// An opaque reference to another [`Function`](super::Function).
///
/// `FuncRef`s are used for [direct](super::InstBuilder::call) function calls
///
/// `FuncRef`s can be created with
///
/// While the order is stable, it is arbitrary.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FuncRef(u32);
entity_impl!(FuncRef, "fn");

impl FuncRef {
    /// Create a new external function reference from its number.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(Self(n))
        } else {
            None
        }
    }
}

/// An opaque reference to any of the entities defined in this module that can appear in CLIF IR.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AnyEntity {
    /// The whole function.
    Function,
    /// a basic block.
    Block(Block),
    /// An instruction.
    Inst(Inst),
    /// An SSA value.
    Value(Value),
    /// An external function.
    FuncRef(FuncRef),
}

impl fmt::Display for AnyEntity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Function => write!(f, "function"),
            Self::Block(r) => r.fmt(f),
            Self::Inst(r) => r.fmt(f),
            Self::Value(r) => r.fmt(f),
            Self::FuncRef(r) => r.fmt(f),
        }
    }
}

impl fmt::Debug for AnyEntity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self as &dyn fmt::Display).fmt(f)
    }
}

impl From<Block> for AnyEntity {
    fn from(r: Block) -> Self {
        Self::Block(r)
    }
}

impl From<Inst> for AnyEntity {
    fn from(r: Inst) -> Self {
        Self::Inst(r)
    }
}

impl From<Value> for AnyEntity {
    fn from(r: Value) -> Self {
        Self::Value(r)
    }
}

impl From<FuncRef> for AnyEntity {
    fn from(r: FuncRef) -> Self {
        Self::FuncRef(r)
    }
}
