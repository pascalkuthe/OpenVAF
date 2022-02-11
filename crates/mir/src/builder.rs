//! MIR instruction builder.
//!
//! A `Builder` provides a convenient interface for inserting instructions into a Cranelift
//! function. Many of its methods are generated from the meta language instruction definitions.

use crate::instructions::{PhiMap, PhiNode, ValueList};
use crate::{Block, DataFlowGraph, FuncRef, Inst, InstructionData, Opcode, Value};

#[cfg(test)]
mod tests;
#[rustfmt::skip]
mod generated;
pub use generated::*;

/// Base trait for instruction builders.
///
/// The `InstBuilderBase` trait provides the basic functionality required by the methods of the
/// generated `InstBuilder` trait. These methods should not normally be used directly. Use the
/// methods in the `InstBuilder` trait instead.
///
/// Any data type that implements `InstBuilderBase` also gets all the methods of the `InstBuilder`
/// trait.
pub trait InstBuilderBase<'f>: Sized {
    /// Get an immutable reference to the data flow graph that will hold the constructed
    /// instructions.
    fn data_flow_graph(&self) -> &DataFlowGraph;
    /// Get a mutable reference to the data flow graph that will hold the constructed
    /// instructions.
    fn data_flow_graph_mut(&mut self) -> &mut DataFlowGraph;

    /// Insert an instruction and return a reference to it, consuming the builder.
    fn build(self, data: InstructionData) -> (Inst, &'f mut DataFlowGraph);
}

/// Any type implementing `InstBuilderBase` gets all the `InstBuilder` methods for free.
impl<'f, T: InstBuilderBase<'f>> InstBuilder<'f> for T {}

/// Base trait for instruction inserters.
///
/// This is an alternative base trait for an instruction builder to implement.
///
/// An instruction inserter can be adapted into an instruction builder by wrapping it in an
/// `InsertBuilder`. This provides some common functionality for instruction builders that insert
/// new instructions, as opposed to the `ReplaceBuilder` which overwrites existing instructions.
pub trait InstInserterBase<'f>: Sized {
    /// Get an immutable reference to the data flow graph.
    fn data_flow_graph(&self) -> &DataFlowGraph;

    /// Get a mutable reference to the data flow graph.
    fn data_flow_graph_mut(&mut self) -> &mut DataFlowGraph;

    /// Insert a new instruction which belongs to the DFG.
    fn insert_built_inst(self, inst: Inst) -> &'f mut DataFlowGraph;
}

use core::marker::PhantomData;

/// Builder that inserts an instruction at the current position.
///
/// An `InsertBuilder` is a wrapper for an `InstInserterBase` that turns it into an instruction
/// builder with some additional facilities for creating instructions that reuse existing values as
/// their results.
pub struct InsertBuilder<'f, IIB: InstInserterBase<'f>> {
    inserter: IIB,
    unused: PhantomData<&'f u32>,
}

impl<'f, IIB: InstInserterBase<'f>> InsertBuilder<'f, IIB> {
    /// Create a new builder which inserts instructions at `pos`.
    /// The `dfg` and `pos.layout` references should be from the same `Function`.
    pub fn new(inserter: IIB) -> Self {
        Self { inserter, unused: PhantomData }
    }

    /// Reuse result values in `reuse`.
    ///
    /// Convert this builder into one that will reuse the provided result values instead of
    /// allocating new ones. The provided values for reuse must not be attached to anything. Any
    /// missing result values will be allocated as normal.
    ///
    /// The `reuse` argument is expected to be an array of `Option<Value>`.
    pub fn with_results<Array>(self, reuse: Array) -> InsertReuseBuilder<'f, IIB, Array>
    where
        Array: AsRef<[Option<Value>]>,
    {
        InsertReuseBuilder { inserter: self.inserter, reuse, unused: PhantomData }
    }

    /// Reuse a single result value.
    ///
    /// Convert this into a builder that will reuse `v` as the single result value. The reused
    /// result value `v` must not be attached to anything.
    ///
    /// This method should only be used when building an instruction with exactly one result. Use
    /// `with_results()` for the more general case.
    pub fn with_result(self, v: Value) -> InsertReuseBuilder<'f, IIB, [Option<Value>; 1]> {
        // TODO: Specialize this to return a different builder that just attaches `v` instead of
        // calling `make_inst_results_reusing()`.
        self.with_results([Some(v)])
    }
}

impl<'f, IIB: InstInserterBase<'f>> InstBuilderBase<'f> for InsertBuilder<'f, IIB> {
    fn data_flow_graph(&self) -> &DataFlowGraph {
        self.inserter.data_flow_graph()
    }

    fn data_flow_graph_mut(&mut self) -> &mut DataFlowGraph {
        self.inserter.data_flow_graph_mut()
    }

    fn build(mut self, data: InstructionData) -> (Inst, &'f mut DataFlowGraph) {
        let inst;
        {
            let dfg = self.inserter.data_flow_graph_mut();
            inst = dfg.make_inst(data);
            dfg.make_inst_results(inst);
        }
        (inst, self.inserter.insert_built_inst(inst))
    }
}

/// Builder that inserts a new instruction like `InsertBuilder`, but reusing result values.
pub struct InsertReuseBuilder<'f, IIB, Array>
where
    IIB: InstInserterBase<'f>,
    Array: AsRef<[Option<Value>]>,
{
    inserter: IIB,
    reuse: Array,
    unused: PhantomData<&'f u32>,
}

impl<'f, IIB, Array> InstBuilderBase<'f> for InsertReuseBuilder<'f, IIB, Array>
where
    IIB: InstInserterBase<'f>,
    Array: AsRef<[Option<Value>]>,
{
    fn data_flow_graph(&self) -> &DataFlowGraph {
        self.inserter.data_flow_graph()
    }

    fn data_flow_graph_mut(&mut self) -> &mut DataFlowGraph {
        self.inserter.data_flow_graph_mut()
    }

    fn build(mut self, data: InstructionData) -> (Inst, &'f mut DataFlowGraph) {
        let inst;
        {
            let dfg = self.inserter.data_flow_graph_mut();
            inst = dfg.make_inst(data);
            // Make an `Iterator<Item = Option<Value>>`.
            let ru = self.reuse.as_ref().iter().cloned();
            dfg.make_inst_results_reusing(inst, ru);
        }
        (inst, self.inserter.insert_built_inst(inst))
    }
}

/// Instruction builder that replaces an existing instruction.
///
/// The inserted instruction will have the same `Inst` number as the old one.
///
/// If the old instruction still has result values attached, it is assumed that the new instruction
/// produces the same number and types of results. The old result values are preserved. If the
/// replacement instruction format does not support multiple results, the builder panics. It is a
/// bug to leave result values dangling.
pub struct ReplaceBuilder<'f> {
    dfg: &'f mut DataFlowGraph,
    inst: Inst,
}

impl<'f> ReplaceBuilder<'f> {
    /// Create a `ReplaceBuilder` that will overwrite `inst`.
    pub fn new(dfg: &'f mut DataFlowGraph, inst: Inst) -> Self {
        Self { dfg, inst }
    }
}

impl<'f> InstBuilderBase<'f> for ReplaceBuilder<'f> {
    fn data_flow_graph(&self) -> &DataFlowGraph {
        self.dfg
    }

    fn data_flow_graph_mut(&mut self) -> &mut DataFlowGraph {
        self.dfg
    }

    fn build(self, data: InstructionData) -> (Inst, &'f mut DataFlowGraph) {
        // Splat the new instruction on top of the old one.
        self.dfg.update_inst(self.inst, data);

        if !self.dfg.has_results(self.inst) {
            // The old result values were either detached or non-existent.
            // Construct new ones.
            self.dfg.make_inst_results(self.inst);
        }

        (self.inst, self.dfg)
    }
}
