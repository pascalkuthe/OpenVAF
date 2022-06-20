mod ssa;

use bitset::BitSet;
use lasso::Rodeo;
use mir::builder::{InsertBuilder, InstBuilder, InstInserterBase};
use mir::cursor::{Cursor, FuncCursor};
use mir::{
    Block, DataFlowGraph, FuncRef, Function, FunctionSignature, Inst, InstructionData, Param, Value,
};
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiVec;

use crate::ssa::SSABuilder;

///! An opaque reference to a variable.
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Place(u32);

impl_idx_from!(Place(u32));
impl_debug_display!(match Place{
    Place(val) => "place{}", val;
});

/// Structure used for translating a series of functions into Cranelift IR.
///
/// In order to reduce memory reallocations when compiling multiple functions,
/// `FunctionBuilderContext` holds various data structures which are cleared between
/// functions, rather than dropped, preserving the underlying allocations.
pub struct FunctionBuilderContext {
    ssa: SSABuilder,
    blocks: TiVec<Block, BlockData>,
}

/// Temporary object used to build a single Cranelift IR `Function`.
pub struct FunctionBuilder<'a> {
    /// The function currently being built.
    /// This field is public so the function can be re-borrowed.
    pub func: &'a mut Function,
    pub interner: &'a mut Rodeo,

    /// Source location to assign to all new instructions.
    srcloc: mir::SourceLoc,

    tag_writes: bool,

    func_ctx: &'a mut FunctionBuilderContext,
    position: Block,
    end: Block,

    pub op_dependent_vals: BitSet<Value>,
}

#[derive(Clone, Default)]
struct BlockData {
    /// A Block is "pristine" iff no instructions have been added since the last
    /// call to `switch_to_block()`.
    pristine: bool,

    /// A Block is "filled" iff a terminator instruction has been inserted since
    /// the last call to `switch_to_block()`.
    ///
    /// A filled block cannot be pristine.
    filled: bool,
}

impl FunctionBuilderContext {
    /// Creates a FunctionBuilderContext structure. The structure is automatically cleared after
    /// each [`FunctionBuilder`](struct.FunctionBuilder.html) completes translating a function.
    pub fn new() -> Self {
        Self { ssa: SSABuilder::new(), blocks: TiVec::new() }
    }

    fn clear(&mut self) {
        self.ssa.clear();
        self.blocks.clear();
    }

    fn is_empty(&self) -> bool {
        self.ssa.is_empty() && self.blocks.is_empty()
    }
}

impl Default for FunctionBuilderContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Implementation of the [`InstBuilder`](cranelift_codegen::ir::InstBuilder) that has
/// one convenience method per Cranelift IR instruction.
pub struct FuncInstBuilder<'short, 'long: 'short> {
    builder: &'short mut FunctionBuilder<'long>,
    block: Block,
}

impl<'short, 'long> FuncInstBuilder<'short, 'long> {
    fn new(builder: &'short mut FunctionBuilder<'long>, block: Block) -> Self {
        Self { builder, block }
    }
}

pub trait RetBuilder {
    fn ret(self) -> Inst;
}

impl<'short, 'long> RetBuilder for InsertBuilder<'short, FuncInstBuilder<'short, 'long>> {
    fn ret(self) -> Inst {
        let exit = self.inserter.builder.func.layout.last_block().unwrap();
        self.jump(exit)
    }
}

impl<'short, 'long> InstInserterBase<'short> for FuncInstBuilder<'short, 'long> {
    fn data_flow_graph(&self) -> &DataFlowGraph {
        &self.builder.func.dfg
    }

    fn data_flow_graph_mut(&mut self) -> &mut DataFlowGraph {
        &mut self.builder.func.dfg
    }

    // This implementation is richer than `InsertBuilder` because we use the data of the
    // instruction being inserted to add related info to the DFG and the SSA building system,
    // and perform debug sanity checks.
    fn insert_built_inst(self, inst: Inst) -> &'short mut DataFlowGraph {
        // We only insert the Block in the layout when an instruction is added to it
        self.builder.ensure_inserted_block();

        self.builder.func.layout.append_inst_to_bb(inst, self.block);
        self.builder.func.srclocs.push(self.builder.srcloc);

        let op_dependent = &mut self.builder.op_dependent_vals;
        op_dependent.ensure(self.builder.func.dfg.num_values() + 1);
        match self.builder.func.dfg.insts[inst] {
            InstructionData::Branch { then_dst, else_dst, .. } => {
                self.builder.declare_successor(then_dst);
                self.builder.declare_successor(else_dst);
                self.builder.fill_current_block()
            }
            InstructionData::Jump { destination } => {
                self.builder.declare_successor(destination);
                self.builder.fill_current_block()
            }

            InstructionData::Binary { args: [arg0, arg1], .. } => {
                let op_dependent = &mut self.builder.op_dependent_vals;
                if op_dependent.contains(arg0) || op_dependent.contains(arg1) {
                    let res = self.builder.func.dfg.first_result(inst);
                    op_dependent.insert(res);
                }
            }

            InstructionData::Unary { arg, .. } => {
                let op_dependent = &mut self.builder.op_dependent_vals;
                if op_dependent.contains(arg) {
                    let res = self.builder.func.dfg.first_result(inst);
                    // if res == 171549u32.into() {
                    //     let inst = self.builder.func.dfg.value_def(arg).inst().unwrap();
                    //     println!(
                    //         "hmm {arg} = {:?} = ({})",
                    //         self.builder.func.dfg.value_def(res),
                    //         self.builder.func.dfg.display_inst(inst)
                    //     );

                    //     println!("{}", self.builder.func.to_debug_string());
                    //     panic!()
                    // }
                    op_dependent.insert(res);
                }
            }
            InstructionData::PhiNode(ref phi) => {
                if self.builder.func.dfg.phi_edges(phi).any(|(_, val)| op_dependent.contains(val)) {
                    let res = self.builder.func.dfg.first_result(inst);
                    op_dependent.insert(res);
                }
            }
            _ => (),
        }
        // let is_op_dependent = self.builder.func.dfg.instr_args(inst).iter().any(|arg| self.builder.op_dependent_vals.contains(elem));
        // if

        &mut self.builder.func.dfg
    }
}

/// This module allows you to create a function in Cranelift IR in a straightforward way, hiding
/// all the complexity of its internal representation.
///
/// The module is parametrized by one type which is the representation of variables in your
/// origin language. It offers a way to conveniently append instruction to your program flow.
/// You are responsible to split your instruction flow into extended blocks (declared with
/// `create_block`) whose properties are:
///
/// - branch and jump instructions can only point at the top of extended blocks;
/// - the last instruction of each block is a terminator instruction which has no natural successor,
///   and those instructions can only appear at the end of extended blocks.
///
/// The parameters of Cranelift IR instructions are Cranelift IR values, which can only be created
/// as results of other Cranelift IR instructions. To be able to create variables redefined multiple
/// times in your program, use the `def_var` and `use_var` command, that will maintain the
/// correspondence between your variables and Cranelift IR SSA values.
///
/// The first block for which you call `switch_to_block` will be assumed to be the beginning of
/// the function.
///
/// At creation, a `FunctionBuilder` instance borrows an already allocated `Function` which it
/// modifies with the information stored in the mutable borrowed
/// [`FunctionBuilderContext`](struct.FunctionBuilderContext.html). The function passed in
/// argument should be newly created with
/// [`Function::with_name_signature()`](Function::with_name_signature), whereas the
/// `FunctionBuilderContext` can be kept as is between two function translations.
///
/// # Errors
///
/// The functions below will panic in debug mode whenever you try to modify the Cranelift IR
/// function in a way that violate the coherence of the code. For instance: switching to a new
/// `Block` when you haven't filled the current one with a terminator instruction, inserting a
/// return instruction with arguments that don't match the function's signature.
impl<'a> FunctionBuilder<'a> {
    /// Creates a new FunctionBuilder structure that will operate on a `Function` using a
    /// `FunctionBuilderContext`.
    pub fn new(
        func: &'a mut Function,
        interner: &'a mut Rodeo,
        func_ctx: &'a mut FunctionBuilderContext,
        tag_writes: bool,
    ) -> Self {
        debug_assert!(func_ctx.is_empty());

        // entry and exit are always empty to allow for easy prepending/appending
        let entry = func.layout.append_new_block();
        func_ctx.blocks.push(BlockData { filled: false, pristine: true });
        func_ctx.ssa.declare_block();

        let exit = func.layout.append_new_block();
        func_ctx.blocks.push(BlockData { filled: false, pristine: true });
        func_ctx.ssa.declare_block();

        let mut res = Self {
            op_dependent_vals: BitSet::new_empty(func.dfg.num_values()),
            func,
            srcloc: Default::default(),
            interner,
            func_ctx,
            position: entry,
            end: exit,
            tag_writes,
        };
        res.seal_block(entry);
        res
    }

    pub fn is_op_dependent(&self, val: Value) -> bool {
        // the bitset is not resised when constants are inserted.
        // Because constants are never op dependent we just short circuit here to avoid panics in
        // that case
        self.func.dfg.value_def(val).as_const().is_none() && self.op_dependent_vals.contains(val)
    }

    /// Creates a new FunctionBuilder structure that will operate on a `Function` using a
    /// `FunctionBuilderContext`.
    pub fn edit(
        func: &'a mut Function,
        interner: &'a mut Rodeo,
        func_ctx: &'a mut FunctionBuilderContext,
        tag_writes: bool,
    ) -> (Self, Inst) {
        debug_assert!(func_ctx.is_empty());
        let mut entry = if let Some(entry) = func.layout.entry_block() {
            entry
        } else {
            // edit() with an empty function is the same as creating a new function
            let builder = Self::new(func, interner, func_ctx, tag_writes);
            let term =
                builder.func.dfg.make_inst(InstructionData::Jump { destination: builder.end });
            return (builder, term);
        };

        let mut exit = func.layout.last_block().unwrap();

        if exit == entry {
            exit = func.layout.append_new_block();
            FuncCursor::new(func).at_bottom(entry).ins().jump(exit);
        }

        let term = match func.layout.first_inst(entry) {
            Some(term) if Some(term) == func.layout.last_inst(entry) => {
                func.layout.remove_inst(term);
                term
            }

            Some(_) => {
                let old_entry = entry;
                entry = func.layout.make_block();
                func.layout.insert_block(entry, old_entry);
                func.dfg.make_inst(InstructionData::Jump { destination: old_entry })
            }
            None => func.dfg.make_inst(InstructionData::Jump { destination: exit }),
        };

        for _bb in 0..func.layout.num_blocks() {
            func_ctx.blocks.push(BlockData { filled: false, pristine: true });
            func_ctx.ssa.declare_block();
        }

        let mut res = Self {
            op_dependent_vals: BitSet::new_empty(func.dfg.num_values()),
            func,
            srcloc: Default::default(),
            interner,
            func_ctx,
            position: entry,
            end: exit,
            tag_writes,
        };
        res.seal_block(entry);
        (res, term)
    }

    pub fn set_end(&mut self, end: Block) {
        self.end = end
    }

    /// Get the block that this builder is currently at.
    pub fn current_block(&self) -> Block {
        self.position
    }

    /// Set the source location that should be assigned to all new instructions.
    pub fn set_srcloc(&mut self, srcloc: mir::SourceLoc) {
        self.srcloc = srcloc;
    }

    /// Set the source location that should be assigned to all new instructions.
    pub fn get_srcloc(&self) -> mir::SourceLoc {
        self.srcloc
    }

    /// Creates a new `Block` and returns its reference.
    pub fn create_block(&mut self) -> Block {
        let block = self.func.layout.make_block();
        self.func_ctx.blocks.push(BlockData { filled: false, pristine: true });
        self.func_ctx.ssa.declare_block();
        block
    }

    pub fn make_param(&mut self, param: Param) -> Value {
        self.func.dfg.make_param(param)
    }

    pub fn fconst(&mut self, val: f64) -> Value {
        self.func.dfg.fconst(val.into())
    }

    pub fn iconst(&mut self, val: i32) -> Value {
        self.func.dfg.iconst(val)
    }

    pub fn sconst(&mut self, val: &str) -> Value {
        let val = self.interner.get_or_intern(val);
        self.func.dfg.sconst(val)
    }

    pub fn make_cond<T>(
        &mut self,
        cond: Value,
        mut lower_branch: impl FnMut(&mut Self, bool) -> T,
    ) -> ((Block, T), (Block, T)) {
        let then_dst = self.create_block();
        let else_dst = self.create_block();
        let next_bb = self.create_block();

        // self.insert_block_after(then_dst, self.position.unwrap());
        // self.insert_block_after(else_dst, then_dst);
        // self.insert_block_after(next_bb, else_dst);

        self.ins().br(cond, then_dst, else_dst);
        self.seal_block(then_dst);
        self.seal_block(else_dst);

        self.switch_to_block(then_dst);
        self.ensure_inserted_block();
        let then_val = lower_branch(self, true);
        self.ins().jump(next_bb);
        let then_tail = self.position;

        self.switch_to_block(else_dst);
        self.ensure_inserted_block();
        let else_val = lower_branch(self, false);
        self.ins().jump(next_bb);
        let else_tail = self.position;

        self.switch_to_block(next_bb);
        self.ensure_inserted_block();
        self.seal_block(next_bb);

        ((then_tail, then_val), (else_tail, else_val))
    }

    pub fn make_select(
        &mut self,
        cond: Value,
        lower_branch: impl FnMut(&mut Self, bool) -> Value,
    ) -> Value {
        let (then_src, else_src) = self.make_cond(cond, lower_branch);

        self.ins().phi(&[then_src, else_src])
    }

    /// Insert `block` in the layout *after* the existing block `after`.
    pub fn insert_block_after(&mut self, block: Block, after: Block) {
        self.func.layout.insert_block_after(block, after);
    }

    /// After the call to this function, new instructions will be inserted into the designated
    /// block, in the order they are declared. You must declare the types of the Block arguments
    /// you will use here.
    ///
    /// When inserting the terminator instruction (which doesn't have a fallthrough to its immediate
    /// successor), the block will be declared filled and it will not be possible to append
    /// instructions to it.
    pub fn switch_to_block(&mut self, block: Block) {
        // First we check that the previous block has been filled.
        debug_assert!(
            self.is_unreachable() || self.is_pristine() || self.is_filled(),
            "you have to fill your block before switching"
        );
        // We cannot switch to a filled block
        debug_assert!(
            !self.func_ctx.blocks[block].filled,
            "you cannot switch to a block which is already filled"
        );

        // Then we change the cursor position.
        self.position = block;
    }

    /// Declares that all the predecessors of this block are known.
    ///
    /// Function to call with `block` as soon as the last branch instruction to `block` has been
    /// created. Forgetting to call this method on every block will cause inconsistencies in the
    /// produced functions.
    pub fn seal_block(&mut self, block: Block) {
        self.func_ctx.ssa.seal_block(block, self.func, &mut self.op_dependent_vals);
    }

    pub fn ensured_sealed(&mut self) {
        self.ensure_inserted_block();
        if !self.is_sealed() {
            self.seal_block(self.position)
        }
    }

    /// Effectively calls seal_block on all unsealed blocks in the function.
    ///
    /// It's more efficient to seal `Block`s as soon as possible, during
    /// translation, but for frontends where this is impractical to do, this
    /// function can be used at the end of translating all blocks to ensure
    /// that everything is sealed.
    pub fn seal_all_blocks(&mut self) {
        self.func_ctx.ssa.seal_all_blocks(self.func, &mut self.op_dependent_vals);
    }

    /// Returns the Cranelift IR value corresponding to the utilization at the current program
    /// position of a previously defined user variable.
    pub fn use_var(&mut self, var: Place) -> Value {
        self.ensure_inserted_block();
        self.func_ctx.ssa.use_var(self.func, &mut self.op_dependent_vals, var, self.position)
    }

    /// Register a new definition of a user variable. The type of the value must be
    /// the same as the type registered for the variable.
    pub fn def_var(&mut self, var: Place, val: Value) {
        if self.tag_writes {
            self.func.dfg.set_tag(val, Some(u32::from(var).into()));
        }
        self.func_ctx.ssa.def_var(var, val, self.position);
    }

    /// Register a new definition of a user variable. The type of the value must be
    /// the same as the type registered for the variable.
    pub fn def_var_at(&mut self, var: Place, val: Value, bb: Block) {
        if self.tag_writes {
            self.func.dfg.set_tag(val, Some(u32::from(var).into()));
        }
        self.func_ctx.ssa.def_var(var, val, bb);
    }

    /// Declare an external function import.
    pub fn import_function(&mut self, data: FunctionSignature) -> FuncRef {
        self.func.import_function(data)
    }
    /// Returns an object with the [`InstBuilder`](cranelift_codegen::ir::InstBuilder)
    /// trait that allows to conveniently append an instruction to the current `Block` being built.
    pub fn ins<'short>(&'short mut self) -> InsertBuilder<'short, FuncInstBuilder<'short, 'a>> {
        InsertBuilder::new(FuncInstBuilder::new(self, self.position))
    }

    /// Make sure that the current block is inserted in the layout.
    pub fn ensure_inserted_block(&mut self) {
        let block = self.position;
        if self.func_ctx.blocks[block].pristine {
            if !self.func.layout.is_block_inserted(block) {
                self.func.layout.insert_block(block, self.end)
            }
            self.func_ctx.blocks[block].pristine = false;
        } else {
            debug_assert!(
                !self.func_ctx.blocks[block].filled,
                "you cannot add an instruction to block {}: already filled\n{}",
                self.position,
                self.func.to_debug_string()
            );
        }
    }

    /// Returns a `FuncCursor` pointed at the current position ready for inserting instructions.
    ///
    /// This can be used to insert SSA code that doesn't need to access locals and that doesn't
    /// need to know about `FunctionBuilder` at all.
    pub fn cursor(&mut self) -> FuncCursor {
        self.ensure_inserted_block();
        FuncCursor::new(self.func).with_srcloc(self.srcloc).at_bottom(self.position)
    }

    /// Declare that translation of the current function is complete. This
    /// resets the state of the `FunctionBuilder` in preparation to be used
    /// for another function.
    pub fn finalize(&mut self) {
        // Check that all the `Block`s are filled and sealed.

        if let Some(exit) = self.func.layout.last_block() {
            if !self.func_ctx.ssa.is_sealed(exit) {
                self.seal_block(exit);
            }
        }

        #[cfg(debug_assertions)]
        {
            for (block, block_data) in self.func_ctx.blocks.iter_enumerated() {
                assert!(
                    block_data.pristine || self.func_ctx.ssa.is_sealed(block),
                    "FunctionBuilder finalized, but block {} is not sealed",
                    block,
                );
                assert!(
                    block_data.pristine || block_data.filled,
                    "FunctionBuilder finalized, but block {} is not filled",
                    block,
                );
            }
        }

        self.func.dfg.strip_alias();

        // Clear the state (but preserve the allocated buffers) in preparation
        // for translation another function.
        self.func_ctx.clear();

        // Reset srcloc and position to initial states.
        self.srcloc = Default::default();
        // self.position = Default::default();
    }
}

/// All the functions documented in the previous block are write-only and help you build a valid
/// Cranelift IR functions via multiple debug asserts. However, you might need to improve the
/// performance of your translation perform more complex transformations to your Cranelift IR
/// function. The functions below help you inspect the function you're creating and modify it
/// in ways that can be unsafe if used incorrectly.
impl<'a> FunctionBuilder<'a> {
    /// Returns the result values of an instruction.
    pub fn inst_results(&self, inst: Inst) -> &[Value] {
        self.func.dfg.inst_results(inst)
    }

    ///// Changes the destination of a jump instruction after creation.
    /////
    ///// **Note:** You are responsible for maintaining the coherence with the arguments of
    ///// other jump instructions.
    //pub fn change_jump_destination(&mut self, inst: Inst, new_dest: Block) {
    //    let old_dest = self.func.dfg[inst]
    //        .branch_destination_mut()
    //        .expect("you want to change the jump destination of a non-jump instruction");
    //    let pred = self.func_ctx.ssa.remove_block_predecessor(*old_dest, inst);
    //    *old_dest = new_dest;
    //    self.func_ctx.ssa.declare_block_predecessor(new_dest, pred, inst);
    //}

    /// Returns `true` if and only if the current `Block` is sealed and has no predecessors declared.
    ///
    /// The entry block of a function is never unreachable.
    pub fn is_unreachable(&self) -> bool {
        let is_entry = match self.func.layout.entry_block() {
            None => false,
            Some(entry) => self.position == entry,
        };
        !is_entry
            && self.func_ctx.ssa.is_sealed(self.position)
            && !self.func_ctx.ssa.has_any_predecessors(self.position)
    }

    /// Returns `true` if and only if no instructions have been added since the last call to
    /// `switch_to_block`.
    pub fn is_pristine(&self) -> bool {
        self.func_ctx.blocks[self.position].pristine
    }

    /// Returns `true` if and only if a terminator instruction has been inserted since the
    /// last call to `switch_to_block`.
    pub fn is_filled(&self) -> bool {
        self.func_ctx.blocks[self.position].filled
    }

    pub fn is_sealed(&self) -> bool {
        self.func_ctx.ssa.is_sealed(self.position)
    }
}

// Helper functions
impl<'a> FunctionBuilder<'a> {
    /// A Block is 'filled' when a terminator instruction is present.
    fn fill_current_block(&mut self) {
        self.func_ctx.blocks[self.position].filled = true;
    }

    fn declare_successor(&mut self, dest_block: Block) {
        self.func_ctx.ssa.declare_block_predecessor(dest_block, self.position);
    }
}
