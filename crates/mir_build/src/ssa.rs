//! A SSA-building API that handles incomplete CFGs.
//!
//! The algorithm is based upon Braun M., Buchwald S., Hack S., Leißa R., Mallon C.,
//! Zwinkau A. (2013) Simple and Efficient Construction of Static Single Assignment Form.
//! In: Jhala R., De Bosschere K. (eds) Compiler Construction. CC 2013.
//! Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg
//!
//! <https://link.springer.com/content/pdf/10.1007/978-3-642-37051-9_6.pdf>

use bforest::Map;
use bitset::HybridBitSet;
use mir::builder::InstBuilderBase;
use mir::cursor::{Cursor, FuncCursor};
use mir::{Block, Function, PhiNode, Value, ValueList, GRAVESTONE};
use smallvec::SmallVec;
use stdx::packed_option::PackedOption;
use typed_index_collections::TiVec;

use crate::Place;

/// Structure containing the data relevant the construction of SSA for a given function.
///
/// The parameter struct `Variable` corresponds to the way variables are represented in the
/// non-SSA language you're translating from.
///
/// The SSA building relies on information about the variables used and defined.
///
/// This SSA building module allows you to def and use variables on the fly while you are
/// constructing the CFG, no need for a separate SSA pass after the CFG is completed.
///
/// A basic block is said _filled_ if all the instruction that it contains have been translated,
/// and it is said _sealed_ if all of its predecessors have been declared. Only filled predecessors
/// can be declared.
pub struct SSABuilder {
    // TODO: Consider a sparse representation rather than SecondaryMap-of-SecondaryMap.
    /// Records for every variable and for every relevant block, the last definition of
    /// the variable in the block.
    variables: TiVec<Place, TiVec<Block, PackedOption<Value>>>,

    /// Records the position of the basic blocks and the list of values used but not defined in the
    /// block.
    ssa_blocks: TiVec<Block, SSABlockData>,
    // visited: BitSet<Block>,
    /// Call stack for use in the `use_var`/`predecessors_lookup` state machine.
    calls: Vec<Call>,
    /// Result stack for use in the `use_var`/`predecessors_lookup` state machine.
    results: Vec<Value>,
}

#[derive(Clone, Default)]
struct SSABlockData {
    // The predecessors of the Block with the block and branch instruction.
    // The elemets of this array MUST be kept in order.
    // Ideally only `add_predecessor` is used to modify it
    predecessors: SmallVec<[Block; 4]>,
    // A block is sealed if all of its predecessors have been declared.
    sealed: bool,
    // List of current phis for which an earlier def has not been found yet.
    undef_phis: Vec<(Place, Value)>,
}

impl SSABlockData {
    fn add_predecessor(&mut self, new_pred: Block) {
        debug_assert!(!self.sealed, "sealed blocks cannot accept new predecessors");
        if let Some(i) = self.predecessors.iter().position(|&e| e >= new_pred) {
            debug_assert_ne!(self.predecessors[i], new_pred);
            self.predecessors.insert(i, new_pred);
        } else {
            // `elem` is larger than all existing elements.
            self.predecessors.push(new_pred);
        };
    }

    // fn remove_predecessor(&mut self, block: Block) {
    //     let pos = self.predecessors.iter().position(|it| *it == block).unwrap();
    //     self.predecessors.swap_remove(pos);
    // }
}

impl SSABuilder {
    /// Allocate a new blank SSA builder struct. Use the API function to interact with the struct.
    pub fn new() -> Self {
        Self {
            variables: TiVec::new(),
            ssa_blocks: TiVec::new(),
            calls: Vec::new(),
            results: Vec::new(),
            // visited: BitSet::new_empty(0),
        }
    }

    /// Clears a `SSABuilder` from all its data, letting it in a pristine state without
    /// deallocating memory.
    pub fn clear(&mut self) {
        self.variables.clear();
        self.ssa_blocks.clear();
        debug_assert!(self.calls.is_empty());
        debug_assert!(self.results.is_empty());
    }

    /// Tests whether an `SSABuilder` is in a cleared state.
    pub fn is_empty(&self) -> bool {
        self.variables.is_empty()
            && self.ssa_blocks.is_empty()
            && self.calls.is_empty()
            && self.results.is_empty()
    }
}

impl Default for SSABuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Small enum used for clarity in some functions.
#[derive(Debug)]
enum ZeroOneOrMore<T> {
    Zero,
    One(T),
    More,
}

#[derive(Debug)]
/// States for the `use_var`/`predecessors_lookup` state machine.
enum Call {
    UseVar(Block),
    FinishSealedOnePredecessor(Block),
    FinishPredecessorsLookup(Value, Block),
}

/// The following methods are the API of the SSA builder. Here is how it should be used when
/// translating to Cranelift IR:
///
/// - for each basic block, create a corresponding data for SSA construction with `declare_block`;
///
/// - while traversing a basic block and translating instruction, use `def_var` and `use_var`
///   to record definitions and uses of variables, these methods will give you the corresponding
///   SSA values;
///
/// - when all the instructions in a basic block have translated, the block is said _filled_ and
///   only then you can add it as a predecessor to other blocks with `declare_block_predecessor`;
///
/// - when you have constructed all the predecessor to a basic block,
///   call `seal_block` on it with the `Function` that you are building.
///
/// This API will give you the correct SSA values to use as arguments of your instructions,
/// as well as modify the jump instruction and `Block` parameters to account for the SSA
/// Phi functions.
///
impl SSABuilder {
    /// Declares a new definition of a variable in a given basic block.
    /// The SSA value is passed as an argument because it should be created with
    /// `ir::DataFlowGraph::append_result`.
    pub fn def_var(&mut self, var: Place, val: Value, block: Block) {
        if usize::from(var) >= self.variables.len() {
            self.variables.resize(usize::from(var) + 1, TiVec::default());
        }
        let dst = &mut self.variables[var];
        if usize::from(block) >= dst.len() {
            dst.resize(usize::from(block) + 1, None.into());
        }
        dst[block] = PackedOption::from(val);
    }

    /// Declares a use of a variable in a given basic block. Returns the SSA value corresponding
    /// to the current SSA definition of this variable and a list of newly created Blocks that
    /// are the results of critical edge splitting for `br_table` with arguments.
    ///
    /// If the variable has never been defined in this blocks or recursively in its predecessors,
    /// this method will silently create an initializer that might contain invalid data. You are
    /// responsible for making sure that you initialize your variables.
    pub fn use_var(&mut self, func: &mut Function, var: Place, block: Block) -> Value {
        // First, try Local Value Numbering (Algorithm 1 in the paper).
        // If the variable already has a known Value in this block, use that.
        if let Some(var_defs) = self.variables.get(var) {
            if let Some(val) = var_defs.get(block).and_then(|val| val.expand()) {
                return val;
            }
        }

        // Otherwise, use Global Value Numbering (Algorithm 2 in the paper).
        // This resolves the Value with respect to its predecessors.
        debug_assert!(self.calls.is_empty());
        debug_assert!(self.results.is_empty());

        // Prepare the 'calls' and 'results' stacks for the state machine.
        self.use_var_nonlocal(func, var, block);

        self.run_state_machine(func, var)
    }

    /// There are two conditions for being able to optimize the lookup of a non local var:
    ///  * The block must have a single predecessor
    ///  * The block cannot be part of a predecessor loop
    ///
    /// To check for these conditions we perform a graph search over block predecessors
    /// marking visited blocks and aborting if we find a previously seen block.
    /// We stop the search if we find a block with multiple predecessors since the
    /// original algorithm can handle these cases.
    fn can_optimize_var_lookup(&mut self, block: Block, block_cnt: usize) -> bool {
        // Check that the initial block only has one predecessor. This is only a requirement
        // for the first block.
        if self.predecessors(block).len() != 1 {
            return false;
        }

        let mut visited = HybridBitSet::new_empty();
        let mut current = block;
        loop {
            let predecessors = self.predecessors(current);

            // We haven't found the original block and we have either reached the entry
            // block, or we found the end of this line of dead blocks, either way we are
            // safe to optimize this line of lookups.
            if predecessors.is_empty() {
                return true;
            }

            // We can stop the search here, the algorithm can handle these cases, even if they are
            // in an undefined island.
            if predecessors.len() > 1 {
                return true;
            }

            if visited.contains(current) {
                return false;
            }

            let next = predecessors[0];
            visited.insert(current, block_cnt);
            current = next;
        }
    }

    /// Resolve the minimal SSA Value of `var` in `block` by traversing predecessors.
    ///
    /// This function sets up state for `run_state_machine()` but does not execute it.
    fn use_var_nonlocal(&mut self, func: &mut Function, var: Place, block: Block) {
        let optimize_var_lookup = self.can_optimize_var_lookup(block, func.layout.num_blocks());
        let data = &mut self.ssa_blocks[block];
        if data.sealed {
            // Optimize the common case of one predecessor: no param needed.
            if optimize_var_lookup {
                let pred = data.predecessors[0];
                self.calls.push(Call::FinishSealedOnePredecessor(block));
                self.calls.push(Call::UseVar(pred));
            } else {
                // Break potential cycles by eagerly adding a sentinel value
                let val = func.dfg.make_invalid_value_for_parser();

                // Define the operandless param added above to prevent lookup cycles.
                self.def_var(var, val, block);

                // Look up a use_var for each precessor.
                self.begin_predecessors_lookup(val, block);
            }
        } else {
            let val = func.dfg.make_invalid_value_for_parser();
            data.undef_phis.push((var, val));
            // Define the operandless param added above to prevent lookup cycles.
            self.def_var(var, val, block);

            // Nothing more can be known at this point.
            self.results.push(val);
        };
    }

    /// For blocks with a single predecessor, once we've determined the value,
    /// record a local def for it for future queries to find.
    fn finish_sealed_one_predecessor(&mut self, var: Place, block: Block) {
        let val = *self.results.last().unwrap();
        self.def_var(var, val, block);
    }

    /// Declares a new basic block to construct corresponding data for SSA construction.
    /// No predecessors are declared here and the block is not sealed.
    /// Predecessors have to be added with `declare_block_predecessor`.
    pub fn declare_block(&mut self) {
        self.ssa_blocks.push(SSABlockData {
            predecessors: SmallVec::new(),
            sealed: false,
            undef_phis: Vec::new(),
        })
    }

    /// Declares a new predecessor for a `Block`
    ///
    /// The precedent `Block` must be filled before added as predecessor.
    /// Note that you must provide no jump arguments to the branch
    /// instruction when you create it since `SSABuilder` will fill them for you.
    ///
    /// Callers are expected to avoid adding the same predecessor more than once in the case
    /// of a jump table.
    pub fn declare_block_predecessor(&mut self, block: Block, pred: Block) {
        debug_assert!(!self.is_sealed(block));
        self.ssa_blocks[block].add_predecessor(pred)
    }

    ///// Remove a previously declared Block predecessor
    /////
    ///// Note: use only when you know what you are doing, this might break the SSA building problem
    //pub fn remove_block_predecessor(&mut self, block: Block) {
    //    debug_assert!(!self.is_sealed(block));
    //    self.ssa_blocks[block].remove_predecessor(block);
    //}

    /// Completes the global value numbering for a `Block`, all of its predecessors having been
    /// already sealed.
    ///
    /// This method modifies the function's `Layout` by adding arguments to the `Block`s to
    /// take into account the Phi function placed by the SSA algorithm.
    ///
    /// Returns the list of newly created blocks for critical edge splitting.
    pub fn seal_block(&mut self, block: Block, func: &mut Function) {
        self.seal_one_block(block, func);
    }

    /// Completes the global value numbering for all unsealed `Block`s in `func`.
    ///
    /// It's more efficient to seal `Block`s as soon as possible, during
    /// translation, but for frontends where this is impractical to do, this
    /// function can be used at the end of translating all blocks to ensure
    /// that everything is sealed.
    pub fn seal_all_blocks(&mut self, func: &mut Function) {
        // Seal all `Block`s currently in the function. This can entail splitting
        // and creation of new blocks, however such new blocks are sealed on
        // the fly, so we don't need to account for them here.
        for block in self.ssa_blocks.keys() {
            if !self.is_sealed(block) {
                self.seal_one_block(block, func);
            }
        }
    }

    /// Helper function for `seal_block` and
    /// `seal_all_blocks`.
    fn seal_one_block(&mut self, block: Block, func: &mut Function) {
        let block_data = &mut self.ssa_blocks[block];
        debug_assert!(!block_data.sealed, "Attempting to seal {} which is already sealed.", block);

        // Extract the undef_variables data from the block so that we
        // can iterate over it without borrowing the whole builder.
        let undef_vars = std::mem::take(&mut block_data.undef_phis);

        // For each undef var we look up values in the predecessors and create a block parameter
        // only if necessary.
        for (var, val) in undef_vars {
            self.predecessors_lookup(func, val, var, block);
        }
        self.mark_block_sealed(block);
    }

    /// Set the `sealed` flag for `block`.
    fn mark_block_sealed(&mut self, block: Block) {
        // Then we mark the block as sealed.
        let block_data = &mut self.ssa_blocks[block];
        debug_assert!(!block_data.sealed);
        debug_assert!(block_data.undef_phis.is_empty());
        block_data.sealed = true;

        // We could call data.predecessors.shrink_to_fit() here, if
        // important, because no further predecessors will be added
        // to this block.
    }

    /// Given the local SSA Value of a Variable in a Block, perform a recursive lookup on
    /// predecessors to determine if it is redundant with another Value earlier in the CFG.
    ///
    /// If such a Value exists and is redundant, the local Value is replaced by the
    /// corresponding non-local Value. If the original Value was a Block parameter,
    /// the parameter may be removed if redundant. Parameters are placed eagerly by callers
    /// to avoid infinite loops when looking up a Value for a Block that is in a CFG loop.
    ///
    /// Doing this lookup for each Value in each Block preserves SSA form during construction.
    ///
    /// Returns the chosen Value.
    ///
    /// ## Arguments
    ///
    /// `sentinel` is a dummy Block parameter inserted by `use_var_nonlocal()`.
    /// Its purpose is to allow detection of CFG cycles while traversing predecessors.
    ///
    /// The `sentinel: Value` and the `ty: Type` are describing the `var: Variable`
    /// that is being looked up.
    fn predecessors_lookup(
        &mut self,
        func: &mut Function,
        sentinel: Value,
        var: Place,
        block: Block,
    ) -> Value {
        debug_assert!(self.calls.is_empty());
        debug_assert!(self.results.is_empty());
        // self.side_effects may be non-empty here so that callers can
        // accumulate side effects over multiple calls.
        self.begin_predecessors_lookup(sentinel, block);
        self.run_state_machine(func, var)
    }

    /// Set up state for `run_state_machine()` to initiate non-local use lookups
    /// in all predecessors of `dest_block`, and arrange for a call to
    /// `finish_predecessors_lookup` once they complete.
    fn begin_predecessors_lookup(&mut self, sentinel: Value, dest_block: Block) {
        self.calls.push(Call::FinishPredecessorsLookup(sentinel, dest_block));
        // Iterate over the predecessors.
        let mut calls = std::mem::take(&mut self.calls);
        calls.extend(self.predecessors(dest_block).iter().rev().map(|pred| Call::UseVar(*pred)));
        self.calls = calls;
    }

    /// Examine the values from the predecessors and compute a result value, creating
    /// block parameters as needed.
    fn finish_predecessors_lookup(
        &mut self,
        func: &mut Function,
        sentinel: Value,
        var: Place,
        dest_block: Block,
    ) {
        let mut pred_values: ZeroOneOrMore<Value> = ZeroOneOrMore::Zero;

        // Determine how many predecessors are yielding unique, non-temporary Values.
        let num_predecessors = self.predecessors(dest_block).len();
        for &pred_val in self.results.iter().rev().take(num_predecessors) {
            match pred_values {
                ZeroOneOrMore::Zero => {
                    if pred_val != sentinel {
                        pred_values = ZeroOneOrMore::One(pred_val);
                    }
                }
                ZeroOneOrMore::One(old_val) => {
                    if pred_val != sentinel && pred_val != old_val {
                        pred_values = ZeroOneOrMore::More;
                        break;
                    }
                }
                ZeroOneOrMore::More => {
                    break;
                }
            }
        }

        // Those predecessors' Values have been examined: pop all their results.
        self.results.truncate(self.results.len() - num_predecessors);

        let result_val = match pred_values {
            ZeroOneOrMore::Zero => {
                // The variable is used but never defined before. This is an irregularity in the
                // code, but rather than throwing an error we silently initialize the variable to
                // 0. This will have no effect since this situation happens in unreachable code.
                func.dfg.values.make_alias_at(GRAVESTONE, sentinel);
                GRAVESTONE
            }
            ZeroOneOrMore::One(pred_val) => {
                // Here all the predecessors use a single value to represent our variable
                // so we don't need to have it as a block argument.
                // We need to replace all the occurrences of val with pred_val but since
                // we can't afford a re-writing pass right now we just declare an alias.
                // Resolve aliases eagerly so that we can check for cyclic aliasing,
                // which can occur in unreachable code.
                let mut resolved = func.dfg.resolve_alias(pred_val);
                if sentinel == resolved {
                    // Cycle detected. Break it by creating a GRAVESTONE
                    resolved = GRAVESTONE;
                }
                func.dfg.values.make_alias_at(resolved, sentinel);
                resolved
            }
            ZeroOneOrMore::More => {
                // There is disagreement in the predecessors on which value to use so we have
                // to create a phi

                let mut args = ValueList::new();
                let mut blocks = Map::new();
                let iter = self.ssa_blocks[dest_block].predecessors.iter().map(|pred_block| {
                    // We already did a full `use_var` above, so we can do just the fast path.
                    let pred_val = self.variables[var][*pred_block].unwrap();
                    let i = args.push(pred_val, &mut func.dfg.insts.value_lists) as u32;
                    (*pred_block, i)
                });

                blocks.insert_sorted_iter(iter, &mut func.dfg.phi_forest, &(), |old, it| {
                    debug_assert_eq!(old, None);
                    it
                });

                FuncCursor::new(func)
                    .at_first_insertion_point(dest_block)
                    .ins()
                    .with_result(sentinel)
                    .build(PhiNode { blocks, args }.into());

                sentinel
            }
        };

        self.results.push(result_val);
    }

    /// Returns the list of `Block`s that have been declared as predecessors of the argument.
    fn predecessors(&self, block: Block) -> &[Block] {
        &self.ssa_blocks[block].predecessors
    }

    /// Returns whether the given Block has any predecessor or not.
    pub fn has_any_predecessors(&self, block: Block) -> bool {
        !self.predecessors(block).is_empty()
    }

    /// Returns `true` if and only if `seal_block` has been called on the argument.
    pub fn is_sealed(&self, block: Block) -> bool {
        self.ssa_blocks[block].sealed
    }

    /// The main algorithm is naturally recursive: when there's a `use_var` in a
    /// block with no corresponding local defs, it recurses and performs a
    /// `use_var` in each predecessor. To avoid risking running out of callstack
    /// space, we keep an explicit stack and use a small state machine rather
    /// than literal recursion.
    fn run_state_machine(&mut self, func: &mut Function, var: Place) -> Value {
        // Process the calls scheduled in `self.calls` until it is empty.
        while let Some(call) = self.calls.pop() {
            match call {
                Call::UseVar(ssa_block) => {
                    // First we lookup for the current definition of the variable in this block
                    if let Some(var_defs) = self.variables.get(var) {
                        if let Some(val) = var_defs.get(ssa_block).and_then(|val| val.expand()) {
                            let val = func.dfg.resolve_alias(val);
                            self.results.push(val);
                            continue;
                        }
                    }
                    self.use_var_nonlocal(func, var, ssa_block);
                }
                Call::FinishSealedOnePredecessor(ssa_block) => {
                    self.finish_sealed_one_predecessor(var, ssa_block);
                }
                Call::FinishPredecessorsLookup(sentinel, dest_block) => {
                    self.finish_predecessors_lookup(func, sentinel, var, dest_block);
                }
            }
        }
        debug_assert_eq!(self.results.len(), 1);
        self.results.pop().unwrap()
    }
}
