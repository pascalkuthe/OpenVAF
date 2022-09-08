use std::iter;
use std::ops::{Index, IndexMut};

use stdx::iter::zip;
use typed_index_collections::{TiSliceKeys, TiVec};

use crate::dfg::values::{DfgValues, ValueDataType};
use crate::entities::Tag;
use crate::instructions::{UseList, UseListPool};
use crate::{DataFlowGraph, Inst, InstructionData, Use, Value, ValueList, ValueListPool};

#[derive(Clone)]
pub struct DfgInsructions {
    /// Data about all of the instructions in the function, including opcodes and operands.
    /// The instructions in this map are not in program order. That is tracked by `Layout`, along
    /// with the block containing each instruction.
    pub(super) declarations: TiVec<Inst, InstructionData>,

    /// List of result values for each instruction.
    ///
    /// This map gets resized automatically by `make_inst()` so it is always in sync with the
    /// primary `insts` map.
    pub results: TiVec<Inst, ValueList>,

    /// List of uses for each instruction.
    ///
    /// This map gets resized automatically by `make_inst()` so it is always in sync with the
    /// primary `insts` map.
    pub(super) uses: TiVec<Inst, UseList>,

    pub value_lists: ValueListPool,
    pub use_lists: UseListPool,
}

impl Default for DfgInsructions {
    fn default() -> Self {
        Self::new()
    }
}

impl DfgInsructions {
    pub fn new() -> Self {
        Self {
            declarations: TiVec::new(),
            results: TiVec::new(),
            uses: TiVec::new(),
            value_lists: ValueListPool::new(),
            use_lists: UseListPool::new(),
        }
    }

    pub fn clear(&mut self) {
        self.declarations.clear();
        self.results.clear();
        self.uses.clear();
        self.value_lists.clear();
        self.use_lists.clear();
    }

    pub fn iter(&self) -> TiSliceKeys<Inst> {
        self.declarations.keys()
    }

    /// Get the total number of instructions created in this function, whether they are currently
    /// inserted in the layout or not.
    ///
    /// This is intended for use with `SecondaryMap::with_capacity`.
    pub fn num(&self) -> usize {
        self.declarations.len()
    }

    pub fn is_valid(&self, inst: Inst) -> bool {
        usize::from(inst) < self.declarations.len()
    }

    /// Removes all uses of the instruction
    pub fn zap(&self, inst: Inst, values: &mut DfgValues) {
        let uses = self.uses[inst].as_slice(&self.use_lists);
        for use_ in uses {
            values.detach_use(*use_, self);
        }
    }

    pub fn safe_to_remove(&self, inst: Inst, values: &DfgValues) -> bool {
        self.results(inst).iter().all(|res| values.is_dead(*res))
    }

    ///// Replace the results of one instruction with aliases to the results of another.
    /////
    ///// Change all the results of `dest_inst` to behave as aliases of
    ///// corresponding results of `src_inst`, as if calling change_to_alias for
    ///// each.
    /////
    ///// After calling this instruction, `dest_inst` will have had its results
    ///// cleared, so it likely needs to be removed from the graph.
    /////
    //pub fn replace_with_aliases(&mut self, dest_inst: Inst, src: &[Value]) {
    //    debug_assert_ne!(
    //        dest_inst, src_inst,
    //        "Replacing {} with itself would create a loop",
    //        dest_inst
    //    );
    //    debug_assert_eq!(
    //        self.results[dest_inst].len(&self.value_lists),
    //        self.results[src_inst].len(&self.value_lists),
    //        "Replacing {} with {} would produce a different number of results.",
    //        dest_inst,
    //        src_inst
    //    );

    //    for (&dest, &src) in self.results[dest_inst]
    //        .as_slice(&self.value_lists)
    //        .iter()
    //        .zip(self.results[src_inst].as_slice(&self.value_lists))
    //    {
    //        let original = src;

    //        self.values[dest] = ValueData::Alias { original };
    //    }

    //    self.clear_results(dest_inst);
    //}

    /// Get all value arguments on `inst` as a slice.
    pub fn args(&self, inst: Inst) -> &[Value] {
        self.declarations[inst].arguments(&self.value_lists)
    }

    /// Get all value arguments on `inst` as a mutable slice.
    pub fn args_mut(&mut self, inst: Inst) -> &mut [Value] {
        self.declarations[inst].arguments_mut(&mut self.value_lists)
    }

    /// Detach the list of result values from `inst` and return it.
    ///
    /// This leaves `inst` without any result values. New result values can be created by calling
    /// `make_inst_results` or by using a `replace(inst)` builder.
    pub fn detach_results(&mut self, inst: Inst) -> ValueList {
        self.results[inst].take()
    }

    /// Clear the list of result values from `inst`.
    ///
    /// This leaves `inst` without any result values. New result values can be created by calling
    /// `make_inst_results` or by using a `replace(inst)` builder.
    pub fn clear_results(&mut self, inst: Inst) {
        self.results[inst].clear(&mut self.value_lists)
    }

    /// Get the first result of an instruction.
    ///
    /// This function panics if the instruction doesn't have any result.
    pub fn first_result(&self, inst: Inst) -> Value {
        self.results[inst].first(&self.value_lists).expect("Instruction has no results")
    }

    /// Test if `inst` has any result values currently.
    pub fn has_results(&self, inst: Inst) -> bool {
        !self.results[inst].is_empty()
    }

    /// Return all the results of an instruction.
    pub fn results(&self, inst: Inst) -> &[Value] {
        self.results[inst].as_slice(&self.value_lists)
    }

    /// Return all the uses of an instruction.
    pub fn operands(&self, inst: Inst) -> &[Use] {
        self.uses[inst].as_slice(&self.use_lists)
    }

    // /// Return all the uses of an instruction.
    // pub fn operands_mut(&mut self, inst: Inst) -> &mut [Use] {
    //     self.uses[inst].as_mut_slice(&mut self.use_lists)
    // }
}

/// Operations that require mutable access to `values` and `insts` (buit logically still belong to
/// instructions)
impl DataFlowGraph {
    /// Append a new instruction result value to `inst`.
    pub fn append_result(&mut self, inst: Inst, tag: Option<Tag>) -> Value {
        let res = self.values.defs.next_key();
        let num = self.insts.results[inst].push(res, &mut self.insts.value_lists);
        debug_assert!(num <= u16::MAX as usize, "Too many result values");
        self.values.make(ValueDataType::Inst { inst, num: num as u16 }, tag)
    }

    /// Attach an existing value to the result value list for `inst`.
    ///
    /// The `res` value is appended to the end of the result list.
    ///
    /// This is a very low-level operation. Usually, instruction results are
    /// created automatically. The `res` value must not be attached to anything else.
    pub fn attach_result(&mut self, inst: Inst, res: Value) {
        debug_assert!(!self.value_attached(res));
        let num = self.insts.results[inst].push(res, &mut self.insts.value_lists);
        debug_assert!(num <= u16::MAX as usize, "Too many result values");
        self.values.defs[res].ty = ValueDataType::Inst { num: num as u16, inst };
    }

    /// Create result values for `inst`, reusing the provided detached values.
    ///
    /// Create a new set of result values for `inst` using `ctrl_typevar` to determine the result
    /// types. Any values provided by `reuse` will be reused. When `reuse` is exhausted or when it
    /// produces `None`, a new value is created.
    pub fn make_inst_results_reusing<I>(&mut self, inst: Inst, reuse: I) -> usize
    where
        I: Iterator<Item = Option<Value>>,
    {
        let mut reuse = reuse.fuse();

        self.insts.results[inst].clear(&mut self.insts.value_lists);

        // Get the call signature if this is a function call.
        let num_results = if let Some(sig) = self.call_signature(inst) {
            sig.returns as usize
        } else {
            // Create result values corresponding to the opcode's constraints.
            let constraints = self.insts.declarations[inst].opcode().constraints();
            constraints.num_fixed_results()
        };

        for _res_idx in 0..num_results {
            if let Some(Some(v)) = reuse.next() {
                self.attach_result(inst, v);
            } else {
                self.append_result(inst, None);
            }
        }
        num_results
    }

    /// Create a new instruction.
    ///
    /// The type of the first result is indicated by `data.ty`. If the instruction produces
    /// multiple results, also call `make_inst_results` to allocate value table entries.
    pub fn make_inst(&mut self, data: InstructionData) -> Inst {
        // add instructions
        self.insts.uses.push(UseList::new());
        self.insts.results.push(ValueList::new());
        let inst = self.insts.declarations.push_and_get_key(data);

        // update use list
        let args = self.insts.declarations[inst].arguments(&self.insts.value_lists);
        let args = args.iter().copied();

        let uses = args.enumerate().map(|(i, arg)| self.values.make_use(arg, inst, i as u16));
        self.insts.uses[inst].extend(uses, &mut self.insts.use_lists);
        inst
    }

    pub fn update_inst(&mut self, inst: Inst, data: InstructionData) {
        self.zap_inst(inst);
        self.insts[inst] = data;
        self.update_inst_uses(inst);
    }

    pub fn update_inst_uses(&mut self, inst: Inst) {
        let data = self.insts.declarations[inst].clone();
        let pool = &mut self.insts.use_lists;

        let uses = self.insts.uses[inst].clone();
        let args = self.insts.declarations[inst].arguments(&self.insts.value_lists);
        let mut args = args.iter().copied();

        for (use_, val) in zip(uses.as_slice(pool), &mut args) {
            self.values.attach_use(*use_, val);
        }

        let arg_len = data.arguments(&self.insts.value_lists).len();
        let use_len = uses.len(pool);

        if arg_len > use_len {
            // make new uses for the remaining uses
            let new_uses = args
                .enumerate()
                .map(|(i, arg)| self.values.make_use(arg, inst, (use_len + i) as u16));

            self.insts.uses[inst].extend(new_uses, pool)
        } else {
            // remove excess uses
            self.insts.uses[inst].truncate(arg_len, pool);
        }
    }

    /// Create result values for an instruction that produces multiple results.
    ///
    /// Instructions that produce no result values only need to be created with `make_inst`,
    /// otherwise call `make_inst_results` to allocate value table entries for the results.
    ///
    /// The result value types are determined from the instruction's value type constraints and the
    /// provided `ctrl_typevar` type for polymorphic instructions. For non-polymorphic
    /// instructions, `ctrl_typevar` is ignored, and `INVALID` can be used.
    ///
    /// The type of the first result value is also set, even if it was already set in the
    /// `InstructionData` passed to `make_inst`. If this function is called with a single-result
    /// instruction, that is the only effect.
    pub fn make_inst_results(&mut self, inst: Inst) -> usize {
        self.make_inst_results_reusing(inst, iter::empty())
    }
}

/// Allow immutable access to instructions via indexing.
impl Index<Inst> for DfgInsructions {
    type Output = InstructionData;

    fn index(&self, inst: Inst) -> &InstructionData {
        &self.declarations[inst]
    }
}

/// Allow mutable access to instructions via indexing.
impl IndexMut<Inst> for DfgInsructions {
    fn index_mut(&mut self, inst: Inst) -> &mut InstructionData {
        &mut self.declarations[inst]
    }
}
