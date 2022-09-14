use std::fmt;
use std::hash::Hash;

use crate::entities::{Block, FuncRef, Use, Value};

#[rustfmt::skip]
mod generated;
pub use generated::*;

/// Some instructions use an external list of argument values because there is not enough space in
/// the 16-byte `InstructionData` struct. These value lists are stored in a memory pool in
/// `dfg.value_lists`.
pub type ValueList = list_pool::ListHandle<Value>;

/// Memory pool for holding value lists. See `ValueList`.
pub type ValueListPool = list_pool::ListPool<Value>;

/// Some instructions use an external list of argument values because there is not enough space in
/// the 16-byte `InstructionData` struct. These value lists are stored in a memory pool in
/// `dfg.value_lists`.
pub type UseList = list_pool::ListHandle<Use>;

/// Memory pool for holding value lists. See `ValueList`.
pub type UseListPool = list_pool::ListPool<Use>;

pub type PhiForest = bforest::MapForest<Block, u32>;
pub type PhiMap = bforest::Map<Block, u32>;

#[derive(Clone, Debug)]
pub enum InstructionData {
    Unary { opcode: Opcode, arg: Value },
    Binary { opcode: Opcode, args: [Value; 2] },
    Branch { cond: Value, then_dst: Block, else_dst: Block, loop_entry: bool },
    // We store blocks as a value list so that we do not need a second pool
    PhiNode(PhiNode),
    Jump { destination: Block },
    Call { func_ref: FuncRef, args: ValueList },
}

impl From<PhiNode> for InstructionData {
    fn from(node: PhiNode) -> Self {
        InstructionData::PhiNode(node)
    }
}

#[test]
fn instruction_data_size() {
    assert_eq!(std::mem::size_of::<InstructionData>(), 16)
}

impl InstructionData {
    pub fn unwrap_phi(&self) -> &PhiNode {
        if let InstructionData::PhiNode(node) = self {
            node
        } else {
            unreachable!()
        }
    }

    pub fn is_phi(&self) -> bool {
        matches!(self, InstructionData::PhiNode(_))
    }

    pub fn is_terminator(&self) -> bool {
        matches!(self, InstructionData::Branch { .. } | InstructionData::Jump { .. })
    }

    pub fn unwrap_phi_mut(&mut self) -> &mut PhiNode {
        if let InstructionData::PhiNode(node) = self {
            node
        } else {
            unreachable!()
        }
    }

    /// Get mutable references to the value arguments to this
    /// instruction.
    ///
    /// # Note
    ///
    /// It is up to the caller to ensure that uses are updates as approriate
    pub fn arguments_mut<'a>(&'a mut self, pool: &'a mut ValueListPool) -> &mut [Value] {
        match self {
            InstructionData::Unary { arg, .. } | InstructionData::Branch { cond: arg, .. } => {
                core::slice::from_mut(arg)
            }
            InstructionData::Binary { args, .. } => &mut *args,
            InstructionData::Call { args, .. } | InstructionData::PhiNode(PhiNode { args, .. }) => {
                args.as_mut_slice(pool)
            }

            InstructionData::Jump { .. } => &mut [],
        }
    }

    /// Get mutable references to the value arguments to this
    /// instruction.
    pub fn arguments<'a>(&'a self, pool: &'a ValueListPool) -> &[Value] {
        match self {
            InstructionData::Unary { arg, .. } | InstructionData::Branch { cond: arg, .. } => {
                core::slice::from_ref(arg)
            }
            InstructionData::Binary { args, .. } => &*args,
            InstructionData::Call { args, .. } | InstructionData::PhiNode(PhiNode { args, .. }) => {
                args.as_slice(pool)
            }

            InstructionData::Jump { .. } => &[],
        }
    }

    pub fn opcode(&self) -> Opcode {
        match self {
            InstructionData::Unary { opcode: op, .. }
            | InstructionData::Binary { opcode: op, .. } => *op,
            InstructionData::Call { .. } => Opcode::Call,
            InstructionData::Jump { .. } => Opcode::Jmp,
            InstructionData::PhiNode { .. } => Opcode::Phi,
            InstructionData::Branch { .. } => Opcode::Br,
        }
    }

    pub fn eq(&self, other: &Self, val_pool: &ValueListPool, forest: &PhiForest) -> bool {
        match (self, other) {
            (
                Self::Unary { opcode: l_op, arg: l_arg },
                Self::Unary { opcode: r_op, arg: r_arg },
            ) => l_op == r_op && l_arg == r_arg,
            (
                Self::Binary { opcode: l_op, args: l_args },
                Self::Binary { opcode: r_op, args: r_args },
            ) => l_op == r_op && l_args == r_args,
            (
                Self::Branch {
                    then_dst: l_then_dst,
                    else_dst: l_else_dst,
                    cond: l_cond,
                    loop_entry: l_loop_entry,
                },
                Self::Branch {
                    then_dst: r_then_dst,
                    else_dst: r_else_dst,
                    cond: r_cond,
                    loop_entry: r_loop_entry,
                },
            ) => {
                l_then_dst == r_then_dst
                    && l_else_dst == r_else_dst
                    && l_cond == r_cond
                    && l_loop_entry == r_loop_entry
            }
            (
                Self::Jump { destination: l_destination },
                Self::Jump { destination: r_destination },
            ) => l_destination == r_destination,
            (
                Self::Call { func_ref: l_func_ref, args: l_args },
                Self::Call { func_ref: r_func_ref, args: r_args },
            ) => l_func_ref == r_func_ref && l_args.as_slice(val_pool) == r_args.as_slice(val_pool),

            (Self::PhiNode(lnode), Self::PhiNode(rnode)) => lnode.eq(rnode, val_pool, forest),

            _ => false,
        }
    }

    pub fn hash<H: std::hash::Hasher>(
        &self,
        state: &mut H,
        val_pool: &ValueListPool,
        forest: &PhiForest,
    ) {
        core::mem::discriminant(self).hash(state);
        match self {
            InstructionData::Unary { opcode: op, arg } => {
                op.hash(state);
                arg.hash(state);
            }
            InstructionData::Binary { opcode: op, args } => {
                op.hash(state);
                args.hash(state);
            }
            InstructionData::Branch { cond, then_dst, else_dst, loop_entry } => {
                cond.hash(state);
                then_dst.hash(state);
                else_dst.hash(state);
                loop_entry.hash(state);
            }
            InstructionData::Jump { destination } => {
                destination.hash(state);
            }
            InstructionData::Call { func_ref, args } => {
                func_ref.hash(state);
                args.as_slice(val_pool).hash(state);
            }
            InstructionData::PhiNode(node) => node.hash(state, val_pool, forest),
        }
    }
}

impl Opcode {
    #[inline]
    pub fn is_branch(self) -> bool {
        matches!(self, Opcode::Jmp | Opcode::Br)
    }

    #[inline]
    pub fn is_call(self) -> bool {
        matches!(self, Opcode::Call)
    }

    #[inline]
    pub const fn constraints(self) -> OpcodeConstraints {
        OPCODE_CONSTRAINTS[self as usize]
    }

    #[inline]
    pub const fn format(self) -> InstructionFormat {
        OPCODE_FORMAT[self as usize]
    }

    #[inline]
    pub const fn name(self) -> &'static str {
        OPCODE_NAMES[self as usize]
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

impl fmt::Debug for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// Value type constraints for a given opcode.
///
/// The `InstructionFormat` determines the constraints on most operands, but `Value` operands and
/// results are not determined by the format. Every `Opcode` has an associated
/// `OpcodeConstraints` object that provides the missing details.
#[derive(Clone, Copy)]
pub struct OpcodeConstraints {
    /// Flags for this opcode encoded as a bit field:
    ///
    /// Bits 0-2:
    ///     Number of fixed result values. This does not include `variable_args` results as are
    ///     produced by call instructions.
    /// Bits 3-5:
    ///     Number of fixed value arguments. The minimum required number of value operands.
    flags: u8,
}

impl OpcodeConstraints {
    const fn new(arg_cnt: u8, ret_cnt: u8) -> OpcodeConstraints {
        OpcodeConstraints { flags: arg_cnt << 3 | ret_cnt }
    }
    /// Get the number of *fixed* result values produced by this opcode.
    /// This does not include `variable_args` produced by calls.
    pub fn num_fixed_results(self) -> usize {
        (self.flags & 0x7) as usize
    }

    /// Get the number of *fixed* input values required by this opcode.
    ///
    /// This does not include `variable_args` arguments on call and branch instructions.
    ///
    /// The number of fixed input values is usually implied by the instruction format, but
    /// instruction formats that use a `ValueList` put both fixed and variable arguments in the
    /// list. This method returns the *minimum* number of values required in the value list.
    pub fn num_fixed_value_arguments(self) -> usize {
        ((self.flags >> 3) & 0x7) as usize
    }
}

#[derive(Clone, Debug)]
pub struct PhiNode {
    pub args: ValueList,
    pub blocks: PhiMap,
}

impl PhiNode {
    #[inline]
    pub fn eq(&self, other: &Self, val_pool: &ValueListPool, forest: &PhiForest) -> bool {
        let l_edges = self.edges(val_pool, forest);
        let r_edges = other.edges(val_pool, forest);
        l_edges.eq(r_edges)
    }

    #[inline]
    pub fn hash<H: std::hash::Hasher>(
        &self,
        state: &mut H,
        val_pool: &ValueListPool,
        forest: &PhiForest,
    ) {
        for (block, val) in self.edges(val_pool, forest) {
            block.hash(state);
            val.hash(state)
        }
    }

    #[inline]
    pub fn edge_val(
        &self,
        block: Block,
        val_pool: &ValueListPool,
        forest: &PhiForest,
    ) -> Option<Value> {
        let pos = self.edge_operand(block, forest)?;
        Some(self.args.as_slice(val_pool)[pos as usize])
    }

    #[inline]
    pub fn edge_operand(&self, block: Block, forest: &PhiForest) -> Option<u32> {
        self.blocks.get(block, forest, &())
    }

    #[inline]
    pub fn edges<'a>(&self, value_lists: &'a ValueListPool, forest: &'a PhiForest) -> PhiEdges<'a> {
        let args = self.args.as_slice(value_lists);
        PhiEdges { iter: self.blocks.iter(forest), args }
    }
}

#[derive(Clone, Copy)]
pub struct PhiEdges<'a> {
    iter: bforest::MapIter<'a, Block, u32>,
    args: &'a [Value],
}

impl Iterator for PhiEdges<'_> {
    type Item = (Block, Value);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(move |(block, pos)| (block, self.args[pos as usize]))
    }
}

impl Opcode {
    #[inline]
    pub fn is_commutative(self) -> bool {
        matches!(
            self,
            Opcode::Fmul
                | Opcode::Fadd
                | Opcode::Iand
                | Opcode::Ixor
                | Opcode::Ior
                | Opcode::Iadd
                | Opcode::Imul
                | Opcode::Ieq
                | Opcode::Feq
                | Opcode::Beq
                | Opcode::Seq
                | Opcode::Ine
                | Opcode::Fne
                | Opcode::Bne
                | Opcode::Sne
        )
    }
}
