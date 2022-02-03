use std::fmt;
use std::hash::Hash;

use lasso::Spur;

use crate::entities::{Block, FuncRef, Value};
use crate::immediates::Ieee64;

#[cfg(test)]
pub(crate) mod tests;

#[rustfmt::skip]
mod generated;
pub use generated::*;

/// Some instructions use an external list of argument values because there is not enough space in
/// the 16-byte `InstructionData` struct. These value lists are stored in a memory pool in
/// `dfg.value_lists`.
pub type ValueList = cranelift_entity::EntityList<Value>;

/// Memory pool for holding value lists. See `ValueList`.
pub type ValueListPool = cranelift_entity::ListPool<Value>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum LoopTag {
    Entry,
    Exit,
    None,
}

#[derive(Clone, Copy, Debug)]
pub enum InstructionData {
    Unary { op: Opcode, arg: Value },
    Binary { op: Opcode, args: [Value; 2] },
    Branch { op: Opcode, args: ValueList, destination: Block, loop_tag: LoopTag },
    Jump { args: ValueList, destination: Block },
    Call { func_ref: FuncRef, args: ValueList },
    UnaryIeee64 { imm: Ieee64 },
    UnaryStr { imm: Spur },
    UnaryInt { imm: i32 },
    UnaryBool { imm: bool },
}

#[test]
fn instruction_data_size() {
    assert_eq!(std::mem::size_of::<InstructionData>(), 16)
}

impl InstructionData {
    /// Get mutable references to the value arguments to this
    /// instruction.
    pub fn arguments_mut<'a>(&'a mut self, pool: &'a mut ValueListPool) -> &mut [Value] {
        match self {
            InstructionData::Unary { arg, .. } => core::slice::from_mut(arg),
            InstructionData::Binary { args, .. } => &mut *args,
            InstructionData::Call { args, .. }
            | InstructionData::Branch { args, .. }
            | InstructionData::Jump { args, .. } => args.as_mut_slice(pool),

            InstructionData::UnaryIeee64 { .. }
            | InstructionData::UnaryStr { .. }
            | InstructionData::UnaryInt { .. }
            | InstructionData::UnaryBool { .. } => &mut [],
        }
    }

    /// Get mutable references to the value arguments to this
    /// instruction.
    pub fn arguments<'a>(&'a self, pool: &'a ValueListPool) -> &[Value] {
        match self {
            InstructionData::Unary { arg, .. } => core::slice::from_ref(arg),
            InstructionData::Binary { args, .. } => &*args,
            InstructionData::Call { args, .. }
            | InstructionData::Branch { args, .. }
            | InstructionData::Jump { args, .. } => args.as_slice(pool),

            InstructionData::UnaryIeee64 { .. }
            | InstructionData::UnaryStr { .. }
            | InstructionData::UnaryInt { .. }
            | InstructionData::UnaryBool { .. } => &[],
        }
    }

    /// Take out the value list with all the value arguments and return
    /// it.
    ///
    /// This leaves the value list in the instruction empty. Use
    /// `put_value_list` to put the value list back.
    pub fn take_value_list(&mut self) -> Option<ValueList> {
        match *self {
            Self::Branch { ref mut args, .. }
            | Self::Call { ref mut args, .. }
            | Self::Jump { ref mut args, .. } => Some(args.take()),
            _ => None,
        }
    }

    /// Put back a value list.
    ///
    /// After removing a value list with `take_value_list()`, use this
    /// method to put it back. It is required that this instruction has
    /// a format that accepts a value list, and that the existing value
    /// list is empty. This avoids leaking list pool memory.
    pub fn put_value_list(&mut self, vlist: ValueList) {
        let args = match *self {
            Self::Branch { ref mut args, .. }
            | Self::Call { ref mut args, .. }
            | Self::Jump { ref mut args, .. } => args,
            _ => panic!("No value list: {:?}", self),
        };
        debug_assert!(args.is_empty(), "Value list already in use");
        *args = vlist;
    }

    pub fn opcode(&self) -> Opcode {
        match self {
            InstructionData::Unary { op, .. }
            | InstructionData::Binary { op, .. }
            | InstructionData::Branch { op, .. } => *op,
            InstructionData::Call { .. } => Opcode::Call,
            InstructionData::Jump { .. } => Opcode::Jmp,
            InstructionData::UnaryIeee64 { .. } => Opcode::Fconst,
            InstructionData::UnaryStr { .. } => Opcode::Sconst,
            InstructionData::UnaryInt { .. } => Opcode::Iconst,
            InstructionData::UnaryBool { .. } => Opcode::Bconst,
        }
    }

    /// Return information about a call instruction.
    ///
    /// Any instruction that can call another function reveals its call signature here.
    pub fn analyze_call<'a>(&'a self, pool: &'a ValueListPool) -> Option<(FuncRef, &[Value])> {
        match *self {
            Self::Call { func_ref, ref args, .. } => Some((func_ref, args.as_slice(pool))),
            _ => {
                debug_assert!(!self.opcode().is_call());
                None
            }
        }
    }

    /// Return information about the destination of a branch or jump instruction.
    ///
    /// Any instruction that can transfer control to another block reveals its possible destinations
    /// here.
    pub fn analyze_branch<'a>(&'a self, pool: &'a ValueListPool) -> Option<(Block, &[Value])> {
        match *self {
            Self::Jump { destination, ref args, .. } => Some((destination, args.as_slice(pool))),
            Self::Branch { destination, ref args, .. } => {
                Some((destination, &args.as_slice(pool)[1..]))
            }

            _ => {
                debug_assert!(!self.opcode().is_branch());
                None
            }
        }
    }

    pub fn eq(&self, other: &Self, pool: &ValueListPool) -> bool {
        match (self, other) {
            (Self::Unary { op: l_op, arg: l_arg }, Self::Unary { op: r_op, arg: r_arg }) => {
                l_op == r_op && l_arg == r_arg
            }
            (Self::Binary { op: l_op, args: l_args }, Self::Binary { op: r_op, args: r_args }) => {
                l_op == r_op && l_args == r_args
            }
            (
                Self::Branch {
                    op: l_op,
                    args: l_args,
                    destination: l_destination,
                    loop_tag: l_loop_tag,
                },
                Self::Branch {
                    op: r_op,
                    args: r_args,
                    destination: r_destination,
                    loop_tag: r_loop_tag,
                },
            ) => {
                l_op == r_op
                    && l_args.as_slice(pool) == r_args.as_slice(pool)
                    && l_destination == r_destination
                    && l_loop_tag == r_loop_tag
            }
            (
                Self::Jump { args: l_args, destination: l_destination },
                Self::Jump { args: r_args, destination: r_destination },
            ) => l_args.as_slice(pool) == r_args.as_slice(pool) && l_destination == r_destination,
            (
                Self::Call { func_ref: l_func_ref, args: l_args },
                Self::Call { func_ref: r_func_ref, args: r_args },
            ) => l_func_ref == r_func_ref && l_args.as_slice(pool) == r_args.as_slice(pool),
            (Self::UnaryIeee64 { imm: l_imm }, Self::UnaryIeee64 { imm: r_imm }) => l_imm == r_imm,
            (Self::UnaryStr { imm: l_imm }, Self::UnaryStr { imm: r_imm }) => l_imm == r_imm,
            (Self::UnaryInt { imm: l_imm }, Self::UnaryInt { imm: r_imm }) => l_imm == r_imm,
            (Self::UnaryBool { imm: l_imm }, Self::UnaryBool { imm: r_imm }) => l_imm == r_imm,
            _ => false,
        }
    }

    pub fn hash<H: std::hash::Hasher>(&self, state: &mut H, pool: &ValueListPool) {
        core::mem::discriminant(self).hash(state);
        match self {
            InstructionData::Unary { op, arg } => {
                op.hash(state);
                arg.hash(state);
            }
            InstructionData::Binary { op, args } => {
                op.hash(state);
                args.hash(state);
            }
            InstructionData::Branch { op, args, destination, loop_tag } => {
                op.hash(state);
                args.as_slice(pool).hash(state);
                destination.hash(state);
                loop_tag.hash(state);
            }
            InstructionData::Jump { args, destination } => {
                args.as_slice(pool).hash(state);
                destination.hash(state);
            }
            InstructionData::Call { func_ref, args } => {
                func_ref.hash(state);
                args.as_slice(pool).hash(state);
            }
            InstructionData::UnaryIeee64 { imm } => imm.hash(state),
            InstructionData::UnaryStr { imm } => imm.hash(state),
            InstructionData::UnaryInt { imm } => imm.hash(state),
            InstructionData::UnaryBool { imm } => imm.hash(state),
        }
    }
}

impl Opcode {
    #[inline]
    pub fn is_branch(self) -> bool {
        matches!(self, Opcode::Jmp | Opcode::Brz | Opcode::Brnz)
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
    pub const fn instruction_format(self) -> InstructionFormat {
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
