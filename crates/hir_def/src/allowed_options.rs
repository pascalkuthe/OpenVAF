use std::{
    intrinsics::transmute,
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign},
};

use derive_more::Display;

/// Some expressions are only allowed in certain a certain context.
/// OpenVAF use bitflags to keep track of what is currently allowed during hir validation
#[repr(u16)]
#[derive(Display, Debug, Copy, Clone, PartialEq, Eq)]
pub enum Allowed {
    #[display(fmt = "referencing nets")]
    NetReferences = 0b0000_0000_0000_0001,
    #[display(fmt = "referencing ports")]
    PortReferences = 0b0000_0000_0000_0010,
    #[display(fmt = "referencing parameter")]
    ParameterReferences = 0b0000_0000_0000_0100,
    #[display(fmt = "referencing variables")]
    VariableReferences = 0b0000_0000_0000_1000,
    #[display(fmt = "calling analog filter functions")]
    AnalogFilters = 0b0000_0000_0001_0000,
    #[display(fmt = "accessing branches")]
    BranchAccess = 0b0000_0000_0010_0000,
    #[display(fmt = "calling system functions")]
    SystemFunctionCalls = 0b0000_0000_0100_0000,
    #[display(fmt = "calling the $temperature system function")]
    Temperature = 0b0000_0000_1000_0000,
    #[display(fmt = "calling VerilogA functions")]
    UserFunctionCalls = 0b0000_0001_0000_0000,
    #[display(fmt = "a user defined function")]
    UserFunctionReference = 0b0000_0010_0000_0000,
    #[display(fmt = "contributing to branches")]
    Contribute = 0b0000_0100_0000_0000,
    #[display(fmt = "declaring named blocks")]
    NamedBlocks = 0b0000_1000_0000_0000,
}

impl Allowed {
    const MAX: u16 = Allowed::NamedBlocks as u16;
    fn from_raw(raw: u16) -> Self {
        assert_eq!(raw.count_ones(), 1);
        assert!(raw <= Self::MAX);
        unsafe { transmute(raw) }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AllowedOps(u16);
impl AllowedOps {
    pub const fn empty() -> AllowedOps {
        AllowedOps(0)
    }

    pub const fn new(allowed_ops: &[Allowed]) -> AllowedOps {
        let mut res = 0;
        let mut i = 0;
        while i < allowed_ops.len() {
            res |= allowed_ops[i] as u16;
            i += 1;
        }
        AllowedOps(res)
    }

    pub const fn contains(&self, op: Allowed) -> bool {
        (self.0 & op as u16) != 0
    }

    pub const fn union(self, other: AllowedOps) -> AllowedOps {
        AllowedOps(self.0 | other.0)
    }

    pub const fn insert(self, allowed: Allowed) -> AllowedOps {
        AllowedOps(self.0 | allowed as u16)
    }

    pub const fn remove(self, not_allowed: Allowed) -> AllowedOps {
        AllowedOps(self.0 & !(not_allowed as u16))
    }

    pub const fn intersection(self, other: AllowedOps) -> AllowedOps {
        AllowedOps(self.0 & other.0)
    }

    pub fn clear(&mut self) {
        self.0 = 0
    }
}

impl BitOr for AllowedOps {
    type Output = AllowedOps;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.union(rhs)
    }
}

impl BitOrAssign for AllowedOps {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs
    }
}

impl BitOrAssign<Allowed> for AllowedOps {
    fn bitor_assign(&mut self, rhs: Allowed) {
        *self = *self | rhs
    }
}

impl BitOr<Allowed> for AllowedOps {
    type Output = AllowedOps;

    fn bitor(self, rhs: Allowed) -> Self::Output {
        self.insert(rhs)
    }
}

impl BitAnd for AllowedOps {
    type Output = AllowedOps;

    fn bitand(self, rhs: Self) -> Self::Output {
        self.intersection(rhs)
    }
}

impl BitAndAssign for AllowedOps {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self | rhs
    }
}

use Allowed::*;

pub const ALLOWED_OPS_CONST_EXPRESSION: AllowedOps = AllowedOps::new(&[ParameterReferences]);

pub const ALLOWED_OPS_ANALOG_FUNCTION_BEHAVIOUR: AllowedOps = AllowedOps::new(&[
    ParameterReferences,
    VariableReferences,
    SystemFunctionCalls,
    UserFunctionCalls,
]);

pub const ALLOWED_OPS_CONDITIONAL_ANALOG_BEHAVIOUR: AllowedOps =
    ALLOWED_OPS_ANALOG_FUNCTION_BEHAVIOUR.union(AllowedOps::new(&[
        BranchAccess,
        Contribute,
        NamedBlocks,
    ]));

pub const ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR: AllowedOps =
    ALLOWED_OPS_CONDITIONAL_ANALOG_BEHAVIOUR.union(AllowedOps::new(&[AnalogFilters]));
