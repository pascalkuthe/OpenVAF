use openvaf_data_structures::BitSet;
use openvaf_hir::VariableId;
use openvaf_middle::cfg::ControlFlowGraph;
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::{
    CallArg, CallType, Derivative, InputKind, Local, Mir, ParameterInput, PortId, Type, Unknown,
};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(PartialEq, Eq, Clone)]
pub enum InstanceInitFunctionCallType {}

impl CallType for InstanceInitFunctionCallType {
    type I = ParameterInput;

    fn const_fold(&self, _: &[DiamondLattice]) -> DiamondLattice {
        match *self {}
    }

    fn derivative<C: CallType>(
        &self,
        _original: Local,
        _mir: &Mir<C>,
        _arg_derivative: impl FnMut(CallArg) -> Derivative<Self::I>,
    ) -> Derivative<Self::I> {
        match *self {}
    }
}

impl Display for InstanceInitFunctionCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl Debug for InstanceInitFunctionCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum InstanceInitInput {
    Parameter(ParameterInput),
    PortConnected(PortId),
}

impl InputKind for InstanceInitInput {
    fn derivative<C: CallType>(&self, unknown: Unknown, mir: &Mir<C>) -> Derivative<Self> {
        if let Self::Parameter(input) = self {
            if matches!((unknown, input), (Unknown::Parameter(x), ParameterInput::Value(y)) if &x == y)
            {
                Derivative::One
            } else {
                Derivative::Zero
            }
        } else {
            Derivative::Zero
        }
    }

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type {
        if let Self::Parameter(input) = self {
            input.ty(mir)
        } else {
            Type::BOOL
        }
    }
}

impl Display for InstanceInitInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param_input) => Display::fmt(param_input, f),
            Self::PortConnected(port) => write!(f, "$port_connected({:?})", port),
        }
    }
}

pub struct InstanceInit {
    pub cfg: ControlFlowGraph<InstanceInitFunctionCallType>,
    pub output_vars: BitSet<VariableId>,
}
