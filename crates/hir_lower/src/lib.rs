use std::iter::FilterMap;
use std::vec;

use hir_def::{BranchId, FunctionId, LocalFunctionArgId, NodeId, ParamId, ParamSysFun, VarId};
use indexmap::IndexMap;
use mir::{DataFlowGraph, FuncRef, FunctionSignature, Param, Value, F_ONE};
use stdx::{impl_debug, impl_idx_from};
use typed_indexmap::{map, TiMap, TiSet};

pub use crate::body::MirBuilder;

mod body;

#[cfg(test)]
mod tests;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CurrentKind {
    ExplicitBranch(BranchId),
    ImplictBranch { hi: NodeId, lo: Option<NodeId> },
    Port(NodeId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ParamKind {
    Param(ParamId),
    Voltage { hi: NodeId, lo: Option<NodeId> },
    Current(CurrentKind),
    Temperature,
    ParamGiven { param: ParamId },
    PortConnected { port: NodeId },
    ParamSysFun(ParamSysFun),
    HiddenState(VarId),
}

impl ParamKind {
    fn unwrap_pot_node(&self) -> NodeId {
        match self {
            ParamKind::Voltage { hi, lo: None } => *hi,
            _ => unreachable!("called unwrap_pot_node on {:?}", self),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PlaceKind {
    Var(VarId),
    FunctionReturn {
        fun: FunctionId,
    },
    FunctionArg {
        fun: FunctionId,
        arg: LocalFunctionArgId,
    },
    BranchVoltage(BranchId),
    BranchCurrent(BranchId),
    ImplicitBranchVoltage {
        hi: NodeId,
        lo: Option<NodeId>,
    },
    ImplicitBranchCurrent {
        hi: NodeId,
        lo: Option<NodeId>,
    },
    /// A parameter during param initiliztion is mutable (write default in case its not given)
    Param(ParamId),
    ParamMin(ParamId),
    ParamMax(ParamId),
}

impl From<VarId> for PlaceKind {
    fn from(var: VarId) -> PlaceKind {
        PlaceKind::Var(var)
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum ParamInfoKind {
    Invalid,
    MinInclusive,
    MaxInclusive,
    MinExclusive,
    MaxExclusive,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Copy)]
pub enum CallBackKind {
    SimParam,
    SimParamOpt,
    SimParamStr,
    Derivative(Param),
    NodeDerivative(NodeId),
    ParamInfo(ParamInfoKind, ParamId),
    CollapseHint(NodeId, Option<NodeId>),
}

impl CallBackKind {
    pub fn signature(&self) -> FunctionSignature {
        match self {
            CallBackKind::SimParam => FunctionSignature {
                name: "simparam".to_owned(),
                params: 1,
                returns: 1,
                has_sideeffects: false,
            },
            CallBackKind::SimParamOpt => FunctionSignature {
                name: "simparam_opt".to_owned(),
                params: 1,
                returns: 1,
                has_sideeffects: false,
            },
            CallBackKind::SimParamStr => FunctionSignature {
                name: "simparam_str".to_owned(),
                params: 1,
                returns: 1,
                has_sideeffects: false,
            },
            CallBackKind::Derivative(param) => FunctionSignature {
                name: format!("ddx_{}", param),
                params: 1,
                returns: 1,
                has_sideeffects: false,
            },
            CallBackKind::NodeDerivative(node) => FunctionSignature {
                name: format!("ddx_node_{:?}", node),
                params: 1,
                returns: 1,
                has_sideeffects: false,
            },
            CallBackKind::ParamInfo(kind, param) => FunctionSignature {
                name: format!("set_{:?}({:?})", kind, param),
                params: 0,
                returns: 0,
                has_sideeffects: true,
            },
            CallBackKind::CollapseHint(hi, lo) => FunctionSignature {
                name: format!("collapse_{:?}_{:?})", hi, lo),
                params: 0,
                returns: 0,
                has_sideeffects: true,
            },
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct CfgNodeId(u32);
impl_idx_from!(CfgNodeId(u32));
impl_debug!(match CfgNodeId{node => "node{}",node.0;});

pub struct FunctionMetadata {
    pub outputs: IndexMap<PlaceKind, Value>,
    pub params: TiMap<Param, ParamKind, Value>,
    pub callbacks: TiSet<FuncRef, CallBackKind>,
}

/// A mapping between abstractions used in the MIR and the corresponding
/// information from the HIR. This allows the MIR to remain independent of the frontend/HIR
#[derive(Debug, PartialEq, Default, Clone)]
pub struct HirInterner {
    pub tagged_reads: IndexMap<Value, VarId, ahash::RandomState>,
    pub outputs: IndexMap<PlaceKind, Value, ahash::RandomState>,
    pub params: TiMap<Param, ParamKind, Value>,
    pub callbacks: TiSet<FuncRef, CallBackKind>,
}

pub type LiveParams<'a> = FilterMap<
    map::Iter<'a, Param, ParamKind, Value>,
    fn((Param, (&'a ParamKind, &'a Value))) -> Option<(Param, &'a ParamKind, Value)>,
>;

impl HirInterner {
    pub fn unkowns(&self) -> impl Iterator<Item = (FuncRef, Box<[(Value, Value)]>)> + '_ {
        self.callbacks.iter_enumerated().filter_map(|(cb, kind)| match kind {
            CallBackKind::Derivative(param) => {
                Some((cb, vec![(self.params[*param], F_ONE)].into_boxed_slice()))
            }
            CallBackKind::NodeDerivative(node) => Some((
                cb,
                self.params
                    .raw
                    .iter()
                    .filter_map(|(kind, value)| match kind {
                        ParamKind::Voltage { hi, .. } if hi == node => Some((*value, F_ONE)),
                        ParamKind::Voltage { lo: Some(lo), .. } if lo == node => {
                            Some((*value, F_ONE))
                        }
                        _ => None,
                    })
                    .collect(),
            )),
            _ => None,
        })
    }

    pub fn live_params<'a>(
        &'a self,
        dfg: &'a DataFlowGraph,
    ) -> FilterMap<
        map::Iter<'a, Param, ParamKind, Value>,
        impl FnMut((Param, (&'a ParamKind, &'a Value))) -> Option<(Param, &'a ParamKind, Value)> + Clone,
    > {
        self.params.iter_enumerated().filter_map(|(param, (kind, val))| {
            if dfg.value_dead(*val) {
                None
            } else {
                Some((param, kind, *val))
            }
        })
    }

    // pub fn map(
    //     &mut self,
    //     place_map: &TiSet<Place, Place>,
    //     param_map: &TiSet<CfgParam, CfgParam>,
    //     callback_map: &TiSet<Callback, Callback>,
    // ) {
    //     self.places = place_map.raw.iter().map(|place| self.places[*place].clone()).collect();
    //     self.params = param_map.raw.iter().map(|param| self.params[*param].clone()).collect();
    //     self.callbacks =
    //         callback_map.raw.iter().map(|callbacks| self.callbacks[*callbacks].clone()).collect();
    // }
}
