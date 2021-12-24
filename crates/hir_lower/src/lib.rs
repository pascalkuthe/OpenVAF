use std::vec;

use cfg::{Callback, CfgParam, Place};
use hir_def::{BranchId, FunctionId, NodeId, ParamId, ParamSysFun, VarId};
use lasso::Rodeo;
use stdx::{impl_debug, impl_idx_from};
use typed_indexmap::TiSet;

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
}

impl ParamKind {
    fn unwrap_pot_node(&self) -> NodeId {
        match self {
            ParamKind::Voltage { hi, lo: None } => *hi,
            _ => unreachable!("called unwrap_pot_node on {:?}", self),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PlaceKind {
    Var(VarId),
    FunctionReturn {
        fun: FunctionId,
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
    Derivative {
        original: Place,
        unkown: u32,
    },
    /// A parameter during param initiliztion is mutable (write default in case its not given)
    Param(ParamId),
}

impl From<VarId> for PlaceKind {
    fn from(var: VarId) -> PlaceKind {
        PlaceKind::Var(var)
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum CallBackKind {
    SimParam,
    SimParamOpt,
    SimParamStr,
    Derivative(CfgParam),
    NodeDerivative(NodeId),
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct CfgNodeId(u32);
impl_idx_from!(CfgNodeId(u32));
impl_debug!(match CfgNodeId{node => "node{}",node.0;});

/// A mapping between abstractions used in the control flow graph and the corresponding
/// information from the HIR. This allows the CFG to remain independent of the HirInterner
#[derive(Debug, PartialEq, Default)]
pub struct HirInterner {
    pub params: TiSet<CfgParam, ParamKind>,
    pub places: TiSet<Place, PlaceKind>,
    pub callbacks: TiSet<Callback, CallBackKind>,
    pub literals: Rodeo,
}

impl HirInterner {
    pub fn unkowns(&self) -> impl Iterator<Item = (Callback, Box<[(CfgParam, u64)]>)> + '_ {
        self.callbacks.iter_enumerated().filter_map(|(cb, kind)| match kind {
            CallBackKind::Derivative(param) => {
                Some((cb, vec![(*param, 1f64.to_bits())].into_boxed_slice()))
            }
            CallBackKind::NodeDerivative(node) => Some((
                cb,
                self.params
                    .iter_enumerated()
                    .filter_map(|(param, kind)| match kind {
                        ParamKind::Voltage { hi, .. } if hi == node => {
                            Some((param, 1f64.to_bits()))
                        }
                        ParamKind::Voltage { lo: Some(lo), .. } if lo == node => {
                            Some((param, 1f64.to_bits()))
                        }
                        _ => None,
                    })
                    .collect(),
            )),
            _ => None,
        })
    }

    pub fn map(
        &mut self,
        place_map: &TiSet<Place, Place>,
        param_map: &TiSet<CfgParam, CfgParam>,
        callback_map: &TiSet<Callback, Callback>,
    ) {
        self.places = place_map.raw.iter().map(|place| self.places[*place].clone()).collect();
        self.params = param_map.raw.iter().map(|param| self.params[*param].clone()).collect();
        self.callbacks =
            callback_map.raw.iter().map(|callbacks| self.callbacks[*callbacks].clone()).collect();
    }
}
