use std::iter::FilterMap;

use ahash::AHashMap;
use bitset::HybridBitSet;
use hir_def::{BranchId, FunctionId, LocalFunctionArgId, NodeId, ParamId, ParamSysFun, VarId};
use indexmap::IndexMap;
use mir::{
    DataFlowGraph, DerivativeInfo, FuncRef, Function, FunctionSignature, Param, Unkown, Value,
};
use stdx::packed_option::PackedOption;
// use stdx::{impl_debug, impl_idx_from};
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
    BranchVoltage {
        branch: BranchId,
        reactive: bool,
    },
    BranchCurrent {
        branch: BranchId,
        reactive: bool,
    },
    ImplicitBranchVoltage {
        hi: NodeId,
        lo: Option<NodeId>,
        reactive: bool,
    },
    ImplicitBranchCurrent {
        hi: NodeId,
        lo: Option<NodeId>,
        reactive: bool,
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
    // StoreState(StateId),
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
                params: 2,
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
            // CallBackKind::StoreState(state) => FunctionSignature {
            //     name: format!("store_{:?})", state),
            //     params: 1,
            //     returns: 0,
            //     has_sideeffects: true,
            // },
        }
    }
}

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
    pub outputs: IndexMap<PlaceKind, PackedOption<Value>, ahash::RandomState>,
    pub params: TiMap<Param, ParamKind, Value>,
    pub callbacks: TiSet<FuncRef, CallBackKind>,
    // pub state: TiSet<StateId, FuncRef>,
}

pub type LiveParams<'a> = FilterMap<
    map::Iter<'a, Param, ParamKind, Value>,
    fn((Param, (&'a ParamKind, &'a Value))) -> Option<(Param, &'a ParamKind, Value)>,
>;

impl HirInterner {
    fn contains_ddx(
        ddx_calls: &mut AHashMap<FuncRef, (HybridBitSet<Unkown>, HybridBitSet<Unkown>)>,
        func: &Function,
        callbacks: &TiSet<FuncRef, CallBackKind>,
        ddx: &CallBackKind,
        val: Unkown,
        neg: bool,
    ) -> bool {
        if let Some(ddx) = callbacks.index(ddx) {
            let (pos_dst, neg_dst) = ddx_calls.entry(ddx).or_default();
            if neg {
                neg_dst.insert(val, func.dfg.num_values());
            } else {
                pos_dst.insert(val, func.dfg.num_values());
            }
            true
        } else {
            false
        }
    }

    pub fn unkowns<'a>(
        &'a mut self,
        func: &'a mut Function,
        sim_derivatives: bool,
    ) -> DerivativeInfo {
        let mut unknowns = TiSet::default();
        let mut ddx_calls = AHashMap::new();
        // let mut nodes: AHashMap<NodeId, HybridBitSet<Value>> = AHashMap::new();
        // let mut required_nodes: IndexSet<NodeId, RandomState> = IndexSet::default();
        for (param, (kind, &val)) in self.params.iter_enumerated() {
            if func.dfg.value_dead(val) {
                continue;
            }

            let param_required = Self::contains_ddx(
                &mut ddx_calls,
                func,
                &self.callbacks,
                &CallBackKind::Derivative(param),
                unknowns.len().into(),
                false,
            );

            let mut node_required = |node, neg| {
                Self::contains_ddx(
                    &mut ddx_calls,
                    func,
                    &self.callbacks,
                    &CallBackKind::NodeDerivative(node),
                    unknowns.len().into(),
                    neg,
                )
            };

            let required = match *kind {
                ParamKind::Voltage { hi, lo: Some(lo) } => {
                    sim_derivatives | node_required(hi, false) | node_required(lo, true)
                }
                ParamKind::Voltage { hi, lo: None } => sim_derivatives | node_required(hi, false),
                ParamKind::Current(_) => sim_derivatives,
                _ => param_required,
            };

            if required {
                unknowns.insert(val);
            }
        }

        //         let mut i = 0;
        //         while let Some(&node) = required_nodes.get_index(i) {
        //             for val in nodes[&node].iter() {
        //                 let (hi, lo) = unknowns[&val].unwrap();
        //                 let next = if hi == node { lo } else { hi };
        //                 required_nodes.insert(next);
        //             }

        //             i += 1;
        //         }

        //         let mut i = 0;
        //         let mut res = TiMap::with_capacity(unknowns.len());
        //         let mut nodes = HybridBitSet::new_empty();

        //         while let Some((&val, &diff)) = unknowns.get_index(i) {
        //             let mut actual_diff = None;
        //             if let Some((hi, lo)) = diff {
        //                 if required_nodes.contains(&hi) {
        //                     let hi_ = ParamKind::Voltage { hi, lo: None };
        //                     let lo_ = ParamKind::Voltage { hi: lo, lo: None };

        //                     let (hi_, changed) = Self::ensure_param_(&mut self.params, func, hi_);
        //                     if changed {
        //                         unknowns.insert(hi_, None);
        //                         Self::contains_ddx(
        //                             &mut ddx_calls,
        //                             func,
        //                             &self.callbacks,
        //                             &CallBackKind::NodeDerivative(hi),
        //                             hi_,
        //                         );
        //                     }
        //                     let (lo_, changed) = Self::ensure_param_(&mut self.params, func, lo_);
        //                     if changed {
        //                         unknowns.insert(lo_, None);
        //                         Self::contains_ddx(
        //                             &mut ddx_calls,
        //                             func,
        //                             &self.callbacks,
        //                             &CallBackKind::NodeDerivative(lo),
        //                             lo_,
        //                         );
        //                     }

        //                     let lo_ = unknowns.get_index_of(&lo_).unwrap().into();
        //                     let hi_ = unknowns.get_index_of(&hi_).unwrap().into();

        //                     nodes.insert_growable(hi_, unknowns.len());
        //                     nodes.insert_growable(lo_, unknowns.len());

        //                     actual_diff = Some((hi_, lo_));
        //                 }
        //             }

        //             i += 1;
        //             res.insert(val, actual_diff);
        //         }

        DerivativeInfo { unkowns: unknowns, ddx_calls }
    }

    pub fn is_param_live(&self, func: &Function, kind: &ParamKind) -> bool {
        if let Some(val) = self.params.raw.get(kind) {
            !func.dfg.value_dead(*val)
        } else {
            false
        }
    }

    pub fn is_param_live_(
        params: &TiMap<Param, ParamKind, Value>,
        func: &Function,
        kind: &ParamKind,
    ) -> bool {
        if let Some(val) = params.raw.get(kind) {
            !func.dfg.value_dead(*val)
        } else {
            false
        }
    }

    pub fn ensure_param(&mut self, func: &mut Function, kind: ParamKind) -> Value {
        Self::ensure_param_(&mut self.params, func, kind).0
    }

    pub(crate) fn ensure_param_(
        params: &mut TiMap<Param, ParamKind, Value>,
        func: &mut Function,
        kind: ParamKind,
    ) -> (Value, bool) {
        let len = params.len();
        let entry = params.raw.entry(kind);
        let changed = matches!(entry, indexmap::map::Entry::Vacant(_));
        let val = *entry.or_insert_with(|| func.dfg.make_param(len.into()));
        (val, changed)
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

// #[derive(PartialEq, Eq, Clone, Copy, Hash)]
// pub struct StateId(u32);
// impl_idx_from!(StateId(u32));
// impl_debug!(match StateId{id => "state{}",id.0;});
