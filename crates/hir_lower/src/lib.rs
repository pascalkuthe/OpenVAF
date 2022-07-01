use std::iter::FilterMap;

use ahash::AHashMap;
use bitset::HybridBitSet;
use hir_def::{
    BranchId, FunctionId, LocalFunctionArgId, NodeId, ParamId, ParamSysFun, Type, VarId,
};
use hir_ty::inference::BranchWrite;
use indexmap::IndexMap;
use lasso::Spur;
use mir::{
    DataFlowGraph, DerivativeInfo, FuncRef, Function, FunctionSignature, Param, Unkown, Value,
};
use stdx::packed_option::PackedOption;
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiVec;
use typed_indexmap::{map, TiMap, TiSet};

pub use crate::body::MirBuilder;

mod body;

#[cfg(test)]
mod tests;

// pub enum BranchKind{
//     Explicit(BranchId),
//     Implicit(BranchId),
// }

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ImplicitEquationKind {
    Ddt,
    DdtContrib(Value),
    Idt(IdtKind),
    NoiseSrc,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CurrentKind {
    Branch(BranchId),
    Unnamed { hi: NodeId, lo: Option<NodeId> },
    Port(NodeId),
}

impl From<BranchWrite> for CurrentKind {
    fn from(kind: BranchWrite) -> Self {
        match kind {
            BranchWrite::Named(branch) => CurrentKind::Branch(branch),
            BranchWrite::Unnamed { hi, lo } => CurrentKind::Unnamed { hi, lo },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ParamKind {
    Param(ParamId),
    Abstime,
    EnableIntegration,
    // PrevVal(Value),
    // LimitChange(Value),
    Voltage { hi: NodeId, lo: Option<NodeId> },
    Current(CurrentKind),
    Temperature,
    ParamGiven { param: ParamId },
    PortConnected { port: NodeId },
    ParamSysFun(ParamSysFun),
    HiddenState(VarId),
    ImplicitUnkown(ImplicitEquation),
}

impl ParamKind {
    fn unwrap_pot_node(&self) -> NodeId {
        match self {
            ParamKind::Voltage { hi, lo: None } => *hi,
            _ => unreachable!("called unwrap_pot_node on {:?}", self),
        }
    }

    pub fn op_dependent(&self) -> bool {
        matches!(
            self,
            ParamKind::Voltage { .. }
                | ParamKind::Current(_)
                | ParamKind::ImplicitUnkown(_)
                | ParamKind::Abstime
                | ParamKind::EnableIntegration
                | ParamKind::HiddenState(_)
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IdtKind {
    Basic,
    Ic,
    Assert,
    Modulus,
    ModulusOffset,
}

impl IdtKind {
    pub const fn num_params(self) -> u16 {
        match self {
            IdtKind::Basic => 1,
            IdtKind::Ic => 2,
            IdtKind::Assert | IdtKind::Modulus => 3,
            IdtKind::ModulusOffset => 4,
        }
    }
    pub const fn has_ic(self) -> bool {
        !matches!(self, IdtKind::Basic)
    }

    pub const fn has_assert(self) -> bool {
        matches!(self, IdtKind::Assert)
    }

    pub const fn has_modulus(self) -> bool {
        matches!(self, IdtKind::Modulus | IdtKind::ModulusOffset)
    }

    pub const fn has_offset(self) -> bool {
        matches!(self, IdtKind::ModulusOffset)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PlaceKind {
    Var(VarId),
    FunctionReturn(FunctionId),
    FunctionArg {
        fun: FunctionId,
        arg: LocalFunctionArgId,
    },
    Contribute {
        dst: BranchWrite,
        dim: Dim,
        voltage_src: bool,
    },
    ImplicitResidual {
        equation: ImplicitEquation,
        dim: Dim,
    },
    CollapseImplicitEquation(ImplicitEquation),
    IsVoltageSrc(BranchWrite),
    /// A parameter during param initiliztion is mutable (write default in case its not given)
    Param(ParamId),
    ParamMin(ParamId),
    ParamMax(ParamId),
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
pub enum DisplayKind {
    Debug,
    Display,
    Info,
    Warn,
    Error,
    Fatal,
    Monitor,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum CallBackKind {
    BoundStep,
    Print { kind: DisplayKind, arg_tys: Box<[Type]> },
    SimParam,
    SimParamOpt,
    SimParamStr,
    Derivative(Param),
    NodeDerivative(NodeId),
    ParamInfo(ParamInfoKind, ParamId),
    CollapseHint(NodeId, Option<NodeId>),
    // Limit { name: Spur, num_args: u32 },
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
                name: format!("collapse_{:?}_{:?}", hi, lo),
                params: 0,
                returns: 0,
                has_sideeffects: true,
            },
            CallBackKind::Print { kind, arg_tys: args } => FunctionSignature {
                name: format!("{:?})", kind),
                params: args.len() as u16 + 1,
                returns: 0,
                has_sideeffects: true,
            },
            CallBackKind::BoundStep => FunctionSignature {
                name: "$bound_step".to_owned(),
                params: 1,
                returns: 0,
                has_sideeffects: true,
            },
            // CallBackKind::Limit { name, num_args } => FunctionSignature {
            //     name: format!("$limit[{name:?}]"),
            //     params: *num_args as u16,
            //     returns: 1,
            //     has_sideeffects: false,
            // },
            // CallBackKind::ExplicitDdt(_) => FunctionSignature {
            //     name: "ddt!".to_owned(),
            //     params: 1,
            //     returns: 1,
            //     has_sideeffects: false,
            // },
            // CallBackKind::StoreState(state) => FunctionSignature {
            //     name: format!("store_{:?})", state),
            //     params: 1,
            //     returns: 0,
            //     has_sideeffects: true,
            // },
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim(u32);
impl_idx_from!(Dim(u32));
impl_debug_display! {
    match Dim {Dim(i) => "dim{}", i;}
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ImplicitEquation(u32);
impl_idx_from!(ImplicitEquation(u32));
impl_debug_display! {
    match ImplicitEquation {ImplicitEquation(i) => "inode{}", i;}
}

pub const RESISTIVE_DIM: Dim = Dim(0u32);
pub const REACTIVE_DIM: Dim = Dim(1u32);

#[derive(Debug, PartialEq, Clone)]
pub enum DimKind {
    Resistive,
    Reactive,
    WhiteNoise(Spur),
    FlickrNoise(Spur),
    TableNoise(Spur),
}

/// A mapping between abstractions used in the MIR and the corresponding
/// information from the HIR. This allows the MIR to remain independent of the frontend/HIR
#[derive(Debug, PartialEq, Default, Clone)]
pub struct HirInterner {
    pub tagged_reads: IndexMap<Value, VarId, ahash::RandomState>,
    pub outputs: IndexMap<PlaceKind, PackedOption<Value>, ahash::RandomState>,
    pub params: TiMap<Param, ParamKind, Value>,
    pub callbacks: TiSet<FuncRef, CallBackKind>,
    pub dims: TiVec<Dim, DimKind>,
    pub implicit_equations: TiVec<ImplicitEquation, ImplicitEquationKind>,
    // pub lim_equations: IndexMap<Value, Vec<Value>, ahash::RandomState>,
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
                    sim_derivatives || node_required(hi, false) | node_required(lo, true)
                }
                ParamKind::Voltage { hi, lo: None } => sim_derivatives || node_required(hi, false),
                ParamKind::Current(_) | ParamKind::ImplicitUnkown(_) => sim_derivatives,
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

    pub fn ensure_param_(
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
