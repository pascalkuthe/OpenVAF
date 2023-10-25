use std::iter::FilterMap;

use ahash::{AHashMap, AHashSet};
use bitset::HybridBitSet;
use hir::{
    Branch, BranchWrite, CompilationDB, Module, Node, ParamSysFun, Parameter, Type, Variable,
};
use indexmap::IndexMap;
use lasso::Rodeo;
use mir::builder::InstBuilder;
use mir::{DataFlowGraph, FuncRef, Function, Inst, KnownDerivatives, Param, Unknown, Value};
use mir_build::{FunctionBuilder, FunctionBuilderContext, RetBuilder};
use stdx::packed_option::PackedOption;
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiVec;
use typed_indexmap::{map, TiMap, TiSet};

pub use callbacks::{CallBackKind, NoiseTable, ParamInfoKind};

use crate::body::BodyLoweringCtx;
use crate::ctx::LoweringCtx;

macro_rules! match_signature {
    ($signature:ident: $($case:ident $(| $extra_case:ident)* => $res:expr),*) => {
        match $signature {
            $($case $(|$extra_case)* => $res,)*
            signature => unreachable!("invalid signature {:?}",signature)
        }

    };
}

mod body;
mod callbacks;
mod ctx;
mod expr;
pub mod fmt;
mod parameters;
mod state;
mod stmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ImplicitEquationKind {
    Ddt,
    NoiseSrc,
    Idt(IdtKind),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CurrentKind {
    Branch(Branch),
    Unnamed { hi: Node, lo: Option<Node> },
    Port(Node),
}

impl From<BranchWrite> for CurrentKind {
    fn from(kind: BranchWrite) -> Self {
        match kind {
            BranchWrite::Named(branch) => CurrentKind::Branch(branch),
            BranchWrite::Unnamed { hi, lo } => CurrentKind::Unnamed { hi, lo },
        }
    }
}

impl TryFrom<CurrentKind> for BranchWrite {
    type Error = ();
    fn try_from(kind: CurrentKind) -> Result<BranchWrite, ()> {
        match kind {
            CurrentKind::Branch(branch) => Ok(BranchWrite::Named(branch)),
            CurrentKind::Unnamed { hi, lo } => Ok(BranchWrite::Unnamed { hi, lo }),
            CurrentKind::Port(_) => Err(()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ParamKind {
    Param(Parameter),
    Abstime,
    EnableIntegration,
    EnableLim,
    PrevState(LimitState),
    NewState(LimitState),
    Voltage { hi: Node, lo: Option<Node> },
    Current(CurrentKind),
    Temperature,
    ParamGiven { param: Parameter },
    PortConnected { port: Node },
    ParamSysFun(ParamSysFun),
    HiddenState(Variable),
    ImplicitUnknown(ImplicitEquation),
}

impl ParamKind {
    fn unwrap_pot_node(&self) -> Node {
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
                | ParamKind::ImplicitUnknown(_)
                | ParamKind::Abstime
                | ParamKind::EnableIntegration
                | ParamKind::HiddenState(_)
                | ParamKind::PrevState(_)
                | ParamKind::NewState(_)
                | ParamKind::EnableLim
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
    Var(Variable),
    FunctionReturn(hir::Function),
    FunctionArg(hir::FunctionArg),
    Contribute {
        dst: BranchWrite,
        reactive: bool,
        voltage_src: bool,
    },
    ImplicitResidual {
        equation: ImplicitEquation,
        reactive: bool,
    },
    CollapseImplicitEquation(ImplicitEquation),
    IsVoltageSrc(BranchWrite),
    /// A parameter during param initiliztion is mutable (write default in case its not given)
    Param(Parameter),
    ParamMin(Parameter),
    ParamMax(Parameter),
    BoundStep,
}

impl PlaceKind {
    pub fn ty(&self, db: &CompilationDB) -> Type {
        match *self {
            PlaceKind::Var(var) => var.ty(db),
            PlaceKind::FunctionReturn(fun) => fun.return_ty(db),
            PlaceKind::FunctionArg(arg) => arg.ty(db),

            PlaceKind::ImplicitResidual { .. } | PlaceKind::Contribute { .. } => Type::Real,
            PlaceKind::ParamMin(param) | PlaceKind::ParamMax(param) | PlaceKind::Param(param) => {
                param.ty(db)
            }
            PlaceKind::IsVoltageSrc(_) | PlaceKind::CollapseImplicitEquation(_) => Type::Bool,
            PlaceKind::BoundStep => Type::Real,
        }
    }

    pub fn is_init_only(&self) -> bool {
        matches!(self, Self::CollapseImplicitEquation(_))
    }
}

impl From<hir::AssignmentLhs> for PlaceKind {
    fn from(hir: hir::AssignmentLhs) -> Self {
        match hir {
            hir::AssignmentLhs::Variable(var) => PlaceKind::Var(var),
            hir::AssignmentLhs::FunctionReturn(fun) => PlaceKind::FunctionReturn(fun),
            hir::AssignmentLhs::FunctionArg(arg) => PlaceKind::FunctionArg(arg),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ImplicitEquation(u32);
impl_idx_from!(ImplicitEquation(u32));
impl_debug_display! {
    match ImplicitEquation {ImplicitEquation(i) => "inode{}", i;}
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LimitState(u32);
impl_idx_from!(LimitState(u32));
impl_debug_display! {
    match LimitState {LimitState(i) => "lim_state{}", i;}
}

/// A mapping between abstractions used in the MIR and the corresponding
/// information from the HIR. This allows the MIR to remain independent of the frontend/HIR
#[derive(Debug, PartialEq, Default, Clone)]
pub struct HirInterner {
    pub outputs: IndexMap<PlaceKind, PackedOption<Value>, ahash::RandomState>,
    pub params: TiMap<Param, ParamKind, Value>,
    pub callbacks: TiSet<FuncRef, CallBackKind>,
    pub callback_uses: TiVec<FuncRef, Vec<Inst>>,
    pub tagged_reads: IndexMap<Value, Variable, ahash::RandomState>,
    pub implicit_equations: TiVec<ImplicitEquation, ImplicitEquationKind>,
    pub lim_state: TiMap<LimitState, Value, Vec<(Value, bool)>>,
}

pub type LiveParams<'a> = FilterMap<
    map::Iter<'a, Param, ParamKind, Value>,
    fn((Param, (&'a ParamKind, &'a Value))) -> Option<(Param, &'a ParamKind, Value)>,
>;

impl HirInterner {
    fn contains_ddx(
        ddx_calls: &mut AHashMap<FuncRef, (HybridBitSet<Unknown>, HybridBitSet<Unknown>)>,
        func: &Function,
        callbacks: &TiSet<FuncRef, CallBackKind>,
        ddx: &CallBackKind,
        val: Unknown,
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

    pub fn unknowns(&self, func: impl AsRef<Function>, sim_derivatives: bool) -> KnownDerivatives {
        let func = func.as_ref();
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
                ParamKind::Current(_) | ParamKind::ImplicitUnknown(_) => sim_derivatives,
                _ => param_required,
            };

            if required {
                unknowns.insert(val);
            }
        }

        for (param, vals) in self.lim_state.iter() {
            for &(val, neg) in vals {
                let param = func.dfg.value_def(*param).unwrap_param();

                let mut required = Self::contains_ddx(
                    &mut ddx_calls,
                    func,
                    &self.callbacks,
                    &CallBackKind::Derivative(param),
                    unknowns.len().into(),
                    neg,
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

                match *self.params.get_index(param).unwrap().0 {
                    ParamKind::Voltage { hi, lo: None } => required |= node_required(hi, neg),
                    ParamKind::Voltage { hi, lo: Some(lo) } => {
                        required |= node_required(hi, false) | node_required(lo, !neg);
                    }
                    _ => (),
                };

                if required | sim_derivatives {
                    unknowns.insert(val);
                }
            }
        }

        KnownDerivatives { unknowns, ddx_calls }
    }

    pub fn is_param_live(&self, func: impl AsRef<Function>, kind: &ParamKind) -> bool {
        let func = func.as_ref();
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

    pub fn ensure_param(&mut self, func: impl AsMut<Function>, kind: ParamKind) -> Value {
        Self::ensure_param_(&mut self.params, func, kind)
    }

    pub fn ensure_param_(
        params: &mut TiMap<Param, ParamKind, Value>,
        mut func: impl AsMut<Function>,
        kind: ParamKind,
    ) -> Value {
        let len = params.len();
        let entry = params.raw.entry(kind);
        *entry.or_insert_with(|| func.as_mut().dfg.make_param(len.into()))
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
}

pub struct MirBuilder<'a> {
    db: &'a CompilationDB,
    module: Module,
    is_output: &'a dyn Fn(PlaceKind) -> bool,
    required_vars: &'a mut dyn Iterator<Item = Variable>,
    tagged_reads: AHashSet<Variable>,
    tag_writes: bool,
    ctx: Option<&'a mut FunctionBuilderContext>,
    lower_equations: bool,
}

impl<'a> MirBuilder<'a> {
    pub fn new(
        db: &'a CompilationDB,
        module: Module,
        is_output: &'a dyn Fn(PlaceKind) -> bool,
        required_vars: &'a mut dyn Iterator<Item = Variable>,
    ) -> MirBuilder<'a> {
        MirBuilder {
            db,
            module,
            tagged_reads: AHashSet::new(),
            is_output,
            required_vars,
            ctx: None,
            lower_equations: false,
            tag_writes: false,
        }
    }

    pub fn tag_reads(&mut self, var: Variable) -> bool {
        self.tagged_reads.insert(var)
    }

    pub fn with_tagged_reads(mut self, tagged_vars: AHashSet<Variable>) -> Self {
        self.tagged_reads = tagged_vars;
        self
    }

    pub fn tag_writes(&mut self) {
        self.tag_writes = true;
    }

    pub fn with_tagged_writes(mut self) -> Self {
        self.tag_writes = true;
        self
    }

    pub fn lower_equations(&mut self) {
        self.lower_equations = true;
    }

    pub fn with_equations(mut self) -> Self {
        self.lower_equations = true;
        self
    }

    pub fn with_ctx(mut self, ctx: &'a mut FunctionBuilderContext) -> Self {
        self.ctx = Some(ctx);
        self
    }

    pub fn with_builder_ctx(mut self, ctx: &'a mut FunctionBuilderContext) -> Self {
        self.ctx = Some(ctx);
        self
    }

    pub fn build(self, literals: &mut Rodeo) -> (Function, HirInterner) {
        let mut func = Function::default();
        let mut interner = HirInterner::default();

        let mut ctx_;
        let ctx = if let Some(ctx) = self.ctx {
            ctx
        } else {
            ctx_ = FunctionBuilderContext::new();
            &mut ctx_
        };

        let builder: FunctionBuilder<'_> =
            FunctionBuilder::new(&mut func, literals, ctx, self.tag_writes);
        let path = self.module.name(self.db);
        let analog_initial_body = self.module.analog_initial_block(self.db);
        let analog_body = self.module.analog_block(self.db);

        let mut ctx = LoweringCtx::new(self.db, builder, !self.lower_equations, &mut interner)
            .with_tagged_vars(self.tagged_reads);
        let mut body_ctx =
            BodyLoweringCtx { ctx: &mut ctx, body: analog_initial_body.borrow(), path: &path };

        // lower analog initial blocks first
        body_ctx.lower_entry_stmts();
        // ... and normal analog blocks afterwards
        body_ctx.body = analog_body.borrow();
        body_ctx.lower_entry_stmts();

        for var in self.required_vars {
            ctx.dec_place(PlaceKind::Var(var));
        }
        let is_output = self.is_output;
        ctx.intern.outputs = ctx
            .places
            .iter_enumerated()
            .map(|(place, kind)| {
                if is_output(*kind) {
                    let mut val = ctx.func.use_var(place);
                    val = ctx.func.ins().optbarrier(val);
                    (*kind, val.into())
                } else {
                    (*kind, None.into())
                }
            })
            .collect();
        ctx.func.ins().ret();
        ctx.func.finalize();
        (func, interner)
    }
}
