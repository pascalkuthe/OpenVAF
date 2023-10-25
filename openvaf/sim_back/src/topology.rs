//! This module is responsible for building a set of coherent branch
//! defininations from the raw lowering results. These can be
//! used to build the model topology (residual, matrix, noise sources)
//! without significant additional analyze.
//!
//! This process is quite involved as many things in Verilog-A are quite
//! implicit and don't quite match our topology model. In particular this
//! module will:
//!
//! * Turn function calls like `ddt` and `whitle_noise` either into direct
//!   contributions if possible (lineraization) or into an implicit node/
//!   intenal equation if not.
//! * Determine all nodes which are statically known to always have a large signal
//!   voltage of zero (small_signal_network).
//! * Separate contributionsi made form the small signal network to the large
//!   signal network into separate values where possible (prune). Prevents the
//!   generation of unnecessary derivatives.
//!

use ahash::AHashMap;
use bitset::{BitSet, SparseBitMatrix};
use hir_lower::{CallBackKind, HirInterner, ImplicitEquation, ParamKind, PlaceKind};
use indexmap::IndexSet;
use lasso::Spur;
use mir::{strip_optbarrier, Function, Inst, Value, F_ZERO, TRUE};
use mir_build::SSAVariableBuilder;
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiVec;
use typed_indexmap::TiMap;

use crate::context::Context;
use crate::noise::NoiseSourceKind;
use crate::topology::builder::Builder;
use crate::util::strip_optbarrier_if_const;
use crate::BranchWrite;

mod builder;
mod lineralize;
mod small_signal_network;
#[cfg(test)]
mod test;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct BranchId(u32);
impl_idx_from!(BranchId(u32));
impl_debug_display! {match BranchId{BranchId(id) => "branch{id}";}}

#[derive(Debug)]
pub(crate) struct BranchInfo {
    pub is_voltage_src: Value,
    pub voltage_src: Contribution,
    pub current_src: Contribution,
}

#[derive(Debug, Clone)]
pub(crate) struct Contribution {
    pub unknown: Option<Value>,
    pub resist: Value,
    pub react: Value,
    pub resist_small_signal: Value,
    pub react_small_signal: Value,
    pub noise: Vec<Noise>,
}

impl Contribution {
    pub fn is_trivial(&self) -> bool {
        self.resist == F_ZERO
            && self.react == F_ZERO
            && self.resist_small_signal == F_ZERO
            && self.react_small_signal == F_ZERO
            && self.noise.is_empty()
    }
}

impl Default for Contribution {
    fn default() -> Self {
        Contribution {
            unknown: None,
            resist: F_ZERO,
            react: F_ZERO,
            resist_small_signal: F_ZERO,
            react_small_signal: F_ZERO,
            noise: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ContributeKind {
    Branch { id: BranchId, is_voltage_src: bool, is_reactive: bool },
    ImplicitEquation { equation: ImplicitEquation, is_reactive: bool },
}

impl ContributeKind {
    pub fn is_reactive(&self) -> bool {
        match *self {
            ContributeKind::Branch { is_reactive, .. } => is_reactive,
            ContributeKind::ImplicitEquation { is_reactive, .. } => is_reactive,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Noise {
    pub name: Spur,
    pub kind: NoiseSourceKind,
    pub factor: Value,
}

impl Noise {
    pub fn new(
        inst: Inst,
        cb: &CallBackKind,
        factor: Value,
        ssa_builder: &mut SSAVariableBuilder,
        func: &mut Function,
    ) -> Noise {
        let (kind, name) = match *cb {
            CallBackKind::WhiteNoise { name, .. } => {
                let mut pwr = func.dfg.instr_args(inst)[0];
                pwr = ssa_builder.define_at_exit(func, F_ZERO, pwr, inst);
                (NoiseSourceKind::WhiteNoise { pwr }, name)
            }
            CallBackKind::FlickerNoise { name, .. } => {
                let pwr = func.dfg.instr_args(inst)[0];
                let exp = func.dfg.instr_args(inst)[1];
                (
                    NoiseSourceKind::FlickerNoise {
                        pwr: ssa_builder.define_at_exit(func, F_ZERO, pwr, inst),
                        exp: ssa_builder.define_at_exit(func, F_ZERO, exp, inst),
                    },
                    name,
                )
            }
            CallBackKind::NoiseTable(ref table) => (
                NoiseSourceKind::NoiseTable { log: table.log, vals: table.vals.clone() },
                table.name,
            ),
            _ => unreachable!(),
        };
        Noise { name, kind, factor }
    }
}

/// An intermediat representation the toplology of a circuit. It represents circuit
/// topology as a set of contributions to branches and implicit equations. These contributions
/// are divided into resistive/reactive voltage/current
#[derive(Debug)]
pub(crate) struct Topology {
    pub(crate) branches: TiMap<BranchId, BranchWrite, BranchInfo>,
    pub(crate) implicit_equations: TiVec<ImplicitEquation, Contribution>,
    pub(crate) small_signal_vals: IndexSet<Value, ahash::RandomState>,
    contributes: AHashMap<Value, ContributeKind>,
}

impl Topology {
    pub(crate) fn new(ctx: &mut Context) -> Self {
        let mut branches = TiMap::with_capacity(128);
        let mut contributes = AHashMap::with_capacity(128);
        let mut implicit_equations: TiVec<_, _> = ctx
            .intern
            .implicit_equations
            .keys()
            .filter_map(|eq| {
                let eq_val = ctx.intern.params.get(&ParamKind::ImplicitUnknown(eq));
                let eq_val = if let Some(&eq_val) = eq_val {
                    if ctx.func.dfg.value_dead(eq_val) {
                        return None;
                    }
                    eq_val
                } else {
                    return None;
                };
                let is_collapsed = ctx
                    .intern
                    .outputs
                    .get(&PlaceKind::CollapseImplicitEquation(eq))
                    .and_then(|val| val.expand())
                    .map(|val| strip_optbarrier(&ctx.func, val));
                if is_collapsed == Some(TRUE) {
                    ctx.func.dfg.replace_uses(eq_val, F_ZERO);
                    return None;
                }

                Some(Contribution {
                    unknown: ctx.intern.params.get(&ParamKind::ImplicitUnknown(eq)).copied(),
                    ..Contribution::default()
                })
            })
            .collect();
        for (kind, val) in &ctx.intern.outputs {
            if let Some(mut val) = val.expand() {
                val = strip_optbarrier_if_const(&ctx.func, val);
                match *kind {
                    PlaceKind::ImplicitResidual { equation, reactive: false } => {
                        implicit_equations[equation].resist = val;
                        contributes.insert(
                            val,
                            ContributeKind::ImplicitEquation { equation, is_reactive: false },
                        );
                    }
                    PlaceKind::ImplicitResidual { equation, reactive: true } => {
                        implicit_equations[equation].react = val;
                        contributes.insert(
                            val,
                            ContributeKind::ImplicitEquation { equation, is_reactive: true },
                        );
                    }
                    PlaceKind::IsVoltageSrc(branch) => {
                        let id: BranchId = branches.next_index();
                        let (hi, lo) = branch.nodes(ctx.db);
                        let is_voltage_src = val;
                        let has_voltage_probe =
                            ctx.intern.is_param_live(&ctx.func, &ParamKind::Voltage { hi, lo })
                                || is_voltage_src != mir::FALSE;
                        let has_current_probe =
                            ctx.intern.is_param_live(&ctx.func, &ParamKind::Current(branch.into()))
                                || is_voltage_src != mir::FALSE;
                        let voltage = has_voltage_probe.then(|| {
                            HirInterner::ensure_param_(
                                &mut ctx.intern.params,
                                &mut ctx.func,
                                ParamKind::Voltage { hi, lo },
                            )
                        });
                        let current = has_current_probe.then(|| {
                            HirInterner::ensure_param_(
                                &mut ctx.intern.params,
                                &mut ctx.func,
                                ParamKind::Current(branch.into()),
                            )
                        });
                        let mut get_contrib = |is_reactive: bool, is_voltage_src: bool| {
                            ctx.intern
                                .outputs
                                .get(&PlaceKind::Contribute {
                                    dst: branch,
                                    reactive: is_reactive,
                                    voltage_src: is_voltage_src,
                                })
                                .and_then(|it| it.expand())
                                .map(|mut val| {
                                    val = strip_optbarrier_if_const(&ctx.func, val);
                                    contributes.insert(
                                        val,
                                        ContributeKind::Branch { id, is_voltage_src, is_reactive },
                                    );
                                    val
                                })
                                .unwrap_or(F_ZERO)
                        };
                        let contrib = BranchInfo {
                            is_voltage_src,
                            voltage_src: Contribution {
                                unknown: voltage,
                                resist: get_contrib(false, true),
                                react: get_contrib(true, true),
                                resist_small_signal: F_ZERO,
                                react_small_signal: F_ZERO,
                                noise: Vec::new(),
                            },
                            current_src: Contribution {
                                unknown: current,
                                resist: get_contrib(false, false),
                                react: get_contrib(true, false),
                                resist_small_signal: F_ZERO,
                                react_small_signal: F_ZERO,
                                noise: Vec::new(),
                            },
                        };
                        branches.insert_full(branch, contrib);
                    }
                    _ => {}
                }
            }
        }
        let mut postdom_frontiers = SparseBitMatrix::new_square(ctx.func.layout.num_blocks());
        ctx.init_op_dependent_insts(&mut postdom_frontiers);
        ctx.dom_tree.compute_postdom_frontiers(&ctx.cfg, &mut postdom_frontiers);
        let mut topology = Topology {
            branches,
            contributes,
            implicit_equations,
            small_signal_vals: IndexSet::with_capacity_and_hasher(
                128,
                ahash::RandomState::default(),
            ),
        };
        let num_insts = ctx.func.dfg.num_insts();
        let mut builder = Builder {
            topology: &mut topology,
            db: ctx.db,
            func: &mut ctx.func,
            output_values: &ctx.output_values,
            cfg: &ctx.cfg,
            scratch_buf: BitSet::new_empty(num_insts),
            postorder: Vec::with_capacity(128),
            val_map: AHashMap::with_capacity(128),
            edges: Vec::with_capacity(128),
            phis: Vec::with_capacity(128),
            op_dependent_insts: &ctx.op_dependent_insts,
            op_dependent_vals: &ctx.op_dependent_vals,
        };
        let operators = builder.analog_operator_evaluations(&postdom_frontiers, &mut ctx.intern);
        drop(postdom_frontiers);
        builder.builid_analog_operators(operators, &mut ctx.intern);
        builder.prune_small_signal();
        topology.contributes = AHashMap::new();
        topology
    }

    fn as_contribution(&self, val: Value) -> Option<ContributeKind> {
        self.contributes.get(&val).copied()
    }

    fn get_branch(&self, branch: BranchId) -> &BranchInfo {
        &self.branches[branch]
    }

    fn get_branch_mut(&mut self, branch: BranchId) -> &mut BranchInfo {
        &mut self.branches[branch]
    }
    fn get_mut(&mut self, contrib: ContributeKind) -> &mut Contribution {
        match contrib {
            ContributeKind::Branch { id, is_voltage_src: true, .. } => {
                &mut self.get_branch_mut(id).voltage_src
            }
            ContributeKind::Branch { id, is_voltage_src: false, .. } => {
                &mut self.get_branch_mut(id).current_src
            }
            ContributeKind::ImplicitEquation { equation: eq, .. } => {
                &mut self.implicit_equations[eq]
            }
        }
    }

    fn branches(&self) -> typed_indexmap::map::Iter<'_, BranchId, BranchWrite, BranchInfo> {
        self.branches.iter_enumerated()
    }
    fn new_implicit_equation(&mut self, eq: ImplicitEquation, contributes: Contribution) {
        if contributes.resist != F_ZERO {
            self.contributes.insert(
                contributes.resist,
                ContributeKind::ImplicitEquation { equation: eq, is_reactive: false },
            );
        }
        if contributes.react != F_ZERO {
            self.contributes.insert(
                contributes.react,
                ContributeKind::ImplicitEquation { equation: eq, is_reactive: true },
            );
        }
        let eq_ = self.implicit_equations.push_and_get_key(contributes);
        debug_assert_eq!(eq_, eq);
    }
}
