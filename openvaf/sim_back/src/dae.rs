use indexmap::IndexSet;
use mir::{strip_optbarrier, Value, F_ZERO};
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::context::Context;
use crate::dae::builder::Builder;
pub use crate::noise::{NoiseSource, NoiseSourceKind};
use crate::{topology, SimUnknownKind};

mod builder;
#[cfg(test)]
mod tests;

/// An unknown in the system of DAE equations
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct SimUnknown(u32);
impl_idx_from!(SimUnknown(u32));
impl_debug_display! {match SimUnknown{SimUnknown(id) => "sim_node{id}";}}

/// Represents the topology of Verliog-A (top level) module as a set
/// of DAE equations.
///
/// I(x) + ddt(Q(x)) = 0
///
/// This system can be solved using a newton iteration:
///
/// J_I(x) delta_x = I(x) + ddt(Q)
/// x' = x - delta_x
///
#[derive(Default, Debug)]
pub struct DaeSystem {
    /// The unknowns of the DAE system which are solved (x)
    pub unknowns: TiSet<SimUnknown, SimUnknownKind>,
    /// The cost function of the DAE system (resistive: I, reactive: Q).
    /// Additionally contains
    pub residual: TiVec<SimUnknown, Residual>,
    /// The jacobian of the DAE system J_ij = (ddx(I_i, x_j), ddx(Q_i, x_j))
    pub jacobian: TiVec<MatrixEntryId, MatrixEntry>,
    /// list of parameter which are known to be small signal values (always zero during
    /// large signal simulation).
    pub small_signal_parameters: IndexSet<Value, ahash::RandomState>,
    /// noise
    pub noise_sources: Vec<NoiseSource>,
}

impl DaeSystem {
    pub(crate) fn new(ctx: &mut Context, contributions: topology::Topology) -> DaeSystem {
        let mut builder =
            Builder::new(ctx).with_small_signal_network(contributions.small_signal_vals);

        for (branch, contributions) in contributions.branches.raw {
            builder.build_branch(branch, &contributions)
        }
        for (eq, contributions) in contributions.implicit_equations.iter_enumerated() {
            builder.build_implicit_equation(eq, contributions)
        }
        builder.finish()
    }

    pub(super) fn sparsify(&mut self, ctx: &mut Context) {
        let mut sparsify = |val| {
            let stripped = strip_optbarrier(&ctx.func, val);
            if ctx.func.dfg.value_def(stripped).inst().is_some() {
                val
            } else {
                ctx.output_values.remove(val);
                if let Some(inst) = ctx.func.dfg.value_def(val).inst() {
                    if ctx.func.dfg.instr_safe_to_remove(inst) {
                        ctx.func.dfg.zap_inst(inst);
                        ctx.func.layout.remove_inst(inst);
                    }
                }
                stripped
            }
        };
        for residual in &mut self.residual {
            residual.map_vals(&mut sparsify)
        }

        self.noise_sources.retain_mut(|noise_src| {
            noise_src.map_vals(&mut sparsify);
            if noise_src.factor == F_ZERO {
                return false;
            }
            match noise_src.kind {
                NoiseSourceKind::WhiteNoise { pwr } => pwr != F_ZERO,
                NoiseSourceKind::FlickerNoise { pwr, .. } => pwr != F_ZERO,
                NoiseSourceKind::NoiseTable { .. } => true,
            }
        });

        self.jacobian.raw.retain_mut(|matrix_entry| {
            matrix_entry.resist = sparsify(matrix_entry.resist);
            matrix_entry.react = sparsify(matrix_entry.react);
            matrix_entry.resist != F_ZERO || matrix_entry.react != F_ZERO
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct Residual {
    /// The resistive part (I) of the DAE cost function
    pub resist: Value,
    /// The reactive part (Q) of the DAE cost function
    pub react: Value,
    resist_small_signal: Value,
    react_small_signal: Value,
    /// Corrective term that needs to be added during each newton iteration to
    /// correct for limiting. Limiting reduces the maximum change in a variable
    /// for this model. That means that instead of x the system is evaluated
    /// with x_lim. The corresponding newton would be
    ///
    /// J(lim_x) delta_limx = I(lim_x) + ddt(Q)                (1)
    /// x' = lim_x - delta_limx                                (2)
    ///
    /// However the simulator is not aware  of the limiting and will instead
    /// calculate: x' = x - delta_limx. That means x' has an error of
    /// err_x = lim_x - x. Inserting that error back into (2) yields a corrective
    /// term:
    /// x' = lim_x - delta_limx = x + err_x - delta_limx
    /// delta_x = delta_limx - err_x
    /// J(lim_x)  (delta_x + err_x) = I(lim_x) + ddt(Q)
    /// J(lim_x) delta_x  = I(lim_x) + ddt(Q) - J(lim_x) * err_x
    /// lim_rhs = J(lim_x) (lim_x - x)
    ///
    /// note that this term endsup being included automatically in handwritten
    /// models of spice-like simulator:
    /// J(lim_x) (x - x')  =   I(lim_x)  ddt(Q) - J(lim_x) (lim_x - x)
    /// J(lim_x) x'  = J(lim_x) x - I(lim_x) - ddt(Q) + J(lim_x) (lim_x - x)
    /// J(lim_x) x'  = J(lim_x) lim_x - I(lim_x) - ddt(Q)
    ///
    ///
    /// This corrective factor needs to be computed both for the resistive and
    /// reactive residual (the jacobian is madeup of both). The reactive component
    /// is stored in this variable.
    pub resist_lim_rhs: Value,
    /// Corrective term that needs to be added during each newton iteration to
    /// correct for limiting. Limiting reduces the maximum change in a variable
    /// for this model. That means that instead of x the system is evaluated
    /// with x_lim. The corresponding newton would be
    ///
    /// J(lim_x) delta_limx = I(lim_x) + ddt(Q)                (1)
    /// x' = lim_x - delta_limx                                (2)
    ///
    /// However the simulator is not aware  of the limiting and will instead
    /// calculate: x' = x - delta_limx. That means x' has an error of
    /// err_x = lim_x - x. Inserting that error back into (2) yields a corrective
    /// term:
    /// x' = lim_x - delta_limx = x + err_x - delta_limx
    /// delta_x = delta_limx - err_x
    /// J(lim_x)  (delta_x + err_x) = I(lim_x) + ddt(Q)
    /// J(lim_x) delta_x  = I(lim_x) + ddt(Q) - J(lim_x) * err_x
    /// lim_rhs = J(lim_x) (lim_x - x)
    ///
    /// note that this term endsup being included automatically in handwritten
    /// models of spice-like simulator:
    /// J(lim_x) (x - x')  =   I(lim_x)  ddt(Q) - J(lim_x) (lim_x - x)
    /// J(lim_x) x'  = J(lim_x) x - I(lim_x) - ddt(Q) + J(lim_x) (lim_x - x)
    /// J(lim_x) x'  = J(lim_x) lim_x - I(lim_x) - ddt(Q)
    ///
    ///
    /// This corrective factor needs to be computed both for the resistive and
    /// reactive residual (the jacobian is madeup of both). The reactive component
    /// is stored in this variable.
    pub react_lim_rhs: Value,
}

impl Default for Residual {
    fn default() -> Self {
        Residual {
            resist: F_ZERO,
            react: F_ZERO,
            resist_small_signal: F_ZERO,
            react_small_signal: F_ZERO,
            resist_lim_rhs: F_ZERO,
            react_lim_rhs: F_ZERO,
        }
    }
}

impl Residual {
    pub fn is_trivial(&self) -> bool {
        self.is_small_signal()
            && self.resist_small_signal == F_ZERO
            && self.react_small_signal == F_ZERO
    }

    pub fn is_small_signal(&self) -> bool {
        self.resist == F_ZERO && self.react == F_ZERO
    }

    pub fn map_vals(&mut self, mut f: impl FnMut(Value) -> Value) {
        self.resist = f(self.resist);
        self.react = f(self.react);
        self.resist_small_signal = f(self.resist_small_signal);
        self.react_small_signal = f(self.react_small_signal);
        self.resist_lim_rhs = f(self.resist_lim_rhs);
        self.react_lim_rhs = f(self.react_lim_rhs);
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub struct MatrixEntry {
    pub row: SimUnknown,
    pub col: SimUnknown,
    pub resist: Value,
    pub react: Value,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct MatrixEntryId(u32);
impl_idx_from!(MatrixEntryId(u32));
impl_debug_display! {match MatrixEntryId{MatrixEntryId(id) => "j{id}";}}
