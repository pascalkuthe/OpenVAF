use ahash::{AHashMap, RandomState};
use bitset::BitSet;
use hir_lower::{HirInterner, ParamKind};
use indexmap::map::Entry;
use indexmap::IndexMap;
use mir::builder::InstBuilder;
use mir::cursor::FuncCursor;
use mir::{Function, KnownDerivatives, Unknown, Value, F_ZERO};
use typed_indexmap::TiMap;

use crate::residual::ResidualId;
use crate::{Residual, SimUnknown};

#[derive(Default, Debug)]
pub struct LimRhs {
    pub resistive: IndexMap<SimUnknown, Value, RandomState>,
    pub reactive: IndexMap<SimUnknown, Value, RandomState>,
}

impl LimRhs {
    pub fn new(
        func: &mut FuncCursor,
        intern: &mut HirInterner,
        residual: &Residual,
        derivatives: &AHashMap<(Value, Unknown), Value>,
        derivative_info: &KnownDerivatives,
    ) -> LimRhs {
        let mut res = LimRhs::default();
        res.populate(func, intern, &residual.resistive, false, derivatives, derivative_info);
        res.populate(func, intern, &residual.reactive, true, derivatives, derivative_info);
        res
    }

    pub(crate) fn populate(
        &mut self,
        func: &mut FuncCursor,
        intern: &mut HirInterner,
        residual: &TiMap<ResidualId, SimUnknown, Value>,
        react: bool,
        derivatives: &AHashMap<(Value, Unknown), Value>,
        derivative_info: &KnownDerivatives,
    ) {
        for (residual, residual_val) in residual.iter() {
            for (state, (unchanged, lim_vals)) in intern.lim_state.iter_enumerated() {
                for (val, neg) in lim_vals {
                    let unknown = derivative_info.unknowns.unwrap_index(val);
                    if let Some(ddx) = derivatives.get(&(*residual_val, unknown)).copied() {
                        if ddx != F_ZERO {
                            let changed = HirInterner::ensure_param_(
                                &mut intern.params,
                                func.func,
                                ParamKind::NewState(state),
                            )
                            .0;

                            let delta = if *neg {
                                func.ins().fadd(changed, *unchanged)
                            } else {
                                func.ins().fsub(changed, *unchanged)
                            };

                            let rhs = func.ins().fmul(ddx, delta);
                            self.ensure_entry(func, *residual, rhs, react);
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn insert_opt_barries(
        &mut self,
        func: &mut FuncCursor,
        output_values: &mut BitSet<Value>,
    ) {
        fn insert_opt_barries(
            entries: &mut IndexMap<SimUnknown, Value, RandomState>,
            func: &mut FuncCursor,
            output_values: &mut BitSet<Value>,
        ) {
            for val in entries.values_mut() {
                *val = func.ins().optbarrier(*val);
            }

            output_values.ensure(func.func.dfg.num_values() + 1);

            for val in entries.values() {
                output_values.insert(*val);
            }
        }

        insert_opt_barries(&mut self.resistive, func, output_values);
        insert_opt_barries(&mut self.reactive, func, output_values);
    }

    pub(crate) fn strip_opt_barries(
        &mut self,
        func: &mut Function,
        output_values: &mut BitSet<Value>,
    ) {
        fn strip_opt_barries(
            entries: &mut IndexMap<SimUnknown, Value, RandomState>,
            func: &mut Function,
            output_values: &mut BitSet<Value>,
        ) {
            for val in entries.values_mut() {
                let inst = func.dfg.value_def(*val).unwrap_inst();
                output_values.remove(*val);
                *val = func.dfg.instr_args(inst)[0];
                output_values.insert(*val);
                func.layout.remove_inst(inst);
            }
        }

        strip_opt_barries(&mut self.resistive, func, output_values);
        strip_opt_barries(&mut self.reactive, func, output_values);
    }

    pub(crate) fn ensure_entry(
        &mut self,
        func: &mut FuncCursor,
        residual: SimUnknown,
        val: Value,
        reactive: bool,
    ) {
        let dst = if reactive { &mut self.reactive } else { &mut self.resistive };
        // no entrys for gnd nodes

        match dst.entry(residual) {
            Entry::Occupied(dst) => {
                let dst = dst.into_mut();
                *dst = func.ins().fadd(*dst, val)
            }
            Entry::Vacant(dst) => {
                dst.insert(val);
            }
        }
    }

    pub(crate) fn sparsify(&mut self) {
        self.resistive.retain(|_, val| *val != F_ZERO);
        self.reactive.retain(|_, val| *val != F_ZERO);
    }
}
