use ahash::AHashMap;
use bitset::BitSet;
use hir_def::db::HirDefDB;
use hir_lower::{HirInterner, ParamKind};
use indexmap::map::Entry;
use mir::builder::InstBuilder;
use mir::cursor::FuncCursor;
use mir::{Function, KnownDerivatives, Unknown, Value, F_ZERO};
use stdx::{format_to, impl_debug_display, impl_idx_from};
use typed_indexmap::TiMap;

use crate::residual::ResidualId;
use crate::{Residual, SimUnknown};

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct MatrixEntry {
    pub row: SimUnknown,
    pub col: SimUnknown,
}
impl_debug_display! {match MatrixEntry{MatrixEntry{row, col} => "({row:?}, {col:?})";}}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct MatrixEntryId(u32);
impl_idx_from!(MatrixEntryId(u32));
impl_debug_display! {match MatrixEntryId{MatrixEntryId(id) => "j{id}";}}

#[derive(Default, Debug)]
pub struct JacobianMatrix {
    pub resistive: TiMap<MatrixEntryId, MatrixEntry, Value>,
    pub reactive: TiMap<MatrixEntryId, MatrixEntry, Value>,
}

impl JacobianMatrix {
    pub fn new(
        func: &mut FuncCursor,
        intern: &mut HirInterner,
        residual: &Residual,
        derivatives: &AHashMap<(Value, Unknown), Value>,
        derivative_info: &KnownDerivatives,
    ) -> JacobianMatrix {
        let mut res = JacobianMatrix::default();
        res.populate(func, intern, &residual.resistive, false, derivatives, derivative_info);
        res.populate(func, intern, &residual.reactive, true, derivatives, derivative_info);
        res
    }

    fn populate(
        &mut self,
        func: &mut FuncCursor,
        intern: &mut HirInterner,
        residual: &TiMap<ResidualId, SimUnknown, Value>,
        react: bool,
        derivatives: &AHashMap<(Value, Unknown), Value>,
        derivative_info: &KnownDerivatives,
    ) {
        for (row, residual) in residual.iter() {
            for (kind, val) in intern.params.iter() {
                if func.func.dfg.value_dead(*val) {
                    continue;
                }
                let (col_hi, col_lo) = match kind {
                    ParamKind::Voltage { hi, lo } => {
                        (SimUnknown::KirchoffLaw(*hi), lo.map(SimUnknown::KirchoffLaw))
                    }

                    ParamKind::ImplicitUnknown(equation) => (SimUnknown::Implicit(*equation), None),

                    ParamKind::Current(kind) => (SimUnknown::Current(*kind), None),
                    _ => continue,
                };

                let unknown = derivative_info.unknowns.unwrap_index(val);
                if let Some(ddx) = derivatives.get(&(*residual, unknown)).copied() {
                    if ddx != F_ZERO {
                        self.ensure_entry(func, *row, col_hi, ddx, false, react);
                        if let Some(col_lo) = col_lo {
                            self.ensure_entry(func, *row, col_lo, ddx, true, react);
                        }
                    }
                }

                if let Some(lim_vals) = intern.lim_state.raw.get(val) {
                    for (val, neg) in lim_vals {
                        let unknown = derivative_info.unknowns.unwrap_index(val);
                        if let Some(ddx) = derivatives.get(&(*residual, unknown)).copied() {
                            if ddx != F_ZERO {
                                self.ensure_entry(func, *row, col_hi, ddx, *neg, react);

                                if let Some(col_lo) = col_lo {
                                    self.ensure_entry(func, *row, col_lo, ddx, !neg, react);
                                }
                            }
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
            entries: &mut TiMap<MatrixEntryId, MatrixEntry, Value>,
            func: &mut FuncCursor,
            output_values: &mut BitSet<Value>,
        ) {
            for val in entries.raw.values_mut() {
                *val = func.ins().optbarrier(*val);
            }

            output_values.ensure(func.func.dfg.num_values() + 1);

            for val in entries.raw.values() {
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
            entries: &mut TiMap<MatrixEntryId, MatrixEntry, Value>,
            func: &mut Function,
            output_values: &mut BitSet<Value>,
        ) {
            for val in entries.raw.values_mut() {
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
        row: SimUnknown,
        col: SimUnknown,
        mut val: Value,
        neg: bool,
        reactive: bool,
    ) {
        let dst = if reactive { &mut self.reactive } else { &mut self.resistive };
        // no entries for gnd nodes

        match dst.raw.entry(MatrixEntry { row, col }) {
            Entry::Occupied(dst) => {
                let dst = dst.into_mut();
                *dst = if neg { func.ins().fsub(*dst, val) } else { func.ins().fadd(*dst, val) }
            }
            Entry::Vacant(dst) => {
                if neg {
                    val = func.ins().fneg(val);
                }
                dst.insert(val);
            }
        }
    }

    pub(crate) fn sparsify(&mut self) {
        self.resistive.raw.retain(|_, val| *val != F_ZERO);
        self.reactive.raw.retain(|_, val| *val != F_ZERO);
    }
}

impl JacobianMatrix {
    pub fn print_resistive_stamps(&self, db: &dyn HirDefDB) -> String {
        let mut res = String::new();
        for (entry, val) in &self.resistive.raw {
            if let SimUnknown::KirchoffLaw(node) = entry.row {
                format_to!(res, "({}, ", db.node_data(node).name);
            } else {
                format_to!(res, "({:?}, ", entry.row);
            }

            if let SimUnknown::KirchoffLaw(node) = entry.col {
                format_to!(res, "{}", db.node_data(node).name);
            } else {
                format_to!(res, "{:?}", entry.col);
            }
            format_to!(res, ") = {}\n", val)
        }

        res
    }

    pub fn print_reactive_stamps(&self, db: &dyn HirDefDB) -> String {
        let mut res = String::new();
        for (entry, val) in &self.reactive.raw {
            if let SimUnknown::KirchoffLaw(node) = entry.row {
                format_to!(res, "({}, ", db.node_data(node).name);
            } else {
                format_to!(res, "({:?}, ", entry.row);
            }

            if let SimUnknown::KirchoffLaw(node) = entry.col {
                format_to!(res, "{}", db.node_data(node).name);
            } else {
                format_to!(res, "{:?}", entry.col);
            }
            format_to!(res, ") = {}\n", val)
        }

        res
    }
}
