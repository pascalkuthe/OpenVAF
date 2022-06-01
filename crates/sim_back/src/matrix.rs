use ahash::AHashMap;
use bitset::BitSet;
use hir_def::db::HirDefDB;
use hir_def::ParamSysFun;
use hir_lower::{HirInterner, ParamKind};
use indexmap::map::Entry;
use mir::builder::InstBuilder;
use mir::cursor::FuncCursor;
use mir::{DerivativeInfo, Function, Unkown, Value, F_ZERO};
use stdx::{format_to, impl_debug_display, impl_idx_from};
use typed_indexmap::TiMap;

use crate::residual::ResidualId;
use crate::SimUnkown;

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct MatrixEntry {
    pub row: SimUnkown,
    pub col: SimUnkown,
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
    pub(crate) fn populate(
        &mut self,
        func: &mut FuncCursor,
        intern: &mut HirInterner,
        residual: &TiMap<ResidualId, SimUnkown, Value>,
        react: bool,
        derivatives: &AHashMap<(Value, Unkown), Value>,
        derivative_info: &DerivativeInfo,
    ) {
        for (row, residual) in residual.raw.iter() {
            for (kind, val) in intern.params.raw.iter() {
                if func.func.dfg.value_dead(*val) {
                    continue;
                }
                let (col_hi, col_lo) = match kind {
                    ParamKind::Voltage { hi, lo } => {
                        (SimUnkown::KirchoffLaw(*hi), lo.map(SimUnkown::KirchoffLaw))
                    }

                    ParamKind::ImplicitUnkown(equation) => (SimUnkown::Implicit(*equation), None),

                    ParamKind::Current(kind) => (SimUnkown::Current(*kind), None),
                    _ => continue,
                };

                let unkown = derivative_info.unkowns.unwrap_index(val);
                if let Some(ddx) = derivatives.get(&(*residual, unkown)).copied() {
                    if ddx == F_ZERO {
                        continue;
                    }

                    self.ensure_entry(func, *row, col_hi, ddx, false, react);
                    if let Some(col_lo) = col_lo {
                        self.ensure_entry(func, *row, col_lo, ddx, true, react);
                    }
                }
            }
        }

        let mfactor = intern.ensure_param(func.func, ParamKind::ParamSysFun(ParamSysFun::mfactor));
        for val in self.resistive.raw.values_mut().chain(self.reactive.raw.values_mut()) {
            *val = func.ins().fmul(*val, mfactor)
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
        row: SimUnkown,
        col: SimUnkown,
        mut val: Value,
        neg: bool,
        reactive: bool,
    ) {
        let dst = if reactive { &mut self.reactive } else { &mut self.resistive };
        // no entrys for gnd nodes

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
            if let SimUnkown::KirchoffLaw(node) = entry.row {
                format_to!(res, "({}, ", db.node_data(node).name);
            } else {
                format_to!(res, "({:?}, ", entry.row);
            }

            if let SimUnkown::KirchoffLaw(node) = entry.col {
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
            if let SimUnkown::KirchoffLaw(node) = entry.row {
                format_to!(res, "({}, ", db.node_data(node).name);
            } else {
                format_to!(res, "({:?}, ", entry.row);
            }

            if let SimUnkown::KirchoffLaw(node) = entry.col {
                format_to!(res, "{}", db.node_data(node).name);
            } else {
                format_to!(res, "{:?}", entry.col);
            }
            format_to!(res, ") = {}\n", val)
        }

        res
    }
}
