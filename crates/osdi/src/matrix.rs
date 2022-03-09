use ahash::AHashMap;
use bitset::BitSet;
use hir_def::db::HirDefDB;
use hir_def::NodeId;
use hir_lower::{CallBackKind, HirInterner, ParamKind, PlaceKind};
use hir_ty::db::HirTyDB;
use hir_ty::lower::BranchKind;
use mir::builder::InstBuilder;
use mir::cursor::FuncCursor;
use mir::{Function, Value, F_ZERO};
use mir_autodiff::FirstOrderUnkown;
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::compilation_db::CompilationDB;

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct MatrixEntry {
    pub row: NodeId,
    pub col: NodeId,
}
impl_debug_display! {match MatrixEntry{MatrixEntry{row, col} => "({row:?}, {col:?})";}}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct MatrixEntryId(u32);
impl_idx_from!(MatrixEntryId(u32));
impl_debug_display! {match MatrixEntryId{MatrixEntryId(id) => "j{id}";}}

#[derive(Default, Debug)]
pub(crate) struct JacobianMatrix {
    pub entries: TiSet<MatrixEntryId, MatrixEntry>,
    pub values: TiVec<MatrixEntryId, Value>,
}

impl JacobianMatrix {
    pub(crate) fn populate(
        &mut self,
        db: &CompilationDB,
        func: &mut FuncCursor,
        intern: &HirInterner,
        derivatives: &AHashMap<(Value, FirstOrderUnkown), Value>,
    ) {
        for (out_kind, rhs) in intern.outputs.iter() {
            let (row_hi, row_lo) = match out_kind {
                PlaceKind::BranchVoltage(_) | PlaceKind::ImplicitBranchVoltage { .. } => {
                    todo!("voltage contribute")
                }
                PlaceKind::BranchCurrent(branch) => match db.branch_info(*branch).unwrap().kind {
                    BranchKind::NodeGnd(node) => (node, None),
                    BranchKind::Nodes(hi, lo) => (hi, Some(lo)),
                    BranchKind::PortFlow(_) => unreachable!(),
                },
                PlaceKind::ImplicitBranchCurrent { hi, lo } => (*hi, *lo),
                _ => continue,
            };

            let hi_gnd = db.node_data(row_hi).is_gnd;
            let lo_gnd = row_lo.map_or(false, |lo| db.node_data(lo).is_gnd);

            for (param, (kind, _)) in intern.params.iter_enumerated() {
                let (col_hi, col_lo) = match kind {
                    ParamKind::Voltage { hi, lo } => (*hi, *lo),
                    // ParamKind::Current(_) => TODO voltage contribute
                    _ => continue,
                };
                let unkown =
                    u32::from(intern.callbacks.unwrap_index(&CallBackKind::Derivative(param)));
                let unkown = FirstOrderUnkown::from(unkown);
                if let Some(ddx) = derivatives.get(&(*rhs, unkown)).copied() {
                    if ddx == F_ZERO {
                        continue;
                    }

                    if !hi_gnd {
                        self.ensure_entry(func, row_hi, col_hi, ddx, false);
                        if let Some(col_lo) = col_lo {
                            self.ensure_entry(func, row_hi, col_lo, ddx, true);
                        }
                    }

                    if let Some(row_lo) = row_lo {
                        if !lo_gnd {
                            self.ensure_entry(func, row_lo, col_hi, ddx, true);
                            if let Some(col_lo) = col_lo {
                                self.ensure_entry(func, row_lo, col_lo, ddx, false);
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
        for val in &mut self.values {
            *val = func.ins().optbarrier(*val);
        }

        output_values.ensure(func.func.dfg.num_values() + 1);

        for val in &self.values {
            output_values.insert(*val);
        }
    }

    pub(crate) fn strip_opt_barries(
        &mut self,
        func: &mut Function,
        output_values: &mut BitSet<Value>,
    ) {
        for val in &mut self.values {
            let inst = func.dfg.value_def(*val).unwrap_inst();
            output_values.remove(*val);
            *val = func.dfg.instr_args(inst)[0];
            output_values.insert(*val);
            func.layout.remove_inst(inst);
        }
    }
    pub(crate) fn ensure_entry(
        &mut self,
        func: &mut FuncCursor,
        row: NodeId,
        col: NodeId,
        mut val: Value,
        neg: bool,
    ) {
        // no entrys for gnd nodes

        let (entry, changed) = self.entries.ensure(MatrixEntry { row, col });
        if changed {
            if neg {
                val = func.ins().fneg(val);
            }
            self.values.push(val)
        } else {
            let old = self.values[entry];
            self.values[entry] =
                if neg { func.ins().fsub(old, val) } else { func.ins().fadd(old, val) }
        }
    }

    pub(crate) fn sparsify(&mut self) {
        let mut iter = self.values.iter();
        self.entries.raw.retain(|_| iter.next() != Some(&F_ZERO));
        self.values.retain(|val| *val != F_ZERO);
    }
}
