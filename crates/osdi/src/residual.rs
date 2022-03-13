use bitset::BitSet;
use hir_def::db::HirDefDB;
use hir_def::NodeId;
use hir_lower::{HirInterner, PlaceKind};
use hir_ty::db::HirTyDB;
use hir_ty::lower::BranchKind;
use indexmap::map::Entry;
use mir::builder::InstBuilder;
use mir::cursor::FuncCursor;
use mir::{Function, Value};
use stdx::{impl_debug_display, impl_idx_from};
use typed_indexmap::TiMap;

use crate::compilation_db::CompilationDB;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct LocalNodeId(u32);
impl_idx_from!(LocalNodeId(u32));
impl_debug_display! {match LocalNodeId{LocalNodeId(id) => "j{id}";}}

#[derive(Default, Debug)]
pub struct Residual {
    pub elements: TiMap<LocalNodeId, NodeId, Value>,
}

impl Residual {
    pub fn populate(&mut self, db: &CompilationDB, func: &mut FuncCursor, intern: &HirInterner) {
        for (out_kind, val) in intern.outputs.iter() {
            let (hi, lo) = match out_kind {
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

            let hi_gnd = db.node_data(hi).is_gnd;
            let lo_gnd = lo.map_or(true, |lo| db.node_data(lo).is_gnd);
            if !hi_gnd {
                self.add_entry(func, hi, *val, false)
            }

            if !lo_gnd {
                if let Some(lo) = lo {
                    self.add_entry(func, lo, *val, true)
                }
            }
        }
    }

    pub(crate) fn insert_opt_barries(
        &mut self,
        func: &mut FuncCursor,
        output_values: &mut BitSet<Value>,
    ) {
        for val in self.elements.raw.values_mut() {
            *val = func.ins().optbarrier(*val);
        }

        output_values.ensure(func.func.dfg.num_values() + 1);

        for val in self.elements.raw.values() {
            output_values.insert(*val);
        }
    }

    pub(crate) fn strip_opt_barries(
        &mut self,
        func: &mut Function,
        output_values: &mut BitSet<Value>,
    ) {
        for val in self.elements.raw.values_mut() {
            let inst = func.dfg.value_def(*val).unwrap_inst();
            output_values.remove(*val);
            *val = func.dfg.instr_args(inst)[0];
            output_values.insert(*val);
            func.layout.remove_inst(inst);
        }
    }
    pub(crate) fn add_entry(
        &mut self,
        func: &mut FuncCursor,
        node: NodeId,
        mut val: Value,
        neg: bool,
    ) {
        // no entrys for gnd nodes

        match self.elements.raw.entry(node) {
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

    // pub(crate) fn sparsify(&mut self) {
    //     self.elements.raw.retain(|_, val| *val != F_ZERO);
    // }
}
