use ahash::{AHashMap, AHashSet};
use bitset::BitSet;
use hir_def::db::HirDefDB;
use hir_def::{NodeId, ParamSysFun};
use hir_lower::{
    HirInterner, ImplicitEquationKind, ParamKind, PlaceKind, REACTIVE_DIM, RESISTIVE_DIM,
};
use hir_ty::inference::BranchWrite;
use indexmap::map::Entry;
use mir::builder::InstBuilder;
use mir::cursor::{Cursor, FuncCursor};
use mir::{ControlFlowGraph, Function, Inst, KnownDerivatives, Value, FALSE, F_ZERO, TRUE};
use stdx::{impl_debug_display, impl_idx_from};
use typed_indexmap::TiMap;

use crate::compilation_db::CompilationDB;
use crate::util::{get_contrib, has_any_contrib, strip_optbarrier, SwitchBranchInfo};
use crate::SimUnknown;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct ResidualId(u32);
impl_idx_from!(ResidualId(u32));
impl_debug_display! {match ResidualId{ResidualId(id) => "res{id}";}}

fn src_residual(
    cursor: &mut FuncCursor,
    intern: &mut HirInterner,
    dst: BranchWrite,
    voltage_src: bool,
    db: &CompilationDB,
) -> [Value; 2] {
    let resist_val = get_contrib(cursor.func, intern, dst, RESISTIVE_DIM, voltage_src);
    let react_val = get_contrib(cursor.func, intern, dst, REACTIVE_DIM, voltage_src);

    let unknown = if voltage_src {
        let (hi, lo) = dst.nodes(db);
        ParamKind::Voltage { hi, lo }
    } else {
        ParamKind::Current(dst.into())
    };
    let unknown = intern.ensure_param(cursor.func, unknown);

    let resit_residual = if resist_val == F_ZERO {
        if react_val == F_ZERO {
            unknown
        } else {
            cursor.ins().fneg(unknown)
        }
    } else {
        cursor.ins().fsub(resist_val, unknown)
    };

    [resit_residual, react_val]
}

#[derive(Default, Debug)]
pub struct Residual {
    pub resistive: TiMap<ResidualId, SimUnknown, Value>,
    pub reactive: TiMap<ResidualId, SimUnknown, Value>,
}

impl Residual {
    pub fn contains(&self, unknown: SimUnknown) -> bool {
        self.resistive.contains_key(&unknown) || self.reactive.contains_key(&unknown)
    }

    #[allow(clippy::too_many_arguments)]
    fn add_branch_contributions(
        &mut self,
        is_voltage_src: Value,
        branch: BranchWrite,
        db: &CompilationDB,
        cursor: &mut FuncCursor,
        intern: &mut HirInterner,
        cfg: &mut ControlFlowGraph,
        op_dependent_insts: &BitSet<Inst>,
    ) {
        let current = branch.into();
        let is_voltage_src = strip_optbarrier(cursor.func, is_voltage_src);

        let residual = match is_voltage_src {
            FALSE => {
                let requires_unknown =
                    intern.is_param_live(cursor.func, &ParamKind::Current(current));
                if requires_unknown {
                    src_residual(cursor, intern, branch, false, db)
                } else {
                    // no extra unknowns required

                    let resist_val = get_contrib(cursor.func, intern, branch, RESISTIVE_DIM, false);
                    let react_val = get_contrib(cursor.func, intern, branch, REACTIVE_DIM, false);
                    if resist_val != F_ZERO {
                        self.add_kirchhoff_laws(cursor, resist_val, branch, false, db);
                    }
                    if react_val != F_ZERO {
                        self.add_kirchhoff_laws(cursor, react_val, branch, true, db);
                    }
                    return;
                }
            }

            TRUE => {
                // voltage src

                let requires_unknown =
                    intern.is_param_live(cursor.func, &ParamKind::Current(current));

                let static_collapse =
                    !requires_unknown && !has_any_contrib(cursor.func, intern, branch, true);

                if static_collapse {
                    // just node collapsing
                    return;
                }

                src_residual(cursor, intern, branch, true, db)
            }

            // switch branch
            _ => {
                let br_info = SwitchBranchInfo::analyze(
                    cursor.func,
                    intern,
                    op_dependent_insts,
                    is_voltage_src,
                    branch,
                );
                let resist_current = get_contrib(cursor.func, intern, branch, RESISTIVE_DIM, false);
                let react_current = get_contrib(cursor.func, intern, branch, REACTIVE_DIM, false);

                if br_info.just_current_src() {
                    // no extra unknowns required so just node collapsing + current source is
                    // enough
                    if resist_current != F_ZERO {
                        self.add_kirchhoff_laws(cursor, resist_current, branch, false, db);
                    }

                    if react_current != F_ZERO {
                        self.add_kirchhoff_laws(cursor, react_current, branch, true, db);
                    }
                    return;
                }

                let start_bb = cursor.current_block().unwrap();
                let voltage_src_bb = cursor.layout_mut().append_new_block();
                let current_src_bb = cursor.layout_mut().append_new_block();
                let next_block = cursor.layout_mut().append_new_block();
                cfg.ensure_bb(next_block);
                cfg.add_edge(start_bb, voltage_src_bb);
                cfg.add_edge(start_bb, current_src_bb);
                cfg.add_edge(voltage_src_bb, next_block);
                cfg.add_edge(current_src_bb, next_block);

                cursor.ins().br(is_voltage_src, voltage_src_bb, current_src_bb);
                cursor.goto_bottom(voltage_src_bb);
                let voltage_residual = src_residual(cursor, intern, branch, true, db);
                cursor.ins().jump(next_block);

                cursor.goto_bottom(current_src_bb);

                let current_residual = if br_info.introduce_unknown {
                    src_residual(cursor, intern, branch, false, db)
                } else {
                    let val = if br_info.op_dependent {
                        intern.ensure_param(cursor.func, ParamKind::Current(branch.into()))
                    } else {
                        F_ZERO
                    };
                    [val, F_ZERO]
                };
                cursor.ins().jump(next_block);

                cursor.goto_bottom(next_block);
                let residual_resist = cursor.ins().phi(&[
                    (current_src_bb, current_residual[0]),
                    (voltage_src_bb, voltage_residual[0]),
                ]);
                let residual_react = cursor.ins().phi(&[
                    (current_src_bb, current_residual[1]),
                    (voltage_src_bb, voltage_residual[1]),
                ]);

                if !br_info.introduce_unknown {
                    if resist_current != F_ZERO {
                        let residual_resist = cursor
                            .ins()
                            .phi(&[(current_src_bb, resist_current), (voltage_src_bb, F_ZERO)]);
                        self.add_kirchhoff_laws(cursor, residual_resist, branch, false, db);
                    }

                    if react_current != F_ZERO {
                        let residual_react = cursor
                            .ins()
                            .phi(&[(current_src_bb, react_current), (voltage_src_bb, F_ZERO)]);
                        self.add_kirchhoff_laws(cursor, residual_react, branch, false, db);
                    }
                }

                [residual_resist, residual_react]
            }
        };

        self.add_entry(cursor, SimUnknown::Current(current), residual[0], false, false);
        self.add_entry(cursor, SimUnknown::Current(current), residual[1], false, true);

        let current = intern.ensure_param(cursor.func, ParamKind::Current(current));
        self.add_kirchhoff_laws(cursor, current, branch, false, db);
    }

    pub fn populate(
        &mut self,
        db: &CompilationDB,
        cursor: &mut FuncCursor,
        intern: &mut HirInterner,
        cfg: &mut ControlFlowGraph,
        op_dependent_insts: &BitSet<Inst>,
        pruned: &AHashSet<NodeId>,
    ) {
        // self.remove_linear_ddt_unknowns(cursor, intern, op_dependent_insts);
        for i in 0..intern.outputs.len() {
            let (kind, val) = intern.outputs.get_index(i).unwrap();

            match *kind {
                PlaceKind::IsVoltageSrc(branch) => {
                    if let (node, None) = branch.nodes(db) {
                        if pruned.contains(&node) {
                            continue;
                        }
                    }
                    self.add_branch_contributions(
                        val.unwrap_unchecked(),
                        branch,
                        db,
                        cursor,
                        intern,
                        cfg,
                        op_dependent_insts,
                    )
                }
                PlaceKind::ImplicitResidual {
                    equation,
                    dim: dim @ (RESISTIVE_DIM | REACTIVE_DIM),
                } => {
                    if matches!(
                        intern.implicit_equations[equation],
                        ImplicitEquationKind::LinearDdt
                    ) {
                        continue;
                    }
                    self.add_entry(
                        cursor,
                        SimUnknown::Implicit(equation),
                        strip_optbarrier(cursor.func, val.unwrap_unchecked()),
                        false,
                        dim == REACTIVE_DIM,
                    );
                }

                _ => (),
            }
        }

        let mfactor =
            intern.ensure_param(cursor.func, ParamKind::ParamSysFun(ParamSysFun::mfactor));
        for val in self.resistive.raw.values_mut().chain(self.reactive.raw.values_mut()) {
            *val = cursor.ins().fmul(*val, mfactor)
        }
    }

    pub fn jacobian_derivatives(
        &self,
        func: &Function,
        intern: &HirInterner,
        derivatives: &KnownDerivatives,
    ) -> Vec<(Value, mir::Unknown)> {
        let mut params: Vec<_> = intern
            .live_params(&func.dfg)
            .filter_map(move |(_, kind, param)| {
                if matches!(
                    kind,
                    ParamKind::Voltage { .. }
                        | ParamKind::Current(_)
                        | ParamKind::ImplicitUnknown(_)
                ) {
                    Some(derivatives.unknowns.unwrap_index(&param))
                } else {
                    None
                }
            })
            .collect();
        let lim_derivatives = intern.lim_state.raw.values().flat_map(|vals| {
            vals.iter().filter_map(|(val, _)| {
                if func.dfg.value_dead(*val) {
                    return None;
                }

                Some(derivatives.unknowns.unwrap_index(val))
            })
        });
        params.extend(lim_derivatives);

        let num_unknowns = params.len() * (self.resistive.len() + self.reactive.len());
        let mut res = Vec::with_capacity(num_unknowns);
        for dim in [&self.resistive, &self.reactive] {
            for residual in dim.raw.values() {
                res.extend(params.iter().map(|unknown| (*residual, *unknown)))
            }
        }

        res
    }

    fn add_kirchhoff_laws(
        &mut self,
        cursor: &mut FuncCursor,
        current: Value,
        dst: BranchWrite,
        reactive: bool,
        db: &CompilationDB,
    ) {
        let (hi, lo) = dst.nodes(db);
        self.add_entry(cursor, SimUnknown::KirchhoffLaw(hi), current, false, reactive);
        if let Some(lo) = lo {
            self.add_entry(cursor, SimUnknown::KirchhoffLaw(lo), current, true, reactive);
        }
    }

    pub(crate) fn insert_opt_barries(
        &mut self,
        func: &mut FuncCursor,
        output_values: &mut BitSet<Value>,
    ) {
        fn insert_opt_barries(
            entries: &mut TiMap<ResidualId, SimUnknown, Value>,
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
            entries: &mut TiMap<ResidualId, SimUnknown, Value>,
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

    pub(crate) fn add_entry(
        &mut self,
        func: &mut FuncCursor,
        node: SimUnknown,
        mut val: Value,
        neg: bool,
        reactive: bool,
    ) {
        let dst = if reactive { &mut self.reactive } else { &mut self.resistive };
        // no entrys for gnd nodes

        match dst.raw.entry(node) {
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

    pub fn resistive_entries(&self, db: &CompilationDB) -> AHashMap<String, Value> {
        self.resistive
            .raw
            .iter()
            .filter_map(|(unknown, val)| {
                if let SimUnknown::KirchhoffLaw(node) = unknown {
                    let name = db.node_data(*node).name.to_string();
                    Some((name, *val))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn reactive_entries(&self, db: &CompilationDB) -> AHashMap<String, Value> {
        self.reactive
            .raw
            .iter()
            .filter_map(|(unknown, val)| {
                if let SimUnknown::KirchhoffLaw(node) = unknown {
                    let name = db.node_data(*node).name.to_string();
                    Some((name, *val))
                } else {
                    None
                }
            })
            .collect()
    }
}
