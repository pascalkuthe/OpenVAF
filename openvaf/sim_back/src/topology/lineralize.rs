//! This module is responsible for determining whether an internal unknown needs
//! to be created for an anlog opertor (like ddt) or to turn the analog operator
//! into a separate dimension instead.

use std::mem::take;

use bitset::SparseBitMatrix;
use hir_lower::{CallBackKind, HirInterner, ImplicitEquationKind, ParamKind, PlaceKind};
use mir::builder::InstBuilder;
use mir::cursor::{Cursor, FuncCursor};
use mir::{
    Block, FuncRef, Function, Inst, InstructionData, Opcode, PhiNode, Value, FALSE, F_ONE, F_ZERO,
    TRUE,
};
use typed_indexmap::TiSet;

use crate::topology::{Contribution, Noise};
use crate::util::{add, update_optbarrier};

#[derive(Debug)]
pub(super) enum Evaluation {
    /// The analog operator must be evaluated as a separate equation
    Equation,
    /// The analog operator can be evaluated as a linear contribution
    /// without the need for an additional unknown
    Linear {
        /// The contribute that this linear equation writes to
        /// Contains a painr of the original contribution and
        /// the separate dimension it was mapped to
        contributes: Box<[(Value, Value)]>,
    },
    /// This operator is not used and can be ignored
    Dead,
}

impl<'a> super::Builder<'a> {
    /// Build topology for a list of analog operators (noise and ddt) with a predetermined evaluation.
    pub(super) fn builid_analog_operators(
        &mut self,
        analog_operators: Vec<(Inst, Evaluation)>,
        intern: &mut HirInterner,
    ) {
        let mut ssa_builder = mir_build::SSAVariableBuilder::new(self.cfg);
        for (operator_inst, evaluation) in analog_operators {
            let arg0 = self.func.dfg.instr_args(operator_inst)[0];
            let cb = self.func.dfg.func_ref(operator_inst).unwrap();
            let is_noise = intern.callbacks[cb].is_noise();
            match evaluation {
                Evaluation::Dead => {
                    cov_mark::hit!(dead_noise);
                    let val = self.func.dfg.first_result(operator_inst);
                    self.func.dfg.replace_uses(val, F_ZERO);
                }
                Evaluation::Linear { contributes } => {
                    cov_mark::hit!(linear_operator);
                    let cb = &intern.callbacks[cb];
                    for (contribute, mut dimension) in &*contributes {
                        let resistive_contribute = *contribute;
                        let inst = self.func.dfg.value_def(resistive_contribute).inst().unwrap();
                        let contribute = self.topology.as_contribution(*contribute).unwrap();
                        let contribute = self.topology.get_mut(contribute);
                        if is_noise {
                            dimension = FuncCursor::new(self.func)
                                .after_inst(inst)
                                .ins()
                                .ensure_optbarrier(dimension);
                            let noise = Noise::new(
                                operator_inst,
                                cb,
                                dimension,
                                &mut ssa_builder,
                                self.func,
                            );
                            contribute.noise.push(noise)
                        } else {
                            update_optbarrier(
                                self.func,
                                &mut contribute.react,
                                |mut val, cursor| {
                                    add(cursor, &mut val, dimension, false);
                                    val
                                },
                            );
                        }
                    }
                }
                Evaluation::Equation => {
                    let eq = if is_noise {
                        ImplicitEquationKind::NoiseSrc
                    } else {
                        ImplicitEquationKind::Ddt
                    };
                    let eq = intern.implicit_equations.push_and_get_key(eq);
                    let eq_val =
                        intern.ensure_param(&mut self.func, ParamKind::ImplicitUnknown(eq));
                    let res = self.func.dfg.first_result(operator_inst);
                    self.func.dfg.replace_uses(res, eq_val);
                    let collapse =
                        ssa_builder.define_at_exit(self.func, TRUE, FALSE, operator_inst);
                    if collapse != FALSE {
                        cov_mark::hit!(collapsible_ddt);
                        debug_assert_ne!(collapse, TRUE);
                        intern
                            .outputs
                            .insert(PlaceKind::CollapseImplicitEquation(eq), collapse.into());
                    }

                    let neg_eq_val = FuncCursor::new(self.func).at_exit().ins().fneg(eq_val);
                    let contributions = if is_noise {
                        self.topology.small_signal_vals.insert(eq_val);
                        Contribution {
                            unknown: Some(eq_val),
                            resist: neg_eq_val,
                            noise: vec![Noise::new(
                                operator_inst,
                                &intern.callbacks[cb],
                                F_ONE,
                                &mut ssa_builder,
                                self.func,
                            )],
                            ..Contribution::default()
                        }
                    } else {
                        let arg0 =
                            ssa_builder.define_at_exit(self.func, F_ZERO, arg0, operator_inst);
                        Contribution {
                            unknown: Some(eq_val),
                            resist: arg0,
                            react: neg_eq_val,
                            ..Contribution::default()
                        }
                    };

                    self.topology.new_implicit_equation(eq, contributions);
                }
            }
            // not needed anymore, wipe the callback
            self.func.dfg.zap_inst(operator_inst);
            self.func.layout.remove_inst(operator_inst);
        }
    }

    pub(super) fn analog_operator_evaluations(
        &mut self,
        postdom_frontiers: &SparseBitMatrix<Block, Block>,
        intern: &mut HirInterner,
    ) -> Vec<(Inst, Evaluation)> {
        let mut analog_operators = Vec::new();

        // first iterate all analog operators and determining if they can
        // be lineraized/turned into dimensions. This step does not modify the
        // function yet as otherwise the detection may return incorrect results
        for (cb, uses) in intern.callback_uses.iter_mut_enumerated() {
            match intern.callbacks[cb] {
                CallBackKind::TimeDerivative => {
                    for inst in take(uses) {
                        if self.func.layout.inst_block(inst).is_none() {
                            continue;
                        }
                        if self.func.dfg.instr_safe_to_remove(inst)
                            || !self.op_dependent_insts.contains(inst)
                        {
                            let result = self.func.dfg.first_result(inst);
                            self.func.dfg.replace_uses(result, F_ZERO);
                            self.func.dfg.zap_inst(inst);
                            self.func.layout.remove_inst(inst);
                            continue;
                        }
                        analog_operators.push((
                            inst,
                            self.determine_evaluation(
                                false,
                                inst,
                                postdom_frontiers,
                                &intern.callbacks,
                            ),
                        ));
                    }
                }
                CallBackKind::WhiteNoise { .. }
                | CallBackKind::FlickerNoise { .. }
                | CallBackKind::NoiseTable(_) => {
                    for inst in take(uses) {
                        analog_operators.push((
                            inst,
                            self.determine_evaluation(
                                true,
                                inst,
                                postdom_frontiers,
                                &intern.callbacks,
                            ),
                        ));
                    }
                }
                _ => continue,
            }
        }
        analog_operators
    }

    fn determine_evaluation(
        &mut self,
        noise: bool,
        inst: Inst,
        postdom_frontiers: &SparseBitMatrix<Block, Block>,
        callbacks: &TiSet<FuncRef, CallBackKind>,
    ) -> Evaluation {
        let Self { func, output_values, scratch_buf, postorder, .. } = self;

        postorder.clear();
        scratch_buf.clear();
        let mut transversal =
            func.dfg.inst_uses_postorder_with(inst, (take(scratch_buf), Vec::new()), |_| true);
        postorder.extend(&mut transversal);
        *scratch_buf = transversal.visited;
        let visisted = scratch_buf;

        let is_op_dependent = |val| {
            if let Some(inst) = func.dfg.value_def(val).inst() {
                self.op_dependent_insts.contains(inst)
            } else {
                self.op_dependent_vals.contains(&val)
            }
        };

        let val_visisted =
            |val| func.dfg.value_def(val).inst().map_or(false, |inst| visisted.contains(inst));
        let mut contributes = Vec::new();
        for &inst in postorder.iter() {
            match func.dfg.insts[inst] {
                InstructionData::Binary { opcode: Opcode::Fadd | Opcode::Fsub, .. } => (),
                // for noise phis don't matter at all
                // since its a small signal value (so doesn't need to be consistently
                // maintained across multiple iterations). I am not quite sure if this
                // plays nice with transient noise and other more advanced simulation
                // types but I can't see why it wouldn't (also the language standard
                // specifically calls these small signal sources).
                InstructionData::PhiNode(_)
                | InstructionData::Branch { .. }
                | InstructionData::Binary {
                    opcode: Opcode::Flt | Opcode::Fle | Opcode::Fgt | Opcode::Fge,
                    ..
                } if noise => (),
                InstructionData::Unary { opcode: Opcode::Fneg, .. } => {}
                // noise is always zero when these are evaluated
                // TODO: complex noise power (would allow us to avoid creating an extra node here)
                InstructionData::Call { func_ref, .. }
                    if noise && callbacks[func_ref] != CallBackKind::TimeDerivative => {}
                InstructionData::Binary { opcode: Opcode::Fmul, args } => {
                    if is_op_dependent(args[0]) && is_op_dependent(args[1]) {
                        return Evaluation::Equation;
                    }
                }
                InstructionData::Binary { opcode: Opcode::Fdiv, args } => {
                    if is_op_dependent(args[1]) {
                        return Evaluation::Equation;
                    }
                }
                InstructionData::PhiNode(ref phi) => {
                    // phis are pretty complex to figure out. The most correct
                    // implementation is to check if a phi is operating point
                    // dependent. To determine that we check whether any of
                    // the control dependencies of the edge are operating point
                    // dependent.
                    //
                    // However, to avoid cerating many unnecessary implicit
                    // equiation a special optimization for chains of additions
                    // is necessary. Chains of addition/subtraction where only one
                    // summand depends on the analog operator don't need an equation.
                    // This optimizes the following (common) case:
                    //
                    // I(x) <+ ddt(foo);
                    // if (op_denpendent)
                    //    I(x) <+ bar;
                    //
                    // this will create an (op dependent) phi [ddt(foo), ddt(foo) + bar].
                    // This does not change the ddt state and therefore doesn't require
                    // introduction of state.

                    let mut op_dependent = false;
                    for (pred, _) in func.dfg.phi_edges(phi) {
                        // check if this edge is operating point dependent
                        if !op_dependent {
                            if let Some(control_deps) = postdom_frontiers.row(pred) {
                                for control_dep in control_deps.iter() {
                                    if let Some((cond, _, _)) = func
                                        .layout
                                        .block_terminator(control_dep)
                                        .and_then(|inst| func.dfg.as_branch(inst))
                                    {
                                        if is_op_dependent(cond) {
                                            op_dependent = true;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    self.val_map.clear();
                    if op_dependent
                        && phi_add_chain_start(
                            func,
                            phi.clone(),
                            &val_visisted,
                            &mut |phi, enter| {
                                if enter {
                                    self.val_map.insert(phi, F_ZERO).is_none()
                                } else {
                                    self.val_map.remove(&phi).is_some()
                                }
                            },
                        )
                        .is_none()
                    {
                        cov_mark::hit!(conditional_phi);
                        return Evaluation::Equation;
                    }
                }
                InstructionData::Unary { opcode: Opcode::OptBarrier, .. } => {
                    // if used in multiple outputs its safe to assume that this
                    // needs its own node.
                    // TODO: ignore
                    let val = func.dfg.first_result(inst);
                    let is_output = if noise {
                        self.topology.as_contribution(val).is_some()
                    } else {
                        output_values.contains(val)
                    };
                    if is_output {
                        // multiple uses of a noise source indicate
                        // correlated noise, for now just create a correlation network
                        if noise && !contributes.is_empty() {
                            return Evaluation::Equation;
                        } else if self
                            .topology
                            .as_contribution(val)
                            .map_or(false, |it| !it.is_reactive())
                        {
                            contributes.push((val, F_ZERO))
                        } else {
                            return Evaluation::Equation;
                        }
                    }
                }
                _ => {
                    return Evaluation::Equation;
                }
            }
        }
        if contributes.is_empty() {
            assert!(noise, "ddt should have been deadcode eliminated");
            return Evaluation::Dead;
        }
        self.create_dimension(
            if noise { F_ONE } else { self.func.dfg.instr_args(inst)[0] },
            self.func.dfg.first_result(inst),
        );
        for (contrib, dim) in &mut contributes {
            *dim = self.val_map[&*contrib];
        }
        Evaluation::Linear { contributes: contributes.into_boxed_slice() }
    }
}

fn phi_add_chain_start(
    func: &Function,
    phi: PhiNode,
    val_visited: &impl Fn(Value) -> bool,
    handle_loops: &mut impl FnMut(Value, bool) -> bool,
) -> Option<Value> {
    let mut add_chain_start = None;
    for (_, mut edge) in func.dfg.phi_edges(&phi) {
        if !val_visited(edge) {
            return None;
        }
        edge = follow_add_chain(func, edge, val_visited, &mut *handle_loops);
        match add_chain_start {
            Some(chain_start) => {
                if chain_start != edge {
                    return None;
                }
            }
            None => add_chain_start = Some(edge),
        }
    }
    add_chain_start
}

fn follow_add_chain(
    func: &Function,
    mut val: Value,
    val_visited: &impl Fn(Value) -> bool,
    handle_loops: &mut impl FnMut(Value, bool) -> bool,
) -> Value {
    while let Some(inst) = func.dfg.value_def(val).inst() {
        match func.dfg.insts[inst] {
            InstructionData::Binary { opcode: Opcode::Fadd, args: [lhs, rhs] } => {
                if !val_visited(lhs) {
                    val = rhs;
                    continue;
                }
                if !val_visited(rhs) {
                    val = lhs;
                    continue;
                }
            }
            InstructionData::Binary { opcode: Opcode::Fsub, args: [lhs, rhs] } => {
                if !val_visited(rhs) {
                    val = lhs;
                    continue;
                }
            }
            InstructionData::PhiNode(ref phi) => {
                if handle_loops(val, true) {
                    let add_chain_start =
                        phi_add_chain_start(func, phi.clone(), val_visited, &mut *handle_loops);
                    handle_loops(val, false);
                    if let Some(add_chain_start) = add_chain_start {
                        val = add_chain_start;
                        continue;
                    }
                }
            }
            _ => (),
        }
        break;
    }
    val
}
