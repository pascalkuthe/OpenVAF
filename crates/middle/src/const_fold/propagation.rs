/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::cfg::{
    BasicBlock, ControlFlowGraph, ModificationPass, Phi, PhiData, Terminator, TerminatorKind,
};
use crate::const_fold::{CallResolver, ConstantFold, NoInputConstResolution};
use crate::dfa::lattice::{FlatSet, JoinSemiLattice, SparseFlatSetMap};
use crate::dfa::visitor::ResultsVisitorMut;
use crate::dfa::{direciton, Analysis, AnalysisDomain, Results, SplitEdgeEffects};
use crate::osdi_types::SimpleConstVal::Bool;
use crate::{
    dfa, CfgFunctions, CfgInputs, ConstVal, Expression, Local, LocalKind, Operand, OperandData,
    RValue, StmntKind,
};
use crate::{impl_pass_span, StatementId, SyntaxCtx};
use data_structures::bit_set::HybridBitSet;
use osdi_types::ConstVal::Scalar;
use osdi_types::Type;
use std::cell::UnsafeCell;
use std::marker::PhantomData;
use tracing::trace_span;

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlockConstants {
    pub reachable: bool,
    pub constants: SparseFlatSetMap<Local, ConstVal>,
    temporaries_changed: bool,
}

impl JoinSemiLattice for BasicBlockConstants {
    fn join(&mut self, other: &Self) -> bool {
        debug_assert!(other.reachable);
        self.temporaries_changed.join(&other.temporaries_changed)
            | self.reachable.join(&true)
            | self.constants.join(&other.constants)
    }
}

impl BasicBlockConstants {
    pub fn get_folded_val(&self, local: Local) -> Option<ConstVal> {
        self.constants.element_sets.get(&local).cloned()
    }
}

pub struct ConditionalConstPropagation<'lt, R: CallResolver>(
    ConditionalConstantPropagationImpl<'lt, R>,
);

impl<'lt, R: CallResolver> From<ConditionalConstantPropagationImpl<'lt, R>>
    for ConditionalConstPropagation<'lt, R>
{
    fn from(inner: ConditionalConstantPropagationImpl<'lt, R>) -> Self {
        Self(inner)
    }
}

use __sealed::ConditionalConstantPropagationImpl;

mod __sealed {
    use super::*;

    pub struct ConditionalConstantPropagationImpl<'lt, R: CallResolver> {
        pub resolver: &'lt R,
        pub global_vars: HybridBitSet<Local>,
        constant_temporaries: UnsafeCell<SparseFlatSetMap<Local, ConstVal>>,
    }

    impl<'lt, R: CallResolver> ConditionalConstantPropagationImpl<'lt, R> {
        pub fn new(resolver: &'lt R, global_vars: HybridBitSet<Local>, local_cnt: usize) -> Self {
            Self {
                resolver,
                global_vars,
                constant_temporaries: UnsafeCell::new(SparseFlatSetMap::new_empty(local_cnt)),
            }
        }

        pub(super) fn get_temporary_constant(&self, local: Local) -> FlatSet<ConstVal> {
            // Access to const_temporaries is save here since the reference immediately and constant_temporaries is sealed within this module so no other references can exist
            unsafe { &mut *self.constant_temporaries.get() }.get_cloned_flat_set(local)
        }

        pub(super) fn get_folded_temporary(&self, local: Local) -> Option<ConstVal> {
            // Access to const_temporaries is save here since the reference immediately and constant_temporaries is sealed within this module so no other references can exist
            unsafe { &mut *self.constant_temporaries.get() }.element_sets.get(&local).cloned()
        }

        pub(super) fn extend_with_temporary_constants(
            &self,
            dst: &mut SparseFlatSetMap<Local, ConstVal>,
        ) {
            // Access to const_temporaries is save here since the reference immediately and constant_temporaries is sealed within this module so no other references can exist
            dst.element_sets.extend(
                unsafe { &*self.constant_temporaries.get() }
                    .element_sets
                    .iter()
                    .map(|(k, v)| (*k, v.clone())),
            );
            dst.top_sets.union(&unsafe { &*self.constant_temporaries.get() }.top_sets);
        }

        pub(super) fn write_constant_to_local(
            &self,
            cfg: &ControlFlowGraph<R::C>,
            state: &mut BasicBlockConstants,
            dst: Local,
            val: FlatSet<ConstVal>,
        ) {
            if LocalKind::Temporary == cfg.locals[dst].kind {
                // Constant temporaries changed if the temporary was previously unknown
                // Access to const_temporaries is save here since the reference is dropped immediately and constant_temporaries is sealed within this module so no other references can exist
                state.temporaries_changed =
                    unsafe { &mut *self.constant_temporaries.get() }.set_flat_set(dst, val)
            } else {
                state.constants.set_flat_set(dst, val);
            }
        }
    }
}

impl<'lt, R: CallResolver> AnalysisDomain<R::C> for ConditionalConstPropagation<'lt, R> {
    type Domain = BasicBlockConstants;
    type Direction = direciton::Forward;
    const NAME: &'static str = "Const Propagation";

    fn bottom_value(&self, cfg: &ControlFlowGraph<<R as CallResolver>::C>) -> Self::Domain {
        self.0.bottom_value(cfg)
    }

    fn initialize_start_block(
        &self,
        cfg: &ControlFlowGraph<<R as CallResolver>::C>,
        state: &mut Self::Domain,
    ) {
        self.0.initialize_start_block(cfg, state)
    }
}

impl<'lt, R: CallResolver> AnalysisDomain<R::C> for ConditionalConstantPropagationImpl<'lt, R> {
    type Domain = BasicBlockConstants;
    type Direction = direciton::Forward;
    const NAME: &'static str = "Const Propagation";

    fn bottom_value(&self, cfg: &ControlFlowGraph<<R as CallResolver>::C>) -> Self::Domain {
        BasicBlockConstants {
            reachable: false,
            temporaries_changed: false,
            constants: SparseFlatSetMap::new_empty(cfg.locals.len()),
        }
    }

    fn initialize_start_block(
        &self,
        _cfg: &ControlFlowGraph<<R as CallResolver>::C>,
        state: &mut Self::Domain,
    ) {
        state.reachable = true;
        state.constants.top_sets.union(&self.global_vars);
    }
}

impl<'lt, R: CallResolver> Analysis<R::C> for ConditionalConstPropagation<'lt, R> {
    fn init_block(&self, _cfg: &ControlFlowGraph<R::C>, state: &mut Self::Domain) {
        state.temporaries_changed = false;
        self.0.extend_with_temporary_constants(&mut state.constants);
    }

    fn apply_phi_effect(
        &self,
        cfg: &ControlFlowGraph<R::C>,
        state: &mut Self::Domain,
        phi: &PhiData,
        bb: BasicBlock,
        idx: Phi,
    ) {
        self.0.apply_phi_effect(cfg, state, phi, bb, idx)
    }

    fn apply_statement_effect(
        &self,
        cfg: &ControlFlowGraph<R::C>,
        state: &mut Self::Domain,
        statement: &(StmntKind<<R as CallResolver>::C>, SyntaxCtx),
        idx: StatementId,
        bb: BasicBlock,
    ) {
        self.0.apply_statement_effect(cfg, state, statement, idx, bb)
    }
}

impl<'lt, R: CallResolver> Analysis<R::C> for ConditionalConstantPropagationImpl<'lt, R> {
    fn init_block(&self, _cfg: &ControlFlowGraph<R::C>, state: &mut Self::Domain) {
        state.temporaries_changed = false;
    }

    fn apply_phi_effect(
        &self,
        cfg: &ControlFlowGraph<R::C>,
        state: &mut Self::Domain,
        phi: &PhiData,
        _bb: BasicBlock,
        _idx: Phi,
    ) {
        let flat_set = phi.sources.iter().fold(FlatSet::Bottom, |mut dst, (_, local)| {
            match self.get_temporary_constant(*local) {
                FlatSet::Bottom => state.constants.join_into(*local, &mut dst),
                res => dst.join(&res),
            };
            dst
        });

        self.write_constant_to_local(cfg, state, phi.dst, flat_set)
    }

    fn apply_statement_effect(
        &self,
        cfg: &ControlFlowGraph<R::C>,
        state: &mut Self::Domain,
        statement: &(StmntKind<<R as CallResolver>::C>, SyntaxCtx),
        _idx: StatementId,
        _bb: BasicBlock,
    ) {
        if let StmntKind::Assignment(dst, ref val) = statement.0 {
            let val = ConstantFold {
                locals: &state.constants,
                resolver: self.resolver,
                resolve_special_locals: |temporary| self.get_temporary_constant(temporary),
            }
            .resolve_rvalue(val, cfg.locals[dst].ty);
            self.write_constant_to_local(cfg, state, dst, val)
        }
    }

    #[inline(always)]
    fn apply_edge_effects(
        &self,
        _cfg: &ControlFlowGraph<R::C>,
        _block: BasicBlock,
        state: &Self::Domain,
    ) -> bool {
        state.reachable
    }

    fn apply_split_edge_effects(
        &self,
        _cfg: &ControlFlowGraph<R::C>,
        _block: BasicBlock,
        discr: &RValue<R::C>,
        state: &Self::Domain,
        edge_effects: &mut impl SplitEdgeEffects<Self::Domain>,
    ) {
        // only propagate along reachable paths
        if !state.reachable {
            edge_effects.apply(|_, _, _| false)
        } else {
            let mut fold = ConstantFold {
                locals: &state.constants,
                resolver: self.resolver,
                resolve_special_locals: |temporary| self.get_temporary_constant(temporary),
            };
            if let FlatSet::Elem(Scalar(Bool(const_discriminant))) =
                fold.resolve_rvalue(discr, Type::BOOL)
            {
                // dont propagate reachable into the edge that can not be reached from this block
                edge_effects.apply(|_, _, switch_edge| switch_edge == const_discriminant)
            }
        }
    }
}

impl<C: CfgFunctions> ControlFlowGraph<C> {
    /// This function runs a conditional constant folding algorithm.
    /// The resulting data flow graph is returned without modifying the CFG itself.
    /// This function is rarely used on its own. Use [`propagate_constants`]/[`propagate_constants_with_inputs`] instead
    ///
    ///
    /// # Parameters
    ///
    /// * `input` -
    /// Generally inputs that are always known to be constants should just be emitted as such during HIR lowering.
    /// This parameter is mostly useful when creating multiple cfgs from the same code and constant folding different sets of inputs
    ///
    ///
    /// # Note
    ///
    /// Call very cautiously this is the single most expensive operation in the compiler. Preferably this should be done only after simpler more efficent optimizations have been performed.
    ///
    /// # Returns
    ///
    /// [`Results`](create::dfa::Results) of condtional const propagation: The constants at the beginning of each block and whether a block is rechable.
    /// See documentation of [`Results`](create::dfa::Results) on how to browse these results.
    ///
    /// However he most common use case is to pass the result to [`write_constants`] which writes the constants back into the CFG
    ///
    pub fn conditional_constant_propagation<'a, R: CallResolver<C = C>>(
        &self,
        resolver: &'a R,
        global_vars: HybridBitSet<Local>,
    ) -> dfa::Results<C, ConditionalConstPropagation<'a, R>> {
        let span = trace_span!("fold_constants");
        let _enter = span.enter();

        let res = ConditionalConstantPropagationImpl::new(resolver, global_vars, self.locals.len())
            .into_engine(self)
            .iterate_to_fixpoint();

        Results { analysis: res.analysis.into(), entry_sets: res.entry_sets }
    }

    pub fn write_constants<R: CallResolver<C = C>>(
        &mut self,
        const_prop_result: &dfa::Results<C, ConditionalConstPropagation<R>>,
    ) {
        const_prop_result.visit_with_mut(self, &mut ConstWriter(&const_prop_result.analysis.0))
    }
}

impl<C: CfgFunctions> Expression<C> {
    pub fn const_eval(&self) -> Option<ConstVal> {
        self.const_eval_with_inputs(&NoInputConstResolution(PhantomData), HybridBitSet::new_empty())
    }

    pub fn const_eval_with_inputs<R: CallResolver<C = C>>(
        &self,
        inputs: &R,
        global_vars: HybridBitSet<Local>,
    ) -> Option<ConstVal> {
        match self.1.contents {
            OperandData::Constant(ref val) => Some(val.clone()),
            OperandData::Copy(local) => {
                let mut res = self
                    .0
                    .conditional_constant_propagation(inputs, global_vars)
                    .into_results_cursor(&self.0);
                res.seek_to_exit_block_end(&self.0);
                res.get().get_folded_val(local)
            }
            OperandData::Read(ref input) => inputs.resolve_input(input).into(),
        }
    }
}

pub struct ConstantPropagation<'a, R> {
    resolver: &'a R,
    global_vars: HybridBitSet<Local>,
}

impl<'a, C: CfgFunctions, R: CallResolver<C = C>> ModificationPass<'_, C>
    for ConstantPropagation<'a, R>
{
    type Result = ();
    impl_pass_span!(self; "constant_propagation", input_resolver = debug(self.resolver));

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let res = cfg.conditional_constant_propagation(self.resolver, self.global_vars);
        cfg.write_constants(&res)
    }
}
impl<C: CfgFunctions> Default for ConstantPropagation<'static, NoInputConstResolution<C>> {
    fn default() -> Self {
        Self {
            resolver: &NoInputConstResolution(PhantomData),
            global_vars: HybridBitSet::new_empty(),
        }
    }
}

impl<'a, R: CallResolver> ConstantPropagation<'a, R> {
    pub fn with_resolver(resolver: &'a R) -> Self {
        Self { resolver, global_vars: HybridBitSet::new_empty() }
    }
}

impl<C: CfgFunctions> ConstantPropagation<'static, NoInputConstResolution<C>> {
    pub fn with_initial_conditions(global_vars: HybridBitSet<Local>) -> Self {
        Self { resolver: &NoInputConstResolution(PhantomData), global_vars }
    }
}

struct ConstWriter<'a, 'b, R: CallResolver>(&'a ConditionalConstantPropagationImpl<'b, R>);

impl<'a, 'b, R: CallResolver> ResultsVisitorMut<R::C> for ConstWriter<'a, 'b, R> {
    type FlowState = BasicBlockConstants;

    fn visit_statement_after_effect(
        &mut self,
        state: &Self::FlowState,
        stmnt: &mut (StmntKind<R::C>, SyntaxCtx),
        _block: BasicBlock,
        _id: StatementId,
    ) {
        match stmnt.0 {
            StmntKind::Assignment(dst, ref mut rval) => {
                let val = self.0.get_folded_temporary(dst).or_else(|| state.get_folded_val(dst));

                write_consts_to_rval(state, val, rval)
            }
            StmntKind::Call(_, ref mut args, _) => write_consts_to_operands(state, args),
            StmntKind::NoOp => {}
        }
    }

    #[inline(always)]
    fn visit_terminator_before_effect(
        &self,
        state: &Self::FlowState,
        term: &mut Terminator<R::C>,
        _block: BasicBlock,
    ) {
        if let TerminatorKind::Split { ref mut condition, .. } = term.kind {
            let mut fold = ConstantFold {
                locals: &state.constants,
                resolver: self.0.resolver,
                resolve_special_locals: |temporary| self.0.get_temporary_constant(temporary),
            };
            let val = fold.resolve_rvalue(condition, Type::BOOL).into_option();
            write_consts_to_rval(state, val, condition);
        }
    }
}

fn write_consts_to_rval<C: CfgFunctions>(
    state: &BasicBlockConstants,
    folded_val: Option<ConstVal>,
    dst: &mut RValue<C>,
) {
    if let Some(folded_val) = folded_val {
        *dst =
            RValue::Use(Operand { span: dst.span(), contents: OperandData::Constant(folded_val) });
    } else {
        write_consts_to_rval_operands(state, dst)
    }
}

fn write_consts_to_rval_operands<C: CfgFunctions>(
    state: &BasicBlockConstants,
    dst: &mut RValue<C>,
) {
    write_consts_to_operands(state, dst.operands_mut())
}

fn write_consts_to_operands<'c, I: CfgInputs + 'c>(
    state: &BasicBlockConstants,
    operands: impl IntoIterator<Item = &'c mut Operand<I>>,
) {
    for operand in operands {
        if let OperandData::Copy(local) = operand.contents {
            if let Some(folded_val) = state.get_folded_val(local) {
                operand.contents = OperandData::Constant(folded_val.clone())
            }
        }
    }
}
