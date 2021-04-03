/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use openvaf_data_structures::index_vec::IndexVec;

use crate::cfg::{BasicBlock, CfgPass, ControlFlowGraph, TerminatorKind};
use crate::const_fold::{CallResolver, ConstantFold, DiamondLattice, NoInputConstResolution};
use crate::dfa::{Analysis, DfGraph, Engine, Forward};
use crate::impl_pass_span;
use crate::{
    fold_rvalue, CallType, ConstVal, Expression, Local, Operand, OperandData, RValue, StmntKind,
};
use openvaf_data_structures::{BitSet, HashMap};
use osdi_types::ConstVal::Scalar;
use osdi_types::SimpleConstVal::Integer;
use osdi_types::Type;
use std::marker::PhantomData;
use tracing::trace_span;

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlockConstants {
    pub unreachable: bool,
    pub constants: HashMap<Local, ConstVal>,
    pub not_a_constant: BitSet<Local>,
}

impl BasicBlockConstants {
    pub fn meet(&mut self, other: &Self) {
        self.not_a_constant.union_with(&other.not_a_constant);
        let not_a_constant = &mut self.not_a_constant;
        self.constants.retain(|local, val| {
            let other = other.constants.get(local);
            match other {
                Some(other) if other != val => {
                    not_a_constant.insert(*local);
                    false
                }
                Some(_) | None => true,
            }
        });
    }

    pub fn write_lattice(&mut self, local: Local, lattice: DiamondLattice) {
        match lattice {
            DiamondLattice::NotAConstant => {
                self.constants.remove(&local);
            }
            DiamondLattice::Val(val) => {
                self.constants.insert(local, val);
                self.not_a_constant.remove(local);
            }
            DiamondLattice::Unknown => {
                self.constants.remove(&local);
                self.not_a_constant.remove(local);
            }
        }
    }
}

pub struct ConditionalConstantPropagation<'lt, R: CallResolver> {
    pub cfg: &'lt ControlFlowGraph<R::C>,
    pub resolver: &'lt R,
}

impl<'lt, R: CallResolver> Analysis<R::C> for ConditionalConstantPropagation<'lt, R> {
    type Set = BasicBlockConstants;
    type Direction = Forward;

    fn transfer_function(
        &mut self,
        in_set: &Self::Set,
        out_set: &mut Self::Set,
        basic_bock: BasicBlock,
        cfg: &ControlFlowGraph<R::C>,
    ) {
        if in_set.unreachable {
            out_set.unreachable = true
        } else {
            out_set.not_a_constant.clear();
            out_set.not_a_constant.union_with(&in_set.not_a_constant);
            out_set.constants.clear();
            out_set.constants.extend(
                in_set
                    .constants
                    .iter()
                    .map(|(local, val)| (*local, val.clone())),
            );

            for phi in &self.cfg[basic_bock].phi_statements {
                let mut lattice = DiamondLattice::Unknown;
                for (_, src) in phi.sources.iter() {
                    if out_set.not_a_constant.contains(*src) {
                        lattice = DiamondLattice::NotAConstant;
                    } else if let Some(constant) = in_set.constants.get(src) {
                        lattice.meet_constant(&constant)
                    }
                }
                out_set.write_lattice(phi.dst, lattice);
            }

            out_set.unreachable = false;

            let mut fold = ConstantFold {
                locals: out_set,
                resolver: self.resolver,
            };

            for stmt in &cfg[basic_bock].statements {
                if let StmntKind::Assignment(dst, ref val) = stmt.0 {
                    fold.resolve_statement(dst, val, self.cfg.locals[dst].ty);
                }
            }
        }
    }

    fn join(&mut self, src_bb: BasicBlock, dst_bb: BasicBlock, graph: &mut DfGraph<Self::Set>) {
        if !graph.out_sets[src_bb].unreachable {
            // try to constant fold terminator and mark as unreachable
            if let TerminatorKind::Split {
                ref condition,
                false_block,
                ..
            } = self.cfg[src_bb].terminator().kind
            {
                let mut fold = ConstantFold {
                    locals: &graph.out_sets[src_bb],
                    resolver: self.resolver,
                };

                if let DiamondLattice::Val(val) = fold_rvalue(&mut fold, condition, Type::INT) {
                    if (val == Scalar(Integer(0))) != (dst_bb == false_block) {
                        // unreachable don't propagate constants
                        return;
                    }
                }
            }

            graph.in_sets[dst_bb].unreachable = false;
            graph.in_sets[dst_bb].meet(&graph.out_sets[src_bb]);
        }
    }

    fn new_set(&self) -> Self::Set {
        BasicBlockConstants {
            unreachable: true,
            constants: HashMap::new(),
            not_a_constant: BitSet::new_empty(self.cfg.locals.len_idx()),
        }
    }

    fn setup_entry(&mut self, block: BasicBlock, graph: &mut DfGraph<Self::Set>) {
        graph.in_sets[block].unreachable = false
    }
}

impl<C: CallType> ControlFlowGraph<C> {
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
    /// Call very cautiously this is the single most expensive operation in the compiler
    /// (this can dwarf lexing + preprocessing + parsing + ast_lowering)
    ///
    /// # Returns
    ///
    /// The `in_sets` of the returned Graph specify all constants known at the beginning of the block.
    /// Furthermore, it specifies whether there is any program path that allows this block to be reached.
    /// The `out_sets` contain similar information however they described the end of the basic block instead
    /// which tends to be less useful.
    ///
    /// The most common use case is to pass the `in_set` to [`write_constants`] together with **the same** ìnputs`
    ///
    pub fn fold_constants<R: CallResolver<C = C>>(
        &self,
        resolver: &R,
    ) -> DfGraph<BasicBlockConstants> {
        let span = trace_span!("fold_constants");
        let _enter = span.enter();

        Engine::new(
            self,
            &mut ConditionalConstantPropagation {
                cfg: self,
                resolver,
            },
        )
        .iterate_to_fixpoint()
    }

    /// Replaces [values](`crate::RValue`) with their constant values obtained from `block_constants` and `inputs` ( See [`fold_constants`]).
    /// This function also removes branches which are known to only have one possible path after const propagation
    pub fn write_constants<R: CallResolver<C = C>>(
        &mut self,
        block_constants: IndexVec<BasicBlock, BasicBlockConstants>,
        inputs: &R,
    ) {
        let span = trace_span!("write_constants");
        let _enter = span.enter();

        for (bb, mut local_constants) in block_constants.into_iter_enumerated() {
            if local_constants.unreachable {
                continue;
            }

            for phi in &self[bb].phi_statements {
                let mut lattice = DiamondLattice::Unknown;
                for (_, src) in phi.sources.iter() {
                    if local_constants.not_a_constant.contains(*src) {
                        lattice = DiamondLattice::NotAConstant;
                    } else if let Some(constant) = local_constants.constants.get(src) {
                        lattice.meet_constant(&constant)
                    }
                }
                local_constants.write_lattice(phi.dst, lattice);
            }

            let mut fold = ConstantFold {
                locals: &mut local_constants,
                resolver: inputs,
            };

            for (stmt, _) in &mut self.blocks[bb].statements {
                if let StmntKind::Assignment(dst, ref mut rval) = *stmt {
                    if let DiamondLattice::Val(val) =
                        fold.resolve_statement(dst, rval, self.locals[dst].ty)
                    {
                        // In most cases the statement is not required anymore (allowing this to be turned into a noop)
                        // but phis make that impossible
                        *rval = RValue::Use(Operand {
                            span: rval.span(),
                            contents: OperandData::Constant(val),
                        })
                    } else {
                        for operand in rval.operands_mut() {
                            if let DiamondLattice::Val(c) = fold.resolve_operand(operand) {
                                operand.contents = OperandData::Constant(c)
                            }
                        }
                    }
                }
            }

            if let TerminatorKind::Split {
                ref mut condition, ..
            } = self[bb].terminator_mut().kind
            {
                if let DiamondLattice::Val(cond) = fold_rvalue(&mut fold, condition, Type::INT) {
                    // Branches are collapse in simplify cfg pass
                    *condition =
                        RValue::Use(Operand::new(OperandData::Constant(cond), condition.span()))
                } else {
                    for operand in condition.operands_mut() {
                        if let DiamondLattice::Val(c) = fold.resolve_operand(operand) {
                            operand.contents = OperandData::Constant(c)
                        }
                    }
                }
            }
        }
    }
}

impl<C: CallType> Expression<C> {
    pub fn const_eval(&self) -> Option<ConstVal> {
        self.const_eval_with_inputs(&NoInputConstResolution(PhantomData))
    }

    pub fn const_eval_with_inputs<R: CallResolver<C = C>>(&self, inputs: &R) -> Option<ConstVal> {
        match self.1.contents {
            OperandData::Constant(ref val) => Some(val.clone()),
            OperandData::Copy(local) => self.0.fold_constants(inputs).out_sets[self.0.end()]
                .constants
                .remove(&local),
            OperandData::Read(ref input) => inputs.resolve_input(input).into(),
        }
    }
}

pub struct ConstantPropagation<'a, R>(&'a R);

impl<'a, C: CallType, R: CallResolver<C = C>> CfgPass<'_, C> for ConstantPropagation<'a, R> {
    type Result = ();

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let local_constants = cfg.fold_constants(self.0).in_sets;
        cfg.write_constants(local_constants, self.0)
    }

    impl_pass_span!(self; "constant_propagation", input_resolver = debug(self.0));
}
impl<C: CallType> Default for ConstantPropagation<'static, NoInputConstResolution<C>> {
    fn default() -> Self {
        Self(&NoInputConstResolution(PhantomData))
    }
}

impl<'a, R: CallResolver> ConstantPropagation<'a, R> {
    pub fn with_resolver(r: &'a R) -> Self {
        Self(r)
    }
}
