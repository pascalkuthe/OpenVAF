/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::{FindAssignments, ReachingDefinitionsAnalysis};
use openvaf_data_structures::bit_set::{BitSet, SparseBitMatrix};
use openvaf_ir::ids::{StatementId, SyntaxCtx};
use openvaf_middle::cfg::{
    AnalysisPass, BasicBlock, ControlFlowGraph, IntLocation, InternedLocations, Phi, PhiData,
    Terminator, TerminatorKind,
};
use openvaf_middle::dfa::ResultsVisitor;
use openvaf_middle::{dfa, impl_pass_span, CallType, Local, StmntKind};
use std::borrow::Borrow;
use std::fmt;
use std::fmt::{Debug, Formatter};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DefUserGraph(pub SparseBitMatrix<IntLocation, IntLocation>);

#[derive(Clone, Eq, PartialEq)]
pub struct UseDefGraph(pub SparseBitMatrix<IntLocation, IntLocation>);

impl Debug for UseDefGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl UseDefGraph {
    pub fn inverse(&self) -> DefUserGraph {
        DefUserGraph(self.0.inverse())
    }
}

pub struct CalculateReachingDefinitions;

pub struct BuildUseDefGraph<'a, R, A> {
    pub locations: &'a InternedLocations,
    pub assignments: A,
    pub reaching_definitions: R,
}

impl<'a, C> AnalysisPass<'_, C>
    for BuildUseDefGraph<'a, CalculateReachingDefinitions, FindAssignments<'a>>
where
    C: CallType,
{
    type Result = UseDefGraph;
    impl_pass_span!("Use-Def Graph Construction");

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let assignments = cfg.analyse(self.assignments);

        let reaching_definitions = cfg.analyse(ReachingDefinitionsAnalysis {
            assignments: &assignments,
            locations: &self.locations,
        });

        cfg.analyse(BuildUseDefGraph {
            locations: self.locations,
            assignments: &assignments,
            reaching_definitions,
        })
    }
}

impl<'a, C, A> AnalysisPass<'_, C> for BuildUseDefGraph<'a, CalculateReachingDefinitions, A>
where
    C: CallType,
    A: Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    type Result = UseDefGraph;
    impl_pass_span!("Use-Def Graph Construction");

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let reaching_definitions = cfg.analyse(ReachingDefinitionsAnalysis {
            assignments: self.assignments.borrow(),
            locations: &self.locations,
        });

        cfg.analyse(BuildUseDefGraph {
            locations: self.locations,
            assignments: self.assignments.borrow(),
            reaching_definitions,
        })
    }
}

impl<'a, 'b, C, A, R> AnalysisPass<'_, C> for BuildUseDefGraph<'a, R, A>
where
    C: CallType,
    A: Borrow<SparseBitMatrix<Local, IntLocation>>,
    R: Borrow<dfa::GenKillResults<C, ReachingDefinitionsAnalysis<'b, A>>>,
{
    type Result = UseDefGraph;
    impl_pass_span!("Use-Def Graph Construction");

    fn run(self, cfg: &ControlFlowGraph<C>) -> Self::Result {
        let Self {
            locations,
            assignments,
            reaching_definitions,
        } = self;

        let mut visistor = UseDefBuilder {
            dst: SparseBitMatrix::new(locations.len(), locations.len()),
            tmp: BitSet::new_empty(locations.len()),
            assignments: assignments.borrow(),
            locations,
        };

        reaching_definitions.borrow().visit_with(cfg, &mut visistor);

        UseDefGraph(visistor.dst)
    }
}

struct UseDefBuilder<'a> {
    dst: SparseBitMatrix<IntLocation, IntLocation>,
    tmp: BitSet<IntLocation>,
    assignments: &'a SparseBitMatrix<Local, IntLocation>,
    locations: &'a InternedLocations,
}

impl<'a> UseDefBuilder<'a> {
    fn visit_rhs(
        &mut self,
        location: IntLocation,
        locals: impl IntoIterator<Item = Local>,
        reaching_definitions: &BitSet<IntLocation>,
    ) {
        self.tmp.clear();
        for local in locals {
            //panic!("{}: {:?} from {:?}", location, local, reaching_definitions);
            if let Some(row) = self.assignments.row(local) {
                self.tmp.union(row);
            }
        }
        self.tmp.intersect(reaching_definitions);
        *self.dst.ensure_row(location) = self.tmp.to_hybrid()
    }
}

impl<'a, C: CallType> ResultsVisitor<C> for UseDefBuilder<'a> {
    type FlowState = BitSet<IntLocation>;

    #[inline]
    fn visit_phi_before_effect(
        &mut self,
        state: &Self::FlowState,
        phi: &PhiData,
        block: BasicBlock,
        id: Phi,
    ) {
        self.visit_rhs(
            self.locations[block].phi_start + id.index(),
            phi.sources.values().copied(),
            state,
        )
    }

    #[inline]
    fn visit_statement_before_effect(
        &mut self,
        state: &Self::FlowState,
        stmnt: &(StmntKind<C>, SyntaxCtx),
        block: BasicBlock,
        id: StatementId,
    ) {
        self.visit_rhs(
            self.locations[block].stmnt_start + id.index(),
            stmnt.0.read_locals(),
            state,
        )
    }

    #[inline]
    fn visit_terminator_before_effect(
        &mut self,
        state: &Self::FlowState,
        term: &Terminator<C>,
        block: BasicBlock,
    ) {
        if let TerminatorKind::Split { ref condition, .. } = term.kind {
            self.visit_rhs(self.locations[block].terminator, condition.locals(), state)
        }
    }
}
