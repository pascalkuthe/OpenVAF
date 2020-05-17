//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::data_flow::framework::{
    Analysis, DataFlowGraph, Engine, ForwardEngine, GenKillAnalysis, GenKillEngine, GenKillSet,
};
use crate::analysis::ExtractionDependencyHandler;
use crate::compact_arena::{invariant_lifetime, TinyHeapArena};
use crate::ir::hir::DisciplineAccess;
use crate::ir::mir::control_flow_graph::{BasicBlockId, Terminator};
use crate::ir::mir::ControlFlowGraph;
use crate::ir::{BranchId, ParameterId, SafeRangeCreation, StatementId, VariableId};
use crate::mir::Mir;
use crate::mir::Statement;
use crate::symbol::Ident;
use fixedbitset::FixedBitSet as BitSet;

pub struct UseDefGraph<'mir, 'cfg> {
    pub uses: TinyHeapArena<'mir, DefiningSet>,
    pub terminator_uses: TinyHeapArena<'cfg, DefiningSet>,
    pub variables: TinyHeapArena<'mir, DefiningSet>,
    pub statement_count: u16,
}

impl<'mir, 'cfg> UseDefGraph<'mir, 'cfg> {
    pub fn total_def_count(&self) -> u16 {
        self.statement_count
    }
    pub fn new(mir: &Mir<'mir>, cfg: &ControlFlowGraph<'cfg, 'mir>) -> Self {
        let statement_count = SafeRangeCreation::<StatementId>::full_range(mir)
            .unwrap()
            .len();

        let var_count = SafeRangeCreation::<VariableId>::full_range(mir)
            .unwrap()
            .len();

        let base_set = BitSet::with_capacity(statement_count as usize);
        unsafe {
            // This iss save since we are creating associated data
            Self {
                uses: TinyHeapArena::new_with(invariant_lifetime(), statement_count, || {
                    DefiningSet(base_set.clone())
                }),

                variables: TinyHeapArena::new_with(invariant_lifetime(), var_count, || {
                    DefiningSet(base_set.clone())
                }),

                terminator_uses: TinyHeapArena::new_with_default_values(
                    invariant_lifetime(),
                    cfg.block_count(),
                ),

                statement_count,
            }
        }
    }
    pub fn clone_for_subcfg<'newtag>(
        &self,
        _: &ControlFlowGraph<'mir, 'newtag>,
    ) -> UseDefGraph<'mir, 'newtag> {
        let mut uses = unsafe { TinyHeapArena::new(invariant_lifetime(), self.uses.len()) };
        self.uses.clone_into(&mut uses);
        let mut terminator_uses =
            unsafe { TinyHeapArena::new(invariant_lifetime(), self.terminator_uses.len()) };
        self.terminator_uses.clone_into(&mut terminator_uses);
        let mut variables =
            unsafe { TinyHeapArena::new(invariant_lifetime(), self.variables.len()) };
        self.variables.clone_into(&mut variables);
        UseDefGraph {
            uses,
            terminator_uses,
            variables,
            statement_count: self.statement_count,
        }
    }

    pub fn reborrow_for_subcfg<'newtag, 'lt>(
        &'lt self,
        cfg: &ControlFlowGraph<'newtag, 'mir>,
    ) -> &'lt UseDefGraph<'mir, 'newtag> {
        assert!(self.terminator_uses.len() <= cfg.block_count());
        unsafe { std::mem::transmute(self) }
    }

    pub fn get_assignments(&self, var: VariableId<'mir>) -> &DefiningSet {
        &self.variables[var.unwrap()]
    }

    pub fn get_reachable_definitions(&self, stmt: StatementId<'mir>) -> &DefiningSet {
        &self.uses[stmt.unwrap()]
    }

    fn get_assignments_mut(&mut self, var: VariableId<'mir>) -> &mut DefiningSet {
        &mut self.variables[var.unwrap()]
    }

    fn get_reachable_definitions_mut(&mut self, stmt: StatementId<'mir>) -> &mut DefiningSet {
        &mut self.uses[stmt.unwrap()]
    }
}

// This is just a thing wrapper around BitSet taking care of Id conversions
// For operations that don't require conversion just use the bitset directly
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Clone)]
pub struct DefiningSet(pub BitSet);

impl DefiningSet {
    pub fn contains<'mir>(&self, id: StatementId) -> bool {
        self.0.contains(id.as_usize())
    }

    pub fn insert<'mir>(&mut self, id: StatementId) {
        self.0.insert(id.unwrap().index() as usize)
    }

    pub fn put(&mut self, id: StatementId) -> bool {
        self.0.put(id.unwrap().index() as usize)
    }

    pub fn set(&mut self, id: StatementId, value: bool) {
        self.0.set(id.as_usize(), value)
    }
}

pub struct ReachableDefinitionsAnalysis<'lt, 'mir, 'cfg> {
    graph: UseDefGraph<'mir, 'cfg>,
    mir: &'lt Mir<'mir>,
}

impl<'lt, 'mir: 'lt, 'cfg: 'lt> ReachableDefinitionsAnalysis<'lt, 'mir, 'cfg> {
    pub fn new(mir: &'lt Mir<'mir>, cfg: &ControlFlowGraph<'cfg, 'mir>) -> Self {
        let mut graph = UseDefGraph::new(mir, cfg);

        for stmt in SafeRangeCreation::<StatementId>::full_range(mir) {
            match mir[stmt] {
                Statement::Assignment(_, var, val) => graph.get_assignments_mut(var).insert(stmt),

                Statement::Contribute(_, _, _, _) => (),
            }
        }

        Self { graph, mir }
    }

    pub fn run(
        mut self,
        cfg: &mut ControlFlowGraph<'cfg, 'mir>,
    ) -> (UseDefGraph<'mir, 'cfg>, DataFlowGraph<'cfg>) {
        let mut dependency_graph = self
            .engine_builder(cfg)
            .engine(cfg, (), ())
            .iterate_to_fixpoint();

        let mut reachable = BitSet::with_capacity(self.graph.total_def_count() as usize);

        cfg.for_all_blocks(|block| {
            reachable.union_with(&dependency_graph.in_sets[block]);
            for stmt in cfg[block].statements.iter().copied() {
                match self.mir[stmt] {
                    Statement::Assignment(_, var, val) => {
                        self.mir.track_expression(
                            val,
                            &mut UseDefBuilder {
                                graph: &mut self.graph,
                                use_stmt: stmt,
                            },
                        );
                        self.graph
                            .get_reachable_definitions_mut(stmt)
                            .0
                            .intersect_with(&reachable);

                        reachable.difference_with(&self.graph.get_assignments(var).0);
                        reachable.insert(stmt.unwrap().index() as usize);
                    }

                    Statement::Contribute(_, _, _, val) => {
                        self.mir.track_real_expression(
                            val,
                            &mut UseDefBuilder {
                                graph: &mut self.graph,
                                use_stmt: stmt,
                            },
                        );
                        self.graph
                            .get_reachable_definitions_mut(stmt)
                            .0
                            .intersect_with(&reachable);

                        // branches are currently not tracked
                    }
                }
            }

            if let Terminator::Split { condition, .. } = cfg[block].terminator {
                self.graph.terminator_uses[block]
                    .0
                    .grow(self.graph.statement_count as usize);

                self.mir.track_integer_expression(
                    condition,
                    &mut UseDefTerminatorBuilder {
                        graph: &mut self.graph,
                        use_terminator_block: block,
                    },
                );

                self.graph.terminator_uses[block]
                    .0
                    .intersect_with(&reachable);
            }
            reachable.clear();
        });

        (self.graph, dependency_graph)
    }
}

impl<'lt: 'engine, 'cfg: 'lt, 'mir: 'lt, 'engine> GenKillAnalysis<'engine, 'cfg, 'mir>
    for ReachableDefinitionsAnalysis<'lt, 'mir, 'cfg>
{
    type Engine = ForwardEngine<'engine, 'cfg, 'mir, GenKillEngine<'engine, 'cfg, Self>>;
    type EngineArgs = ();

    fn transfer_function(
        &mut self,
        gen_kill_set: &mut GenKillSet,
        basic_bock: BasicBlockId<'cfg>,
        cfg: &mut ControlFlowGraph<'cfg, 'mir>,
    ) {
        for stmt in cfg[basic_bock].statements.iter().copied() {
            match self.mir[stmt] {
                Statement::Assignment(_, var, val) => {
                    gen_kill_set.kill_all(&self.graph.get_assignments(var).0);
                    gen_kill_set.gen_statement(stmt);
                }

                // branches are currently not tracked
                Statement::Contribute(_, _, _, _) => {}
            }
        }
    }

    fn set_size(&self) -> usize {
        self.graph.statement_count as usize
    }

    fn engine(
        analysis: &'engine mut GenKillEngine<'engine, 'cfg, Self>,
        cfg: &'engine mut ControlFlowGraph<'cfg, 'mir>,
        _: (),
        _: (),
    ) -> Self::Engine {
        ForwardEngine::new_with_empty_dfg(cfg, analysis, ())
    }
}

struct UseDefBuilder<'lt, 'mir, 'cfg> {
    graph: &'lt mut UseDefGraph<'mir, 'cfg>,
    use_stmt: StatementId<'mir>,
}

impl<'lt, 'mir, 'cfg> ExtractionDependencyHandler<'mir> for UseDefBuilder<'lt, 'mir, 'cfg> {
    fn handle_variable_reference(&mut self, var: VariableId<'mir>) {
        self.graph.uses[self.use_stmt.unwrap()]
            .0
            .union_with(&self.graph.variables[var.unwrap()].0)
        // todo do intersection here for stateful lint (worse performance but should be okay)
    }

    fn handle_parameter_reference(&mut self, _: ParameterId<'mir>) {}

    fn handle_branch_reference(&mut self, _: DisciplineAccess, _: BranchId<'mir>) {}

    fn handle_system_function_call(&mut self, _: Ident) {}
}

struct UseDefTerminatorBuilder<'lt, 'mir, 'cfg> {
    graph: &'lt mut UseDefGraph<'mir, 'cfg>,
    use_terminator_block: BasicBlockId<'cfg>,
}

impl<'lt, 'mir, 'cfg> ExtractionDependencyHandler<'mir>
    for UseDefTerminatorBuilder<'lt, 'mir, 'cfg>
{
    fn handle_variable_reference(&mut self, var: VariableId<'mir>) {
        self.graph.terminator_uses[self.use_terminator_block]
            .0
            .union_with(&self.graph.variables[var.unwrap()].0);
    }

    fn handle_parameter_reference(&mut self, _: ParameterId<'mir>) {}

    fn handle_branch_reference(&mut self, _: DisciplineAccess, _: BranchId<'mir>) {}

    fn handle_system_function_call(&mut self, _: Ident) {}
}
