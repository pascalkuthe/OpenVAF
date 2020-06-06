//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************
#![allow(clippy::similar_names)]

use crate::analysis::data_flow::framework::{
    DataFlowGraph, Engine, Forward, GenKillAnalysis, GenKillEngine, GenKillSet,
};
use crate::analysis::DependencyHandler;

use crate::cfg::*;
use crate::data_structures::BitSet;
use crate::ir::hir::DisciplineAccess;
use crate::ir::{
    BranchId, ParameterId, PortId, RealExpressionId, StatementId, StringExpressionId,
    SystemFunctionCall, VariableId,
};
use crate::mir::Mir;
use crate::mir::Statement;
use crate::ControlFlowGraph;
use index_vec::*;

#[derive(Debug, Clone)]
pub struct UseDefGraph {
    pub stmt_use_def_chains: IndexVec<StatementId, BitSet<StatementId>>,
    pub terminator_use_def_chains: IndexVec<BasicBlockId, BitSet<StatementId>>,
    pub assignments: IndexVec<VariableId, BitSet<StatementId>>,
}

impl UseDefGraph {
    pub fn new(mir: &Mir, cfg: &ControlFlowGraph) -> Self {
        let statement_count = mir.statements.len();
        let var_count = mir.variables.len();

        Self {
            stmt_use_def_chains: index_vec![BitSet::new_empty(statement_count.into());statement_count],
            terminator_use_def_chains: index_vec![BitSet::new_empty(statement_count.into());cfg.blocks.len()],
            assignments: index_vec![BitSet::new_empty(statement_count.into());var_count],
        }
    }

    pub fn stmt_count(&self) -> u32 {
        self.stmt_use_def_chains.len() as u32
    }

    pub fn len_stmd_idx(&self) -> StatementId {
        self.stmt_use_def_chains.len_idx()
    }
}

pub struct ReachableDefinitionsAnalysis<'lt> {
    graph: UseDefGraph,
    mir: &'lt Mir,
}

impl<'lt> ReachableDefinitionsAnalysis<'lt> {
    pub fn new(mir: &'lt Mir, cfg: &ControlFlowGraph) -> Self {
        let mut graph = UseDefGraph::new(mir, cfg);

        for (id, stmt) in mir.statements.iter_enumerated() {
            match stmt {
                Statement::Assignment(_, var, _) => graph.assignments[*var].insert(id),

                Statement::Contribute(_, _, _, _) => (),
            }
        }

        Self { graph, mir }
    }

    pub fn run(mut self, cfg: &ControlFlowGraph) -> (UseDefGraph, DataFlowGraph<StatementId>) {
        let mut genkill_engine = GenKillEngine::new(cfg, &mut self);
        let engine = Engine::new(cfg, &mut genkill_engine);
        let dfg = engine.iterate_to_fixpoint();

        let mut reachable = BitSet::new_empty(self.graph.stmt_use_def_chains.len_idx());

        for (id, bb) in cfg.blocks.iter_enumerated() {
            reachable.union_with(&dfg.in_sets[id]);
            for stmt in bb.statements.iter().copied() {


                match self.mir[stmt] {
                    Statement::Assignment(_, var, expr) => {
                        self.mir.track_expression(
                            expr,
                            &mut UseDefBuilder {
                                graph: &mut self.graph,
                                use_stmt: stmt,
                            },
                        );

                        self.graph.stmt_use_def_chains[stmt].intersect_with(&reachable);

                        reachable.difference_with(&self.graph.assignments[var]);
                        reachable.insert(stmt);
                    }

                    Statement::Contribute(_, _, _, val) => {
                        self.mir.track_real_expression(
                            val,
                            &mut UseDefBuilder {
                                graph: &mut self.graph,
                                use_stmt: stmt,
                            },
                        );
                        self.graph.stmt_use_def_chains[stmt].intersect_with(&reachable);

                        // branches are currently not tracked
                    }
                }
            }

            if let Terminator::Split { condition, .. } = bb.terminator {
                self.graph.terminator_use_def_chains[id]
                    .grow(self.graph.stmt_use_def_chains.len_idx());

                self.mir.track_integer_expression(
                    condition,
                    &mut UseDefTerminatorBuilder {
                        graph: &mut self.graph,
                        use_terminator_block: id,
                    },
                );

                self.graph.terminator_use_def_chains[id].intersect_with(&reachable);
            }

            reachable.clear();
        }

        (self.graph, dfg)
    }
}

impl<'lt> GenKillAnalysis<'_> for ReachableDefinitionsAnalysis<'lt> {
    type SetType = StatementId;
    type Direction = Forward;

    fn transfer_function(
        &mut self,
        gen_kill_set: &mut GenKillSet<StatementId>,
        basic_bock: BasicBlockId,
        cfg: &ControlFlowGraph,
    ) {
        for stmt in cfg[basic_bock].statements.iter().copied() {
            match self.mir[stmt] {
                Statement::Assignment(_, var, _) => {
                    gen_kill_set.kill_all(&self.graph.assignments[var]);
                    gen_kill_set.gen(stmt);
                }

                // branches are currently not tracked
                Statement::Contribute(_, _, _, _) => {}
            }
        }
    }

    fn max_idx(&self) -> Self::SetType {
        self.graph.len_stmd_idx()
    }
}

struct UseDefBuilder<'lt> {
    graph: &'lt mut UseDefGraph,
    use_stmt: StatementId,
}

impl<'lt> DependencyHandler for UseDefBuilder<'lt> {
    fn handle_variable_reference(&mut self, var: VariableId) {
        self.graph.stmt_use_def_chains[self.use_stmt].union_with(&self.graph.assignments[var])
        // todo do intersection here for stateful lint (worse performance but should be okay)
    }

    fn handle_parameter_reference(&mut self, _: ParameterId) {}

    fn handle_branch_reference(&mut self, _: DisciplineAccess, _: BranchId, _: u8) {}

    fn handle_system_function_call(
        &mut self,
        _: SystemFunctionCall<RealExpressionId, StringExpressionId, PortId, ParameterId>,
    ) {
    }
}

struct UseDefTerminatorBuilder<'lt> {
    graph: &'lt mut UseDefGraph,
    use_terminator_block: BasicBlockId,
}

impl<'lt> DependencyHandler for UseDefTerminatorBuilder<'lt> {
    fn handle_variable_reference(&mut self, var: VariableId) {
        self.graph.terminator_use_def_chains[self.use_terminator_block]
            .union_with(&self.graph.assignments[var]);
    }

    fn handle_parameter_reference(&mut self, _: ParameterId) {}

    fn handle_branch_reference(&mut self, _: DisciplineAccess, _: BranchId, _: u8) {}

    fn handle_system_function_call(
        &mut self,
        _: SystemFunctionCall<RealExpressionId, StringExpressionId, PortId, ParameterId>,
    ) {
    }
}

#[cfg(feature = "graph_debug")]
mod print {
    use super::*;
    use rustc_ap_graphviz as dot;
    use rustc_ap_graphviz::LabelText::{EscStr, LabelStr};
    use rustc_ap_graphviz::{Edges, GraphWalk, Id, LabelText, Labeller, Nodes};
    use rustc_hash::FxHashSet;
    use std::borrow::Cow;
    use std::io::Write;

    impl ControlFlowGraph {
        pub fn render_data_dependence_to<W: Write>(&self, write: &mut W, udg: &UseDefGraph) {
            dot::render(&DataDependenceGraph(self, udg), write).expect("Rendering failed")
        }
    }

    struct DataDependenceGraph<'lt>(&'lt ControlFlowGraph, &'lt UseDefGraph);

    #[derive(Copy, Clone, Eq, PartialEq)]
    enum FlowOrDependence {
        Flow,
        Dependence,
    }

    impl<'a> dot::Labeller<'a> for DataDependenceGraph<'a> {
        type Node = BasicBlockId;
        type Edge = (FlowOrDependence, BasicBlockId, BasicBlockId);

        fn graph_id(&'a self) -> Id<'a> {
            dot::Id::new("ControlFlowGraph").unwrap()
        }

        fn node_id(&'a self, n: &Self::Node) -> Id<'a> {
            dot::Id::new(format!("BB_{}", n.index())).unwrap()
        }

        fn node_label(&'a self, &n: &Self::Node) -> LabelText<'a> {
            match self.0.blocks[n].terminator {
                Terminator::End => EscStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\n END",
                    n.index(),
                    self.0.blocks[n].statements.len(),
                ))),
                Terminator::Goto(_) => LabelStr(Cow::Owned(format!(
                    "BB_{}: {} Statements",
                    n.index(),
                    self.0.blocks[n].statements.len(),
                ))),
                Terminator::Split {
                    condition, merge, ..
                } => LabelStr(Cow::Owned(format!(
                    "BB_{}: {} Statements\nSplit at {:?}\nMerge at BB_{}",
                    n.index(),
                    self.0.blocks[n].statements.len(),
                    condition,
                    merge.index(),
                ))),
            }
        }
        fn edge_label(
            &'a self,
            &(edge_type, start, dst): &(FlowOrDependence, BasicBlockId, BasicBlockId),
        ) -> LabelText<'a> {
            match edge_type {
                FlowOrDependence::Flow => match self.0[start].terminator {
                    Terminator::Goto(_) => LabelStr(Cow::Borrowed("GOTO")),
                    Terminator::End => LabelStr(Cow::Borrowed("ILLEGAL")),
                    Terminator::Split {
                        condition,
                        true_block,
                        false_block,
                        merge,
                    } => {
                        let true_or_false = if true_block == dst {
                            "TRUE"
                        } else if false_block == dst {
                            "FALSE"
                        } else {
                            "ILLEGAL"
                        };
                        LabelStr(Cow::Borrowed(true_or_false))
                    }
                },

                FlowOrDependence::Dependence => LabelStr(Cow::Borrowed("Data dependence")),
            }
        }
    }

    impl<'a> dot::GraphWalk<'a> for DataDependenceGraph<'a> {
        type Node = BasicBlockId;
        type Edge = (FlowOrDependence, BasicBlockId, BasicBlockId);

        fn nodes(&'a self) -> Nodes<'a, Self::Node> {
            Cow::Owned(self.0.blocks.indices().collect())
        }

        fn edges(&'a self) -> Edges<'a, Self::Edge> {
            let mut edges = Vec::new();
            for (id, bb) in self.0.blocks.iter_enumerated() {
                match bb.terminator {
                    Terminator::Goto(dst) => edges.push((FlowOrDependence::Flow, id, dst)),
                    Terminator::Split {
                        condition,
                        true_block,
                        false_block,
                        merge,
                    } => {
                        edges.push((FlowOrDependence::Flow, id, false_block));
                        edges.push((FlowOrDependence::Flow, id, true_block));
                    }
                    Terminator::End => (),
                }
            }
            let mut processed_data_dependencies = FxHashSet::with_capacity_and_hasher(
                self.0.blocks.len() * self.0.blocks.len(),
                Default::default(),
            );

            for (src, data_dependencies) in self.1.stmt_use_def_chains.iter_enumerated() {
                let src_block = self.0.containing_block(src).unwrap();
                for dst in data_dependencies.ones() {
                    let dst_block = self.0.containing_block(dst).unwrap();
                    if processed_data_dependencies.insert((src_block, dst_block)) {
                        edges.push((FlowOrDependence::Dependence, src_block, dst_block));
                    }
                }
            }

            for (src, data_dependencies) in self.1.terminator_use_def_chains.iter_enumerated() {
                for dst in data_dependencies.ones() {
                    let dst_block = self.0.containing_block(dst).unwrap();
                    if processed_data_dependencies.insert((src, dst_block)) {
                        edges.push((FlowOrDependence::Dependence, src, dst_block));
                    }
                }
            }

            Cow::Owned(edges)
        }

        fn source(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.1
        }

        fn target(&'a self, edge: &Self::Edge) -> Self::Node {
            edge.2
        }
    }
}
