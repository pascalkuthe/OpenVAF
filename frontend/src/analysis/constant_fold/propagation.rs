use crate::analysis::constant_fold::lattice::ProductLattice;
use crate::analysis::constant_fold::resolver::LocalConstantPropagator;
use crate::analysis::constant_fold::{DiamondLattice, TypedDiamondLattice};
use crate::analysis::data_flow::framework::{Analysis, DataFlowGraph, Engine, Forward};
use crate::cfg::Terminator;
use crate::ir::cfg::{BasicBlockId, ControlFlowGraph};
use crate::ir::ids::ParameterId;
use crate::mir::{Mir, Statement};
use crate::{HashMap, StringLiteral};
use index_vec::index_vec;

// For constants that shall be substituted everywhere
#[derive(Clone, Debug, Default)]
pub struct GlobalConstants {
    pub real_parameters: HashMap<ParameterId, f64>,
    pub int_parameters: HashMap<ParameterId, i64>,
    pub string_parameters: HashMap<ParameterId, StringLiteral>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlockConstants {
    unreachable: bool,
    product_lattice: ProductLattice,
}
impl BasicBlockConstants {
    pub fn mark_unreachable(&mut self) {
        self.unreachable = true;
        for x in &mut self.product_lattice {
            *x = TypedDiamondLattice::Unknown
        }
    }

    pub fn meet(&mut self, other: &Self) {
        for (lattic, other) in self.product_lattice.iter_mut().zip(&other.product_lattice) {
            *lattic = lattic.meet(*other)
        }
    }
}

pub struct ConditionalConstantPropagation<'lt> {
    pub globals: &'lt GlobalConstants,
    pub mir: &'lt Mir,
    pub cfg: &'lt ControlFlowGraph,
}

impl<'lt> Analysis for ConditionalConstantPropagation<'lt> {
    type Set = BasicBlockConstants;
    type Direction = Forward;

    fn transfer_function(
        &mut self,
        in_set: &Self::Set,
        out_set: &mut Self::Set,
        basic_bock: BasicBlockId,
        cfg: &ControlFlowGraph,
    ) {
        if in_set.unreachable {
            out_set.unreachable = true
        } else {
            out_set
                .product_lattice
                .copy_from_slice(&in_set.product_lattice);
            out_set.unreachable = false;
            for stmt in &cfg[basic_bock].statements {
                if let Statement::Assignment(var, val) = self.mir[*stmt].contents {
                    out_set.product_lattice[var] = self.mir.constant_eval_expr(
                        val,
                        &mut LocalConstantPropagator(&out_set.product_lattice, self.globals),
                    )
                }
            }
        }
    }

    fn join(&mut self, src: BasicBlockId, dst: BasicBlockId, graph: &mut DataFlowGraph<Self::Set>) {
        if !graph.out_sets[src].unreachable {
            // try to constant fold terminator and mark as unreachable
            if let Terminator::Split {
                condition,
                false_block,
                ..
            } = self.cfg[src].terminator
            {
                if let DiamondLattice::Val(val) = self.mir.constant_eval_int_expr(
                    condition,
                    &mut LocalConstantPropagator(
                        &graph.out_sets[src].product_lattice,
                        self.globals,
                    ),
                ) {
                    if (val == 0) != (dst == false_block) {
                        // unreachable don't propagate constants
                        return;
                    }
                }
            }

            graph.in_sets[dst].unreachable = false;
            graph.in_sets[dst].meet(&graph.out_sets[src])
        }
    }

    fn new_set(&self) -> Self::Set {
        BasicBlockConstants {
            unreachable: true,
            product_lattice: index_vec![TypedDiamondLattice::Unknown;self.mir.variables.len()],
        }
    }

    fn setup_entry(&mut self, block: BasicBlockId, graph: &mut DataFlowGraph<Self::Set>) {
        graph.in_sets[block].unreachable = false
    }
}

impl ControlFlowGraph {
    pub fn propagate_constants(&mut self, mir: &mut Mir, globals: &GlobalConstants) {
        let dfg = Engine::new(
            self,
            &mut ConditionalConstantPropagation {
                globals,
                mir,
                cfg: &self,
            },
        )
        .iterate_to_fixpoint();

        for (bb, mut local_constants) in dfg.in_sets.into_iter_enumerated() {
            if local_constants.unreachable {
                continue;
            }

            for stmt in &self[bb].statements {
                match mir[*stmt].contents {
                    Statement::Assignment(var, val) => {
                        local_constants.product_lattice[var] = mir.constant_fold_expr(
                            val,
                            &mut LocalConstantPropagator(&local_constants.product_lattice, globals),
                        )
                    }

                    Statement::Contribute(_, _, expr) => {
                        mir.constant_fold_real_expr(
                            expr,
                            &mut LocalConstantPropagator(&local_constants.product_lattice, globals),
                        );
                    }
                    Statement::StopTask(_, _) => {}
                }
            }

            if let Terminator::Split {
                condition,
                true_block,
                false_block,
                ..
            } = self[bb].terminator
            {
                if let DiamondLattice::Val(cond) = mir.constant_fold_int_expr(
                    condition,
                    &mut LocalConstantPropagator(&local_constants.product_lattice, globals),
                ) {
                    if cond == 0 {
                        self[bb].terminator = Terminator::Goto(false_block)
                    } else {
                        self[bb].terminator = Terminator::Goto(true_block)
                    }
                }
            }
        }
    }
}
