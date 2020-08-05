use openvaf_constant_fold::{
    constant_eval_expr, constant_eval_int_expr, constant_fold_expr, constant_fold_int_expr,
    constant_fold_real_expr, ConstResolver, DiamondLattice, ProductLattice, TypedDiamondLattice,
};
use openvaf_data_structures::index_vec::index_vec;
use openvaf_data_structures::HashMap;
use openvaf_ir::ids::{ParameterId, VariableId};
use openvaf_mir::cfg::Terminator;
use openvaf_mir::cfg::{BasicBlockId, ControlFlowGraph};
use openvaf_mir::{Mir, Statement};
use openvaf_session::sourcemap::StringLiteral;

use crate::dfa_framework::{Analysis, DataFlowGraph, Engine, Forward};

pub struct LocalConstantPropagator<'lt>(pub &'lt ProductLattice, pub &'lt GlobalConstants);

impl<'lt> ConstResolver for LocalConstantPropagator<'lt> {
    fn real_variable_value(&mut self, var: VariableId) -> DiamondLattice<f64> {
        self.0[var].into()
    }

    fn int_variable_value(&mut self, var: VariableId) -> DiamondLattice<i64> {
        self.0[var].into()
    }

    fn str_variable_value(&mut self, var: VariableId) -> DiamondLattice<StringLiteral> {
        self.0[var].into()
    }

    fn real_parameter_value(&mut self, param: ParameterId) -> DiamondLattice<f64> {
        self.1
            .real_parameters
            .get(&param)
            .copied()
            .map_or(DiamondLattice::NotAConstant, DiamondLattice::Val)
    }

    fn int_parameter_value(&mut self, param: ParameterId) -> DiamondLattice<i64> {
        self.1
            .int_parameters
            .get(&param)
            .copied()
            .map_or(DiamondLattice::NotAConstant, DiamondLattice::Val)
    }

    fn str_parameter_value(&mut self, param: ParameterId) -> DiamondLattice<StringLiteral> {
        self.1
            .string_parameters
            .get(&param)
            .copied()
            .map_or(DiamondLattice::NotAConstant, DiamondLattice::Val)
    }
}

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
                    out_set.product_lattice[var] = constant_eval_expr(
                        self.mir,
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
                if let DiamondLattice::Val(val) = constant_eval_int_expr(
                    self.mir,
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

pub fn conditional_constant_propagation(
    cfg: &mut ControlFlowGraph,
    mir: &mut Mir,
    globals: &GlobalConstants,
) {
    let dfg = Engine::new(
        cfg,
        &mut ConditionalConstantPropagation { globals, mir, cfg },
    )
    .iterate_to_fixpoint();

    for (bb, mut local_constants) in dfg.in_sets.into_iter_enumerated() {
        if local_constants.unreachable {
            continue;
        }

        for stmt in &cfg[bb].statements {
            match mir[*stmt].contents {
                Statement::Assignment(var, val) => {
                    local_constants.product_lattice[var] = constant_fold_expr(
                        mir,
                        val,
                        &mut LocalConstantPropagator(&local_constants.product_lattice, globals),
                    )
                }

                Statement::Contribute(_, _, expr) => {
                    constant_fold_real_expr(
                        mir,
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
        } = cfg[bb].terminator
        {
            if let DiamondLattice::Val(cond) = constant_fold_int_expr(
                mir,
                condition,
                &mut LocalConstantPropagator(&local_constants.product_lattice, globals),
            ) {
                if cond == 0 {
                    cfg[bb].terminator = Terminator::Goto(false_block)
                } else {
                    cfg[bb].terminator = Terminator::Goto(true_block)
                }
            }
        }
    }
}
