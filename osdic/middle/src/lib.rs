/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::storage_locations::StorageLocations;
pub use frontend::{run_frontend, run_frontend_from_ts, GeneralOsdiCall};
use itertools::Itertools;
use openvaf_data_structures::{BitSet, HashMap};
use openvaf_diagnostics::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use openvaf_ir::ids::SyntaxCtx;
use openvaf_ir::Spanned;
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations, START_BLOCK};
use openvaf_middle::const_fold::{ConstantPropagation, NoInputConstResolution};
use openvaf_middle::osdi_types::ConstVal::Scalar;
use openvaf_middle::osdi_types::SimpleConstVal::Real;
use openvaf_middle::{CallType, LocalKind, Mir, OperandData, RValue, StmntKind, VariableLocalKind};
use openvaf_session::sourcemap::span::DUMMY_SP;
use openvaf_transformations::{
    BackwardSlice, CfgVisitor, DeadCodeElimination, LiveLocalAnalysis, RemoveDeadLocals, Simplify,
    SimplifyBranches, Visit,
};
use std::fs::File;
pub use subfuncitons::OsdiFunctions;
pub use topology::CircuitTopology;
use tracing::info;

mod frontend;
mod lim;
mod storage_locations;
mod subfuncitons;
mod topology;

pub fn run_middle<P: DiagnosticSlicePrinter>(
    mir: &Mir<GeneralOsdiCall>,
    cfg: &mut ControlFlowGraph<GeneralOsdiCall>,
) -> UserResult<(CircuitTopology, OsdiFunctions), P> {
    let mut topology = CircuitTopology::new(&mir, cfg);

    let mut errors = MultiDiagnostic(Vec::new());
    cfg.generate_derivatives(&mir, &mut errors);

    if !errors.is_empty() {
        return Err(errors.user_facing::<P>().into());
    }
    cfg.insert_variable_declarations(&mir);

    cfg.blocks[START_BLOCK].statements.raw.splice(
        0..0,
        cfg.locals.iter_enumerated().filter_map(|(local, decl)| {
            if matches!(decl.kind, LocalKind::Branch(_, _, _)) {
                Some((
                    StmntKind::Assignment(
                        local,
                        RValue::Use(Spanned {
                            span: DUMMY_SP,
                            contents: OperandData::Constant(Scalar(Real(0.0))),
                        }),
                    ),
                    SyntaxCtx::ROOT,
                ))
            } else {
                None
            }
        }),
    );

    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);

    let block_constants = cfg.fold_constants(&NoInputConstResolution::new(), HashMap::new());

    let constant_at_iteration_end = &block_constants.out_sets[cfg.end()].constants;

    cfg.write_constants(block_constants.in_sets, &NoInputConstResolution::new());

    let locations = cfg.intern_locations();

    topology.removed_const_zero_derivatives(constant_at_iteration_end);

    let local_map = cfg.run_pass(RemoveDeadLocals);

    for connection in &mut topology.connections {
        for local in &mut connection.derivatives {
            if let Some(local) = local {
                *local = local_map[*local]
            }
        }
    }

    topology.create_matrix_entires(&mir);

    info!(
        derivatives = debug(topology.matrix_stamp_locals.ones().collect_vec()),
        matrix_entries = debug(&topology.matrix_entries),
        "Completed analog block analysis!",
    );

    let mut output_locals = topology.matrix_stamp_locals.clone();
    for (local, decl) in cfg.locals.iter_enumerated() {
        if matches!(decl.kind, LocalKind::Branch(_, _, VariableLocalKind::User)) {
            output_locals.insert(local)
        }
    }

    cfg.run_pass(DeadCodeElimination::with_output_locals(
        &locations,
        output_locals,
    ));

    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);

    let mut storage = StorageLocations::new(&cfg);

    let functions = OsdiFunctions::create_from_analog_block_by_automatic_division(
        &mir, cfg, &topology, &storage,
    );

    storage.init_locations_positions(&functions.model_storage, &functions.instance_storage);

    Ok((topology, functions))
}

//mod sim_spec;
pub fn optimize_cfg<C: CallType + 'static>(cfg: &mut ControlFlowGraph<C>) {
    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);
    cfg.run_pass(RemoveDeadLocals);
    cfg.run_pass(ConstantPropagation::default());
    cfg.run_pass(Simplify);
    cfg.run_pass(SimplifyBranches);
    cfg.run_pass(RemoveDeadLocals);
}
