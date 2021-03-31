/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

//mod copy_propagation;
mod backward_slice;
mod lints;
mod program_dependence;
mod remove_dead_locals;
mod simplify;
mod simplify_branches;
mod verify;

pub use backward_slice::BackwardSlice;
pub use program_dependence::{
    BuildPDG, ControlDependenceGraph, ProgramDependenceGraph, ReachingDefinitionsAnalysis,
    UseDefGraph, CalculateDataDependence, BuildReachingDefinitions
};
pub use remove_dead_locals::RemoveDeadLocals;
pub use simplify::Simplify;
pub use simplify_branches::SimplifyBranches;
pub use verify::{MalformationKind, Malformation, Verify};
