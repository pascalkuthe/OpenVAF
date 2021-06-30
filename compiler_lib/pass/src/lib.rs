/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

pub use backward_slice::BackwardSlice;
pub use forward_slice::ForwardSlice;
pub use live_variables::{DeadCodeElimination, DeadCodeScan, LiveLocalAnalysis};
pub use post_dominance::BuildPostDominators;
pub use reaching_definitions::{FindAssignments, ReachingDefinitionsAnalysis};
pub use remove_dead_locals::RemoveDeadLocals;
pub use simplify::Simplify;
pub use simplify_branches::SimplifyBranches;
pub use strip::Strip;
pub use verify::Verify;
pub use visit::Visit;

//mod copy_propagation;
mod backward_slice;
mod forward_slice;
mod lints;
mod live_variables;
mod post_dominance;
pub mod program_dependence;
mod reaching_definitions;
mod remove_dead_locals;
mod simplify;
mod simplify_branches;
mod strip;
pub mod verify;
pub mod visit;
// mod copy_propagation;
