//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

mod constant_propagation;
mod control_dependence;
pub mod dfa_framework;
pub mod lints;
mod post_dominance;
mod program_slicing;
mod reaching_definitions;

pub use constant_propagation::{conditional_constant_propagation, GlobalConstants};
pub use control_dependence::ControlDependenceGraph;
pub use post_dominance::{post_dominators, IPDOM};
pub use program_slicing::{
    backward_slice, backward_variable_slice, backward_variable_slice_assuming_input,
    backward_variable_slice_with_variables_as_input,
};
pub use reaching_definitions::{ReachingDefinitionsAnalysis, UseDefGraph};

pub struct ProgramDependenceGraph {
    pub data_dependencies: UseDefGraph,
    pub control_dependencies: ControlDependenceGraph,
}
