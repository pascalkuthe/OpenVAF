//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

pub mod constant_folding;
pub mod data_flow;
pub mod program_slicing;
#[cfg(test)]
mod test;
pub use program_slicing::DependencyHandler;
mod control_dependence;
mod post_dominance;
use crate::analysis::data_flow::reaching_variables::UseDefGraph;
pub use control_dependence::ControlDependenceGraph;
pub use post_dominance::IPDOM;

pub struct ProgramDependenceGraph {
    pub data_dependencies: UseDefGraph,
    pub control_dependencies: ControlDependenceGraph,
}
