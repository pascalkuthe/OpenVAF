//! This crate provides a collection of simple optimization alogrithms for the CFG
//! These alogrithms do not serve a purpose of speeding up execution time as this is already well
//! covered by LLVM.
//! Instead these algorithms aim to optimize the CFG so that other analysis passes produce better
//! results or execute faster.

mod copy_propagation;
mod dead_code;
mod remove_dead_data;
mod simplify;
mod simplify_branches;

#[cfg(test)]
mod tests;

pub use copy_propagation::copy_propagation;
pub use dead_code::{dead_code_elimination, DeadCodeElimination, LivePlacesAnalysis};
pub use remove_dead_data::{remove_dead_data, LocalMap, ParamMap, PlaceMap};
pub use simplify::simplify_cfg;
pub use simplify_branches::simplify_branches;
