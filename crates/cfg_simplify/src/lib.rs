mod remove_dead_data;
mod simplify;
mod simplify_branches;

#[cfg(test)]
mod tests;

pub use remove_dead_data::{remove_dead_data, LocalMap, ParamMap, PlaceMap};
pub use simplify::simplify_cfg;
pub use simplify_branches::simplify_branches;
