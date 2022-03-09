mod const_prop;
mod dead_code;
mod dead_code_agressive;
mod inst_combine;
mod post_dominators;
mod simplify_cfg;

pub use const_prop::sparse_conditional_constant_propagation;
pub use dead_code::dead_code_elimination;
pub use dead_code_agressive::agressive_dead_code_elimination;
pub use inst_combine::inst_combine;
pub use simplify_cfg::simplify_cfg;
