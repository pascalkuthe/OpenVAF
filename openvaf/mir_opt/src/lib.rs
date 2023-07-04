mod const_eval;
mod const_prop;
mod dead_code;
mod dead_code_aggressive;
mod global_value_numbering;
mod inst_combine;
mod simplify;
mod simplify_cfg;
mod split_tainted;

pub use const_prop::sparse_conditional_constant_propagation;
pub use dead_code::dead_code_elimination;
pub use dead_code_aggressive::aggressive_dead_code_elimination;
pub use global_value_numbering::{ClassId, GVN};
pub use inst_combine::inst_combine;
pub use simplify_cfg::{simplify_cfg, simplify_cfg_no_phi_merge};
pub use split_tainted::propagate_taint;
