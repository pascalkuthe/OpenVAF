mod frontend;
mod lim;
mod subfuncitons;
use openvaf_middle::const_fold::ConstantPropagation;

pub use frontend::{run_frontend, run_frontend_from_ts};
use openvaf_middle::cfg::ControlFlowGraph;
use openvaf_middle::CallType;
use openvaf_transformations::{RemoveDeadLocals, Simplify, SimplifyBranches};
pub use subfuncitons::divide_analog_block;


//mod sim_spec;
