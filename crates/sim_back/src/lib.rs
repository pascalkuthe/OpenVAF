mod compilation_db;
mod middle;

mod lim_rhs;
pub mod matrix;
pub mod residual;

pub use compilation_db::{CompilationDB, ModuleInfo};
use hir_def::NodeId;
use hir_lower::{CurrentKind, ImplicitEquation};
pub use middle::{BoundStepKind, CacheSlot, EvalMir};
use mir::{Function, InstructionData, Opcode, Value};
pub use residual::Residual;
use stdx::impl_debug_display;

#[cfg(test)]
mod tests;

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub enum SimUnknown {
    KirchoffLaw(NodeId),
    Current(CurrentKind),
    Implicit(ImplicitEquation),
}

impl_debug_display! {
    match SimUnknown{
        SimUnknown::KirchoffLaw(node) => "{node}";
        SimUnknown::Current(curr) => "br[{curr:?}]";
        SimUnknown::Implicit(node) => "{node}";
    }
}

fn strip_optbarrier(func: &Function, mut val: Value) -> Value {
    while let Some(inst) = func.dfg.value_def(val).inst() {
        if let InstructionData::Unary { opcode: Opcode::OptBarrier, arg } = func.dfg.insts[inst] {
            val = arg;
        } else {
            break;
        }
    }
    val
}
