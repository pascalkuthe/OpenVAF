pub use compilation_db::{CompilationDB, ModuleInfo};
use hir_def::NodeId;
use hir_lower::{CurrentKind, ImplicitEquation};
pub use middle::{BoundStepKind, CacheSlot, EvalMir};
pub use residual::Residual;
use stdx::impl_debug_display;

mod compilation_db;
mod lim_rhs;
pub mod matrix;
mod middle;
pub mod residual;

mod prune;
#[cfg(test)]
mod tests;
mod util;

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
