pub use middle::{BoundStepKind, CacheSlot, EvalMir};
pub use module_info::{collect_modules, ModuleInfo};
pub use residual::Residual;

use hir::Node;
use hir_lower::{CurrentKind, ImplicitEquation};
use stdx::impl_debug_display;

mod lim_rhs;
pub mod matrix;
mod middle;
mod module_info;
mod prune;
pub mod residual;

mod util;

#[cfg(test)]
mod tests;

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub enum SimUnknown {
    KirchoffLaw(Node),
    Current(CurrentKind),
    Implicit(ImplicitEquation),
}

impl_debug_display! {
    match SimUnknown{
        SimUnknown::KirchoffLaw(node) => "{node:?}";
        SimUnknown::Current(curr) => "br[{curr:?}]";
        SimUnknown::Implicit(node) => "{node}";
    }
}
