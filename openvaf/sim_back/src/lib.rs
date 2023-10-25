
pub use module_info::{collect_modules, ModuleInfo};

mod context;
pub mod dae;
mod module_info;
mod noise;
mod topology;

mod util;

// #[cfg(test)]
// mod tests;

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub enum SimUnknownKind {
    KirchoffLaw(Node),
    Current(CurrentKind),
    Implicit(ImplicitEquation),
}

impl_debug_display! {
    match SimUnknownKind{
        SimUnknownKind::KirchoffLaw(node) => "{node:?}";
        SimUnknownKind::Current(curr) => "br[{curr:?}]";
        SimUnknownKind::Implicit(node) => "{node}";
    }
}
