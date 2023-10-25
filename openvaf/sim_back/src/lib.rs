
pub use module_info::{collect_modules, ModuleInfo};

mod context;
mod module_info;
mod noise;
mod topology;

mod util;

// #[cfg(test)]
// mod tests;

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
