mod back;
mod compilation_db;
mod middle;

pub mod matrix;
pub mod residual;

pub use compilation_db::{CompilationDB, ModuleInfo};
pub use middle::{CacheSlot, EvalMir};
pub use residual::Residual;

#[cfg(test)]
mod tests;
