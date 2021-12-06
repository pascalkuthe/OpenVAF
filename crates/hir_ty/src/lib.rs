pub mod builtin;
pub mod db;
pub mod diagnostics;
pub mod inference;
mod lower;
pub mod types;
pub mod validation;

pub use lower::{BranchTy, DisciplineTy, NatureTy};

#[cfg(test)]
mod tests;
