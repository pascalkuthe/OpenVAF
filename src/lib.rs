//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************
//! A frontend for Verilog AMS that returns an Ast as its endresult
//!
//! TODO showoff API
//!
//!

#[macro_use]
extern crate intrusive_collections;
#[macro_use]
extern crate lazy_static;
extern crate std;

pub use ir::ast;
#[macro_use]
pub use ir::ast::Ast;
pub use parser::lexer::Lexer;
pub use parser::preprocessor::Preprocessor;
pub use parser::preprocessor::SourceMap;
pub use ptr::FrozenBox;
pub use span::Span;

#[macro_use]
pub mod compact_arena;
mod schemantic;
pub mod symbol;

#[macro_use]
mod util;
#[macro_use]
mod ir;
mod error;
pub mod parser;
mod ptr;
mod span;
mod symbol_table;
#[cfg(test)]
mod test;
