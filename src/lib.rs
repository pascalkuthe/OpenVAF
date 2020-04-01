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
extern crate bitflags;
#[macro_use]
extern crate intrusive_collections;
#[macro_use]
extern crate lazy_static;
extern crate std;

pub use bumpalo;

pub use ir::ast;
pub use ir::ast::Ast;
pub use ir::hir;
pub use ir::hir::Hir;
pub use ir::mir;
pub use parser::lexer::Lexer;
pub use parser::preprocessor::Preprocessor;
pub use parser::preprocessor::SourceMap;
pub use schemantic_analysis::run_semantic;
pub use span::Span;

#[macro_use]
pub mod compact_arena;
pub mod symbol;
#[macro_use]
pub mod util;
#[macro_use]
pub mod ir;
pub mod ast_lowering;
mod error;
pub mod parser;
mod schemantic_analysis;
mod span;
pub mod symbol_table;
