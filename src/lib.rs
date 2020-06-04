//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************
//! A frontend for Verilog AMS that returns an Tree based IR as its end result
//!
//! ```
//! # use bumpalo::Bump;
//! # use std::path::Path;
//! let source_map_allocator = Bump::new();
//!  let source_map = ast
//!     .parse_from_and_print_errors(Path::new("<File>"), &source_map_allocator, true)?;
//!  let hir = ast
//!     .lower_and_print_errors(source_map, true)?;
//!  let mir = hir
//!     .lower_and_print_errors(source_map, true)?;
//! ```
//!  
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

pub use ahash;
pub use rustc_hash;

#[doc(inline)]
pub use ir::ast;
#[doc(hidden)]
pub use ir::ast::Ast;
#[doc(inline)]
pub use ir::cfg;
#[doc(hidden)]
pub use ir::cfg::ControlFlowGraph;
#[doc(inline)]
pub use ir::hir;
#[doc(hidden)]
pub use ir::hir::Hir;
#[doc(inline)]
pub use ir::mir;
#[doc(hidden)]
pub use parser::lexer::Lexer;
#[doc(hidden)]
pub use parser::preprocessor::Preprocessor;
#[doc(inline)]
pub use parser::preprocessor::SourceMap;
#[doc(hidden)]
pub(crate) use parser::Parser;
#[doc(inline)]
pub use span::Span;

pub mod symbol;
#[macro_use]
pub mod util;
#[macro_use]
pub mod ir;
pub mod analysis;
pub mod ast_lowering;
pub mod data_structures;
mod error;
mod hir_lowering;
mod literals;
pub mod parser;
mod span;
pub mod symbol_table;

pub use literals::StringLiteral;
