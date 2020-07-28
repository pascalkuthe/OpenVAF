//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

//! OpenVAF is a framework for writing VerilogA compiler/static analysis tools focused on compact modelling
//! As such it provides the ability to fully parse and check a VerilogA source file and bring it into a form both suitable for code generation and analysis.
//! Furtheremore it provides a highly configuarable diagnostic/lint/error system that allows printing
//! [very informative feedback](https://dspom.gitlab.io/OpenVAF/diagnostics.html) to users
//!
//!
//! # Compilation steps
//!
//! To make navigating the documentation and sourcecode easier a highlevel overview of the compilation process is provided below
//!
//! ## [Preprocessing](crate::preprocessor)
//!
//! The first contact with sourcefiles happen here. These sourcefile are loaded into an [`sourcemap`].
//! When a sourcefile is loaded its tokens are lexed into tokens which are then checked for compiler directive.
//! These compiler directives are resolved and a vector of `ParserTokens` is produced
//!
//! ## [Parsing](crate::parser)
//!
//! The preprocessed Tokens are parsed into [`AST`](crate::ast::Ast) nodes during this step.
//! Any names (variables, parameters, etc) are also placed into an [`symbol_table`].
//!
//! This is the last Step that deals with classical text procissing. All further analysis
//! happen on Tree/Graph based [irs](crate::ir)
//!
//! ## [AST lowering](crate::ast_lowering)
//!
//! During this step names are resolved to their declarations and other global information is resolved.
//! As a result an `ir` very similar to the AST called [`HIR`](crate::hir) is produced
//!
//! ## [HIR lowering](crate::hir_lowering)
//!
//! After all global information is known local transformations and type checking can take place.
//! During this pass all expression are type checked according to the rules layout out in the standard.
//! Furthremore instead of representing statements in a linear/scoped fashion they are represented as
//! a [control flow graph](crate::cfg). This step produces an [`Mir`](crate::mir::Mir) which is the end product of OpenVAF.
//!
//!
//! ## Analysis
//!
//! In practice analysing and modyfing the MIR is highly desirable. The minimal required
//! to be standard compliant is to generate all [derivatives](crate::cfg::ControlFlowGraph::calculate_all_registered_derivatives).
//! This is seperate step to allow adding additional derivatives during analysis.
//!
//! For static analysis OpenVAF currently contains a [data flow framework](crate::analysis::data_flow::framework) which is used to implement
//! [raching definitions analysis](crate::analysis::data_flow::reaching_definitions), [constant propagation](crate::analysis::constant_fold) and [backwards program slicing](crate::cfg::ControlFlowGraph::backward_slice)
//!
//!
//! # Examples
//!
//! ``` no_run
//!
//! # use open_vaf::preprocessor::{preprocess_user_facing_with_printer, std_path};
//! # use std::path::Path;
//! # use open_vaf::diagnostic::{UserResult, StandardPrinter};
//! # use open_vaf::mir::Mir;
//! # use open_vaf::ast::Ast;
//! # use open_vaf::SourceMap;
//! # use open_vaf::lints::Linter;
//! # const EXPANSION_DISCLAIMER: &str = "hint to use the backtrace option";
//!
//!  fn compile(file: &Path)-> UserResult<Mir>{
//!
//!     let paths = std_path("path_to_constants.va".into(),"path_to_disciplines.va".into());
//!     
//!     let (sm,main_file) = SourceMap::new_with_mainfile(file).expect("Failed to open mainfile");
//!
//!     let (ts, sm) = preprocess_user_facing_with_printer(sm, EXPANSION_DISCLAIMER, main_file, paths)?;
//!
//!     let ast = Ast::parse_from_token_stream_user_facing_with_printer(ts, &sm, EXPANSION_DISCLAIMER)?;
//!
//!     let hir = ast.lower_user_facing(&sm,EXPANSION_DISCLAIMER)?;
//!
//!     let diagnostic = Linter::early_user_diagnostics(&sm, EXPANSION_DISCLAIMER)?;
//!
//!     eprint!("{}", diagnostic);
//!
//!     let mut mir = hir.lower_user_facing_with_printer(&sm, EXPANSION_DISCLAIMER)?;
//!
//!
//!     mir.calculate_all_registered_derivatives().map_err(|err|err.user_facing::<StandardPrinter>(&sm,EXPANSION_DISCLAIMER))?;
//!
//!     mir.lint_unused_items();
//!
//!     let diagnostic = Linter::late_user_diagnostics(&sm, EXPANSION_DISCLAIMER)?;
//!
//!     eprint!("{}", diagnostic);
//!
//!     Ok(mir)
//!
//! }
//!
//! ```
//!
//! # Note
//!
//! This compiler is based upon the Verilog-A subsection of the [Verilog AMS 2.4](https://accellera.org/images/downloads/standards/v-ams/VAMS-LRM-2-4.pdf) standard.
//! Significant parts (documentation to be crated) of the language are still missing that may or may not be implemented in the future.
//! The goal right now is to support functionality relevant to compact modelling and **not** to write a general Verilog-AMS compiler
//!

#![allow(
    clippy::module_name_repetitions,
    clippy::unreadable_literal,
    clippy::unseparated_literal_suffix,
    clippy::pub_enum_variant_names
)]

use data_structures::sync::OnceCell;

pub use ir::ast;
pub use ir::cfg;
pub use ir::hir;
pub use ir::mir;

pub use literals::StringLiteral;
pub use sourcemap::SourceMap;

use crate::constants::Constants;
use crate::data_structures::sync::{Lock, RwLock};
use crate::lints::Linter;
use crate::literals::StringLiteralInterner;
use crate::sourcemap::{SpanInterner, SyntaxContextInterner};

/// Reexport for macros
#[doc(hidden)]
pub mod _macro_reexports {
    pub use linkme;
    pub use once_cell;
    pub use paste;
}

pub type HashMap<K, V> = ahash::AHashMap<K, V>;
pub type HashSet<T> = ahash::AHashSet<T>;

pub mod constants;

pub mod symbol;
#[macro_use]
pub mod util;
#[macro_use]
pub mod ir;
pub mod analysis;
pub mod ast_lowering;
pub mod data_structures;
pub mod derivatives;
pub mod diagnostic;
pub mod hir_lowering;
mod literals;
pub mod parser;
pub mod preprocessor;
pub mod sourcemap;
pub mod symbol_table;

pub mod lints;

#[cfg(test)]
pub mod test;

struct Globals {
    symbol_interner: Lock<symbol::Interner>,
    span_interner: Lock<SpanInterner>,
    syntax_context_interner: Lock<SyntaxContextInterner>,
    string_literal_interner: Lock<StringLiteralInterner>,
    linter: RwLock<Linter>,
    constants: OnceCell<Constants>,
}

impl Globals {
    #[must_use]
    pub(crate) fn new() -> Globals {
        Globals {
            symbol_interner: Lock::new(symbol::Interner::fresh()),
            span_interner: Lock::new(SpanInterner::default()),
            syntax_context_interner: Lock::new(SyntaxContextInterner::default()),
            string_literal_interner: Lock::new(StringLiteralInterner::default()),
            linter: RwLock::new(Linter::default()),
            constants: OnceCell::new(),
        }
    }
}

thread_local!(static GLOBALS: Globals = Globals::new());
