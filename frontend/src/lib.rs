//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

#![allow(
    clippy::module_name_repetitions,
    clippy::unreadable_literal,
    clippy::unseparated_literal_suffix,
    clippy::pub_enum_variant_names
)]

/// Reexport for macros
pub mod _macro_reexports {
    pub use linkme;
    pub use once_cell;
    pub use paste;
}

pub type HashMap<K, V> = ahash::AHashMap<K, V>;
pub type HashSet<T> = ahash::AHashSet<T>;

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
pub(crate) use parser::Parser;
#[doc(inline)]
pub use sourcemap::Span;

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
mod hir_lowering;
mod literals;
pub mod parser;
pub mod preprocessor;
pub mod sourcemap;
pub mod symbol_table;

pub mod lints;

use crate::data_structures::sync::{Lock, RwLock};
use crate::literals::StringLiteralInterner;
use crate::sourcemap::{SpanInterner, SyntaxContextInterner};

use crate::lints::Linter;
pub use literals::StringLiteral;
pub use sourcemap::SourceMap;

use crate::constants::Constants;
use data_structures::sync::OnceCell;

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
