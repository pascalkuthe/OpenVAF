/*

 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************

 Adapted from https://github.com/rust-lang/rust src/librustc/symbol.rs under MIT-License

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
    ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
    TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
    PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
    SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
    IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
*/

use core::fmt;
use more_asserts::assert_le;
use std::clone::Clone;
use std::hash::{Hash, Hasher};
use std::iter::{FromIterator, Iterator};

use crate::{HashMap, GLOBALS};
use bumpalo::Bump;

use crate::sourcemap::span::DUMMY_SP;
use crate::sourcemap::Span;
use core::fmt::Formatter;
use core::ptr::NonNull;
use index_vec::{define_index_type, IndexVec};
use open_vaf_macros::symbols;
use std::fmt::{Debug, Display};
use std::ops::Deref;

#[derive(Copy, Clone, Debug)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    #[inline]
    #[must_use]
    /// Constructs a new identifier from a symbol and a span.
    pub const fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }

    pub const DUMMY: Self = Self {
        name: keywords::EMPTY,
        span: DUMMY_SP,
    };

    #[must_use]
    pub const fn spanned_empty(span: Span) -> Self {
        Self {
            name: keywords::EMPTY,
            span,
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    /// Maps a string to an identifier with a dummy span.
    pub fn from_str(string: &str) -> Self {
        Self::new(Symbol::intern(string), DUMMY_SP)
    }

    /// Maps a string and a span to an identifier.
    #[must_use]
    pub fn from_str_and_span(string: &str, span: Span) -> Self {
        Self::new(Symbol::intern(string), span)
    }

    #[must_use]
    pub fn without_first_quote(self) -> Self {
        Self::new(
            Symbol::intern(self.as_str().trim_start_matches('\'')),
            self.span,
        )
    }

    #[must_use]
    /// Convert the name to a `SymbolStr`. This is a slowish operation because
    /// it requires locking the symbol interner.
    pub fn as_str(self) -> SymbolStr {
        self.name.as_str()
    }
}

impl PartialEq for Ident {
    fn eq(&self, rhs: &Self) -> bool {
        self.name == rhs.name
    }
}

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.name, f)
    }
}

define_index_type! {
            /// An interned string.
            ///
            /// Internally, a `Symbol` is implemented as an index, and all operations
            /// (including hashing, equality, and ordering) operate on that index.
            pub struct Symbol = u32;

            DEBUG_FORMAT = "<Symbol {}>";

            IMPL_RAW_CONVERSIONS = true;

            DISABLE_MAX_INDEX_CHECK = !cfg!(debug_assertions);
}

impl Symbol {
    #[must_use]
    /// Maps a string to its interned representation.
    pub fn intern(string: &str) -> Self {
        with_interner(|interner| interner.intern(string))
    }

    /// Access the symbol's chars. This is a slowish operation because it
    /// requires locking the symbol interner.
    pub fn with<F: FnOnce(&str) -> R, R>(self, f: F) -> R {
        with_interner(|interner| f(interner.get(self)))
    }

    #[must_use]
    /// Convert to a `SymbolStr`. This is a slowish operation because it
    /// requires locking the symbol interner.
    ///
    /// # Safety
    pub fn as_str(self) -> SymbolStr {
        with_interner(|interner| SymbolStr::new(interner.get(self)))
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.with(|contents| f.write_str(contents))
    }
}

// The `&'static str`s in this type actually point into the arena.
#[derive(Default)]
pub struct Interner {
    arena: Bump,
    names: HashMap<&'static str, Symbol>,
    strings: IndexVec<Symbol, &'static str>,
}

impl Interner {
    fn prefill(init: &[&'static str]) -> Self {
        Self {
            strings: IndexVec::from_iter(init.iter().copied()),
            names: init.iter().copied().zip((0..).map(Symbol::new)).collect(),
            arena: Bump::with_capacity(2048),
        }
    }

    pub fn intern(&mut self, string: &str) -> Symbol {
        if let Some(&name) = self.names.get(string) {
            return name;
        }

        assert_le!(self.strings.len(), u32::MAX as usize);
        #[allow(clippy::cast_possible_truncation)]
        let string = self.arena.alloc_str(string);
        // It is safe to extend the arena allocation to `'static` because we only access
        // these while the arena is still alive.
        let string: &'static str = unsafe { &*(string as *const str) };
        let symbol = self.strings.push(string);
        self.names.insert(string, symbol);
        symbol
    }

    // Get the symbol as a string. `Symbol::as_str()` should be used in
    // preference to this function.
    pub fn get(&self, symbol: Symbol) -> &str {
        self.strings[symbol]
    }
}

symbols! {
    EMPTY: " ",
    OpenVAF,
    temperature: "$temperature",
    flow,
    potential,
    abstol,
    access,
    units,
    idt_nature,
    ddt_nature,
    desc,
    domain,

}

#[inline]
pub(super) fn with_interner<T, F: FnOnce(&mut Interner) -> T>(f: F) -> T {
    GLOBALS.with(|globals| f(&mut *globals.symbol_interner.lock()))
}

/// An alternative to `Symbol`, useful when the chars within the symbol need to
/// be accessed. It deliberately has limited functionality and should only be
/// used for temporary values.
///
/// Because the interner outlives any thread which uses this type, we can
/// safely treat `string` which points to interner data, as an immortal string,
/// as long as this type never crosses between threads.
//
// by creating a new thread right after constructing the interner.
#[derive(Clone, Eq, PartialOrd, Ord)]
pub struct SymbolStr {
    string: NonNull<str>,
}
impl SymbolStr {
    fn new(interner_str: &str) -> Self {
        unsafe {
            // this is save since we only use NonNull to indicate that SymbolStr is not send
            // &str is always non null anyway and the const to mut case is also save since self.string() is only ever used in deref as reading
            let raw = interner_str as *const str as *mut str;
            Self {
                string: NonNull::new_unchecked(raw),
            }
        }
    }
}

// This impl allows a `SymbolStr` to be directly equated with a `String` or
// `&str`.
impl<T: std::ops::Deref<Target = str>> std::cmp::PartialEq<T> for SymbolStr {
    fn eq(&self, other: &T) -> bool {
        self.deref() == &**other
    }
}

/// This impl means that if `ss` is a `SymbolStr`:
/// - `*ss` is a `str`;
/// - `&*ss` is a `&str`;
/// - `&ss as &str` is a `&str`, which means that `&ss` can be passed to a
///   function expecting a `&str`.
impl Deref for SymbolStr {
    type Target = str;
    #[inline]
    fn deref(&self) -> &str {
        // This is save since this is only created for interned strings which are bump allocated and therefore never moved
        // Since SymbolStr contains a raw pointer and is therefore not Sync And Senc so the threadlocal will always outlive any SymbolStr in Safe code
        unsafe { self.string.as_ref() }
    }
}

impl Debug for SymbolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

impl Display for SymbolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self.deref(), f)
    }
}
