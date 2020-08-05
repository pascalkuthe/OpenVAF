/*

 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************

 Adapted from https://github.com/rust-lang/rust src/librustc_span/lib.rs and src/librustc_span/span_encoding.rs under MIT-License:

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

// Spans are encoded using 1-bit tag and 2 different encoding formats (one for each tag value).
// One format is used for keeping span data inline,
// another contains index into an out-of-line span interner.
// The encoding format for inline spans were obtained by optimizing over crates in rustc/libstd.
// See https://internals.rust-lang.org/t/rfc-compiler-refactoring-spans/1357/28

use crate::sourcemap::context::SyntaxContext;
use crate::sourcemap::BytePos;
use more_asserts::{assert_le, debug_assert_le};
use openvaf_data_structures::sync::Lock;
use openvaf_data_structures::HashMap;

/// A compressed span.
///
/// `SpanData` is 12 bytes, which is a bit too big to stick everywhere. `Span`
/// is a form that only takes up 8 bytes, with less space for the length and
/// context. The vast majority (99.9%+) of `SpanData` instances will fit within
/// those 8 bytes; any `SpanData` whose fields don't fit into a `Span` are
/// stored in a separate interner table, and the `Span` will index into that
/// table. Interning is rare enough that the cost is low, but common enough
/// that the code is exercised regularly.
///
/// An earlier version of this code used only 4 bytes for `Span`, but that was
/// slower because only 80--90% of spans could be stored inline (even less in
/// very large crates) and so the interner was used a lot more.
///
/// Inline (compressed) format:
/// - `span.base_or_index == span_data.lo`
/// - `span.len_or_tag == len == span_data.hi - span_data.lo` (must be `<= MAX_LEN`)
/// - `span.ctxt == span_data.ctxt` (must be `<= MAX_CTXT`)
///
/// Interned format:
/// - `span.base_or_index == index` (indexes into the interner table)
/// - `span.len_or_tag == LEN_TAG` (high bit set, all other bits are zero)
/// - `span.ctxt == 0`
///
/// The inline form uses 0 for the tag value (rather than 1) so that we don't
/// need to mask out the tag bit when getting the length, and so that the
/// dummy span can be all zeroes.
///
/// Notes about the choice of field sizes:
/// - `base` is 32 bits in both `Span` and `SpanData`, which means that `base`
///   values never cause interning. The number of bits needed for `base`
///   depends on the crate size. 32 bits allows up to 4 GiB of code in a crate.
///   `script-servo` is the largest crate in `rustc-perf`, requiring 26 bits
///   for some spans.
/// - `len` is 15 bits in `Span` (a u16, minus 1 bit for the tag) and 32 bits
///   in `SpanData`, which means that large `len` values will cause interning.
///   The number of bits needed for `len` does not depend on the crate size.
///   The most common number of bits for `len` are 0--7, with a peak usually at
///   3 or 4, and then it drops off quickly from 8 onwards. 15 bits is enough
///   for 99.99%+ of cases, but larger values (sometimes 20+ bits) might occur
///   dozens of times in a typical crate.
/// - `ctxt` is 16 bits in `Span` and 32 bits in `SpanData`, which means that
///   large `ctxt` values will cause interning. The number of bits needed for
///   `ctxt` values depend partly on the crate size and partly on the form of
///   the code. No crates in `rustc-perf` need more than 15 bits for `ctxt`,
///   but larger crates might need more than 16 bits.
///
#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct Span {
    base_or_index: u32,
    len_or_tag: u16,
    ctxt_or_zero: u16,
}

const LEN_TAG: u16 = 0b1000_0000_0000_0000;
const MAX_LEN: u32 = 0b0111_1111_1111_1111;
const MAX_CTXT: u32 = 0b1111_1111_1111_1111;

/// Dummy span, both position and length are zero, syntax context is the root file (also 0)
pub const DUMMY_SP: Span = Span {
    base_or_index: 0,
    len_or_tag: 0,
    ctxt_or_zero: SyntaxContext::ROOT_U16,
};

impl Default for Span {
    fn default() -> Self {
        DUMMY_SP
    }
}

impl Span {
    #[inline]
    pub fn new(mut lo: BytePos, mut hi: BytePos, ctxt: SyntaxContext) -> Self {
        if lo > hi {
            std::mem::swap(&mut lo, &mut hi);
        }

        let (base, len, ctxt2) = (lo.raw(), hi.raw() - lo.raw(), ctxt.raw());

        if len <= MAX_LEN && ctxt2 <= MAX_CTXT {
            // Inline format.
            Span {
                base_or_index: base,
                len_or_tag: len as u16,
                ctxt_or_zero: ctxt2 as u16,
            }
        } else {
            // Interned format.
            let index = with_span_interner(|interner| interner.intern(&SpanData { lo, hi, ctxt }));
            Span {
                base_or_index: index,
                len_or_tag: LEN_TAG,
                ctxt_or_zero: 0,
            }
        }
    }

    #[inline]
    pub fn data(self) -> SpanData {
        if self.len_or_tag == LEN_TAG {
            // Interned format.
            debug_assert_eq!(self.ctxt_or_zero, 0);
            let index = self.base_or_index;
            with_span_interner(|interner| *interner.get(index))
        } else {
            // Inline format.
            debug_assert_le!(self.len_or_tag as u32, MAX_LEN);
            SpanData {
                lo: BytePos::from_raw_unchecked(self.base_or_index),
                hi: BytePos::from_raw_unchecked(self.base_or_index + self.len_or_tag as u32),
                ctxt: SyntaxContext::from_raw_unchecked(self.ctxt_or_zero as u32),
            }
        }
    }

    #[inline]
    pub fn extend(self, to: Self) -> Span {
        self.data().extend(to.data()).compress()
    }
}

#[derive(Default)]
pub struct SpanInterner {
    spans: HashMap<SpanData, u32>,
    span_data: Vec<SpanData>,
}

impl SpanInterner {
    fn intern(&mut self, span_data: &SpanData) -> u32 {
        if let Some(index) = self.spans.get(span_data) {
            return *index;
        }

        let index = self.spans.len() as u32;
        self.span_data.push(*span_data);
        self.spans.insert(*span_data, index);
        index
    }

    #[inline]
    fn get(&self, index: u32) -> &SpanData {
        &self.span_data[index as usize]
    }
}

// If an interner exists, return it. Otherwise, prepare a fresh one.
#[inline]
fn with_span_interner<T, F: FnOnce(&mut SpanInterner) -> T>(f: F) -> T {
    with_span_interner_lock(|interner_lock| f(&mut *interner_lock.lock()))
}

session_data!(static span_interner_lock: Lock<SpanInterner> = Lock::new(SpanInterner::default()));

/// Spans represent a region of code, used for error reporting. Positions in spans
/// are *absolute* positions from the beginning of the `SourceMap`, not positions
/// relative to source files. Methods on the `SourceMap` can be used to relate spans back
/// to the original source.
/// You must be careful if the span crosses more than one file - you will not be
/// able to use many of the functions on spans in `SourceMap` and you cannot assume
/// that the length of the `span = hi - lo`; there may be space in the `BytePos`
/// range between files.
///
/// `SpanData` is public because `Span` uses a thread-local interner and can't be
/// sent to other threads, but some pieces of performance infra run in a separate thread.
/// Using `Span` is generally preferred.

#[derive(Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd, Debug)]
pub struct SpanData {
    pub lo: BytePos,
    pub hi: BytePos,
    /// Information about where the code came from
    pub ctxt: SyntaxContext,
}

#[allow(clippy::inline_always)]
impl SpanData {
    #[inline(always)]
    #[must_use]
    pub fn with_lo(&self, lo: BytePos) -> Span {
        Span::new(lo, self.hi, self.ctxt)
    }

    #[inline(always)]
    #[must_use]
    pub fn with_hi(&self, hi: BytePos) -> Span {
        Span::new(self.lo, hi, self.ctxt)
    }

    #[inline(always)]
    #[must_use]
    pub fn with_ctxt(&self, ctxt: SyntaxContext) -> Span {
        Span::new(self.lo, self.hi, ctxt)
    }

    #[inline(always)]
    #[must_use]
    pub fn compress(&self) -> Span {
        Span::new(self.lo, self.hi, self.ctxt)
    }

    /// Extends `self` to reach to also include `to`
    /// Note that this is a non-trivial algorithm and should be called conservatively
    #[must_use]
    pub fn extend(self, to: Self) -> Self {
        if self.ctxt == to.ctxt {
            Self { hi: to.hi, ..self }
        } else {
            let (ctxt, lo, hi) = self.ctxt.lowest_common_parent(to.ctxt);
            let lo = lo.map_or(self.lo, |lo| lo.data().lo);
            let hi = hi.map_or(to.hi, |hi| hi.data().hi);

            Self { lo, hi, ctxt }
        }
    }
}
