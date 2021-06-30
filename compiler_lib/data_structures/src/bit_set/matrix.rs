/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

/*
    Adapted from https://github.com/rust-lang/rust  under MIT-License

    LICENSE BELOW ONLY APPLIES TO THIS INDIVIDUAL FILE!

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

use crate::bit_set::{
    num_words, word_index_and_mask, BitIter, BitSet, HybridBitSet, UnionIntoHybridBitSet, Word,
    WORD_BITS,
};
use crate::index_vec::{Idx, IndexSliceExntesions, IndexVec, IndexVecExtensions};
use crate::iter;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

/// A fixed-size 2D bit matrix type with a dense representation.
///
/// `R` and `C` are index types used to identify rows and columns respectively;
/// typically newtyped `usize` wrappers, but they can also just be `usize`.
///
/// All operations that involve a row and/or column index will panic if the
/// index exceeds the relevant bound.
#[derive(Clone, Eq, PartialEq)]
pub struct BitMatrix<R: Idx, C: Idx> {
    num_rows: usize,
    num_columns: usize,
    words: Vec<Word>,
    marker: PhantomData<(R, C)>,
}

impl<R: Idx, C: Idx> BitMatrix<R, C> {
    /// Creates a new `rows x columns` matrix, initially empty.
    pub fn new(num_rows: usize, num_columns: usize) -> BitMatrix<R, C> {
        // For every element, we need one bit for every other
        // element. Round up to an even number of words.
        let words_per_row = num_words(num_columns);
        BitMatrix {
            num_rows,
            num_columns,
            words: vec![0; num_rows * words_per_row],
            marker: PhantomData,
        }
    }

    /// Creates a new matrix, with `row` used as the value for every row.
    pub fn from_row_n(row: &BitSet<C>, num_rows: usize) -> BitMatrix<R, C> {
        let num_columns = row.domain_size();
        let words_per_row = num_words(num_columns);
        assert_eq!(words_per_row, row.words().len());
        BitMatrix {
            num_rows,
            num_columns,
            words: iter::repeat(row.words())
                .take(num_rows)
                .flatten()
                .cloned()
                .collect(),
            marker: PhantomData,
        }
    }

    pub fn rows(&self) -> impl Iterator<Item = R> {
        (0..self.num_rows).map(R::from_usize)
    }

    /// The range of bits for a given row.
    fn range(&self, row: R) -> (usize, usize) {
        let words_per_row = num_words(self.num_columns);
        let start = row.index() * words_per_row;
        (start, start + words_per_row)
    }

    /// Sets the cell at `(row, column)` to true. Put another way, insert
    /// `column` to the bitset for `row`.
    ///
    /// Returns `true` if this changed the matrix.
    pub fn insert(&mut self, row: R, column: C) -> bool {
        assert!(row.index() < self.num_rows && column.index() < self.num_columns);
        let (start, _) = self.range(row);
        let (word_index, mask) = word_index_and_mask(column);
        let words = &mut self.words[..];
        let word = words[start + word_index];
        let new_word = word | mask;
        words[start + word_index] = new_word;
        word != new_word
    }

    /// Do the bits from `row` contain `column`? Put another way, is
    /// the matrix cell at `(row, column)` true?  Put yet another way,
    /// if the matrix represents (transitive) reachability, can
    /// `row` reach `column`?
    pub fn contains(&self, row: R, column: C) -> bool {
        assert!(row.index() < self.num_rows && column.index() < self.num_columns);
        let (start, _) = self.range(row);
        let (word_index, mask) = word_index_and_mask(column);
        (self.words[start + word_index] & mask) != 0
    }

    /// Returns those indices that are true in rows `a` and `b`. This
    /// is an *O*(*n*) operation where *n* is the number of elements
    /// (somewhat independent from the actual size of the
    /// intersection, in particular).
    pub fn intersect_rows(&self, row1: R, row2: R) -> Vec<C> {
        assert!(row1.index() < self.num_rows && row2.index() < self.num_rows);
        let (row1_start, row1_end) = self.range(row1);
        let (row2_start, row2_end) = self.range(row2);
        let mut result = Vec::with_capacity(self.num_columns);
        for (base, (i, j)) in (row1_start..row1_end).zip(row2_start..row2_end).enumerate() {
            let mut v = self.words[i] & self.words[j];
            for bit in 0..WORD_BITS {
                if v == 0 {
                    break;
                }
                if v & 0x1 != 0 {
                    result.push(C::from_usize(base * WORD_BITS + bit));
                }
                v >>= 1;
            }
        }
        result
    }

    /// Adds the bits from row `read` to the bits from row `write`, and
    /// returns `true` if anything changed.
    ///
    /// This is used when computing transitive reachability because if
    /// you have an edge `write -> read`, because in that case
    /// `write` can reach everything that `read` can (and
    /// potentially more).
    pub fn union_rows(&mut self, read: R, write: R) -> bool {
        assert!(read.index() < self.num_rows && write.index() < self.num_rows);
        let (read_start, read_end) = self.range(read);
        let (write_start, write_end) = self.range(write);
        let words = &mut self.words[..];
        let mut changed = false;
        for (read_index, write_index) in iter::zip(read_start..read_end, write_start..write_end) {
            let word = words[write_index];
            let new_word = word | words[read_index];
            words[write_index] = new_word;
            changed |= word != new_word;
        }
        changed
    }

    /// Adds the bits from `with` to the bits from row `write`, and
    /// returns `true` if anything changed.
    pub fn union_row_with(&mut self, with: &BitSet<C>, write: R) -> bool {
        assert!(write.index() < self.num_rows);
        assert_eq!(with.domain_size(), self.num_columns);
        let (write_start, write_end) = self.range(write);
        let mut changed = false;
        for (read_index, write_index) in iter::zip(0..with.words().len(), write_start..write_end) {
            let word = self.words[write_index];
            let new_word = word | with.words()[read_index];
            self.words[write_index] = new_word;
            changed |= word != new_word;
        }
        changed
    }

    /// Sets every cell in `row` to true.
    pub fn insert_all_into_row(&mut self, row: R) {
        assert!(row.index() < self.num_rows);
        let (start, end) = self.range(row);
        let words = &mut self.words[..];
        for index in start..end {
            words[index] = !0;
        }
        self.clear_excess_bits(row);
    }

    /// Clear excess bits in the final word of the row.
    fn clear_excess_bits(&mut self, row: R) {
        let num_bits_in_final_word = self.num_columns % WORD_BITS;
        if num_bits_in_final_word > 0 {
            let mask = (1 << num_bits_in_final_word) - 1;
            let (_, end) = self.range(row);
            let final_word_idx = end - 1;
            self.words[final_word_idx] &= mask;
        }
    }

    /// Gets a slice of the underlying words.
    pub fn words(&self) -> &[Word] {
        &self.words
    }

    /// Iterates through all the columns set to true in a given row of
    /// the matrix.
    pub fn iter(&self, row: R) -> BitIter<'_, C> {
        assert!(row.index() < self.num_rows);
        let (start, end) = self.range(row);
        BitIter::new(&self.words[start..end])
    }

    /// Returns the number of elements in `row`.
    pub fn count(&self, row: R) -> usize {
        let (start, end) = self.range(row);
        self.words[start..end]
            .iter()
            .map(|e| e.count_ones() as usize)
            .sum()
    }
}

impl<R: Idx, C: Idx> fmt::Debug for BitMatrix<R, C> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        /// Forces its contents to print in regular mode instead of alternate mode.
        struct OneLinePrinter<T>(T);
        impl<T: fmt::Debug> fmt::Debug for OneLinePrinter<T> {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(fmt, "{:?}", self.0)
            }
        }

        write!(fmt, "BitMatrix({}x{}) ", self.num_rows, self.num_columns)?;
        let items = self.rows().flat_map(|r| self.iter(r).map(move |c| (r, c)));
        fmt.debug_set().entries(items.map(OneLinePrinter)).finish()
    }
}

/// A fixed-column-size, variable-row-size 2D bit matrix with a moderately
/// sparse representation.
///
/// Initially, every row has no explicit representation. If any bit within a
/// row is set, the entire row is instantiated as `Some(<HybridBitSet>)`.
/// Furthermore, any previously uninstantiated rows prior to it will be
/// instantiated as `None`. Those prior rows may themselves become fully
/// instantiated later on if any of their bits are set.
///
/// `R` and `C` are index types used to identify rows and columns respectively;
/// typically newtyped `usize` wrappers, but they can also just be `usize`.
#[derive(Clone, PartialEq, Eq)]
pub struct SparseBitMatrix<R, C>
where
    R: Idx,
    C: Idx,
{
    num_columns: usize,
    num_rows: usize,
    rows: IndexVec<R, HybridBitSet<C>>,
}

impl<R, C> Debug for SparseBitMatrix<R, C>
where
    R: Idx,
    C: Idx,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (row, columns) in self.rows.iter_enumerated() {
            if !columns.is_empty() {
                writeln!(f, "{:?}: {:?}", row, columns)?
            }
        }
        Ok(())
    }
}

impl<R: Idx, C: Idx> SparseBitMatrix<R, C> {
    /// Creates a new empty sparse bit matrix with no rows or columns.
    pub fn new(num_rows: usize, num_columns: usize) -> Self {
        Self {
            num_columns,
            num_rows,
            rows: IndexVec::new(),
        }
    }

    pub fn inverse(&self) -> SparseBitMatrix<C, R> {
        let mut res = SparseBitMatrix::new(self.num_columns, self.num_columns);
        for (row, data) in self.rows.iter_enumerated() {
            for column in data.iter() {
                res.insert(column, row);
            }
        }

        res
    }

    pub fn ensure_row(&mut self, row: R) -> &mut HybridBitSet<C> {
        // Instantiate any missing rows up to and including row `row` with an empty HybridBitSet.
        self.rows
            .ensure_contains_elem(row, || HybridBitSet::new_empty());
        &mut self.rows[row]
    }

    /// Sets the cell at `(row, column)` to true. Put another way, insert
    /// `column` to the bitset for `row`.
    ///
    /// Returns `true` if this changed the matrix.
    pub fn insert(&mut self, row: R, column: C) -> bool {
        let num_columns = self.num_columns;
        self.ensure_row(row).insert(column, num_columns)
    }

    /// Do the bits from `row` contain `column`? Put another way, is
    /// the matrix cell at `(row, column)` true?  Put yet another way,
    /// if the matrix represents (transitive) reachability, can
    /// `row` reach `column`?
    pub fn contains(&self, row: R, column: C) -> bool {
        self.row(row).map_or(false, |r| r.contains(column))
    }

    /// Adds the bits from row `read` to the bits from row `write`, and
    /// returns `true` if anything changed.
    ///
    /// This is used when computing transitive reachability because if
    /// you have an edge `write -> read`, because in that case
    /// `write` can reach everything that `read` can (and
    /// potentially more).
    pub fn union_rows(&mut self, read: R, write: R) -> bool {
        if read == write || self.row(read).is_none() {
            return false;
        }

        self.ensure_row(write);
        let (read_row, write_row) = self.rows.pick2_mut(read, write);
        write_row.union(read_row, self.num_columns)
    }

    /// Union a row, `from`, into the `into` row.
    pub fn union_into_row(&mut self, into: R, from: &impl UnionIntoHybridBitSet<C>) -> bool {
        let col = self.num_columns;
        self.ensure_row(into).union(from, col)
    }

    /// Insert all bits in the given row.
    pub fn insert_all_into_row(&mut self, row: R) {
        let col = self.num_columns;
        self.ensure_row(row).insert_all(col);
    }

    pub fn rows(&self) -> impl Iterator<Item = R> {
        self.rows.indices()
    }

    /// Iterates through all the columns set to true in a given row of
    /// the matrix.
    pub fn iter<'a>(&'a self, row: R) -> impl Iterator<Item = C> + 'a {
        self.row(row).into_iter().flat_map(|r| r.iter())
    }

    pub fn row(&self, row: R) -> Option<&HybridBitSet<C>> {
        self.rows.get(row)
    }
}
