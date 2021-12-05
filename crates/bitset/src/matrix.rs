use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::{fmt, iter};

use stdx::vec::{SliceExntesions, VecExtensions};

use crate::{
    num_words, word_index_and_mask, BitIter, BitSet, HybridBitSet, UnionIntoHybridBitSet, Word,
    WORD_BITS,
};

/// A fixed-size 2D bit matrix type with a dense representation.
///
/// `R` and `C` are index types used to identify rows and columns respectively;
/// typically newtyped `usize` wrappers, but they can also just be `usize`.
///
/// All operations that involve a row and/or column index will panic if the
/// index exceeds the relevant bound.
#[derive(Eq, PartialEq)]
pub struct BitMatrix<R: From<usize>, C: Into<usize>> {
    num_rows: usize,
    num_columns: usize,
    words: Vec<Word>,
    marker: PhantomData<(R, C)>,
}

impl<
        R: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
        C: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
    > Clone for BitMatrix<R, C>
{
    fn clone_from(&mut self, source: &Self) {
        self.num_rows = source.num_rows;
        self.num_columns = source.num_columns;
        self.words.clone_from(&source.words);
    }

    fn clone(&self) -> Self {
        Self {
            num_rows: self.num_rows,
            num_columns: self.num_columns,
            words: self.words.clone(),
            marker: self.marker,
        }
    }
}

impl<
        R: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
        C: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
    > BitMatrix<R, C>
{
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
            words: iter::repeat(row.words()).take(num_rows).flatten().cloned().collect(),
            marker: PhantomData,
        }
    }

    pub fn rows(&self) -> impl Iterator<Item = R> {
        (0..self.num_rows).map(R::from)
    }

    /// The range of bits for a given row.
    fn range(&self, row: R) -> (usize, usize) {
        let words_per_row = num_words(self.num_columns);
        let start = row.into() * words_per_row;
        (start, start + words_per_row)
    }

    /// Sets the cell at `(row, column)` to true. Put another way, insert
    /// `column` to the bitset for `row`.
    ///
    /// Returns `true` if this changed the matrix.
    pub fn insert(&mut self, row: R, column: C) -> bool {
        assert!(row.into() < self.num_rows && column.into() < self.num_columns);
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
        assert!(row.into() < self.num_rows && column.into() < self.num_columns);
        let (start, _) = self.range(row);
        let (word_index, mask) = word_index_and_mask(column);
        (self.words[start + word_index] & mask) != 0
    }

    /// Returns those indices that are true in rows `a` and `b`. This
    /// is an *O*(*n*) operation where *n* is the number of elements
    /// (somewhat independent from the actual size of the
    /// intersection, in particular).
    pub fn intersect_rows(&self, row1: R, row2: R) -> Vec<C> {
        assert!(row1.into() < self.num_rows && row2.into() < self.num_rows);
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
                    result.push(C::from(base * WORD_BITS + bit));
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
        assert!(read.into() < self.num_rows && write.into() < self.num_rows);
        let (read_start, read_end) = self.range(read);
        let (write_start, write_end) = self.range(write);
        let words = &mut self.words[..];
        let mut changed = false;
        for (read_index, write_index) in Iterator::zip(read_start..read_end, write_start..write_end)
        {
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
        assert!(write.into() < self.num_rows);
        assert_eq!(with.domain_size(), self.num_columns);
        let (write_start, write_end) = self.range(write);
        let mut changed = false;
        for (read_index, write_index) in
            Iterator::zip(0..with.words().len(), write_start..write_end)
        {
            let word = self.words[write_index];
            let new_word = word | with.words()[read_index];
            self.words[write_index] = new_word;
            changed |= word != new_word;
        }
        changed
    }

    /// Sets every cell in `row` to true.
    pub fn insert_all_into_row(&mut self, row: R) {
        assert!(row.into() < self.num_rows);
        let (start, end) = self.range(row);
        let words = &mut self.words[..];
        for word in &mut words[start..end] {
            *word = !0;
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
        assert!(row.into() < self.num_rows);
        let (start, end) = self.range(row);
        BitIter::new(&self.words[start..end])
    }

    /// Returns the number of elements in `row`.
    pub fn count(&self, row: R) -> usize {
        let (start, end) = self.range(row);
        self.words[start..end].iter().map(|e| e.count_ones() as usize).sum()
    }
}

impl<R, C> fmt::Debug for BitMatrix<R, C>
where
    R: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug + Debug,
    C: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug + Debug,
{
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
#[derive(PartialEq, Eq)]
pub struct SparseBitMatrix<R, C>
where
    R: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
    C: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
{
    num_columns: usize,
    num_rows: usize,
    rows: Vec<HybridBitSet<C>>,
    _row_ty: PhantomData<fn() -> R>,
}

impl<
        R: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
        C: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
    > Clone for SparseBitMatrix<R, C>
{
    fn clone_from(&mut self, source: &Self) {
        self.rows.clone_from(&source.rows);
        self.num_columns = source.num_columns;
        self.num_rows = source.num_rows;
    }

    fn clone(&self) -> Self {
        Self {
            num_columns: self.num_columns,
            num_rows: self.num_rows,
            rows: self.rows.clone(),
            _row_ty: PhantomData,
        }
    }
}

impl<R, C> Debug for SparseBitMatrix<R, C>
where
    R: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug + Debug,
    C: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (row, columns) in self.rows.iter().enumerate() {
            if !columns.is_empty() {
                writeln!(f, "{:?}: {:?}", R::from(row), columns)?
            }
        }
        Ok(())
    }
}

impl<R, C> SparseBitMatrix<R, C>
where
    R: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
    C: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
{
    /// Creates a new empty sparse bit matrix with no rows or columns.
    pub fn new(num_rows: usize, num_columns: usize) -> Self {
        Self { num_columns, num_rows, rows: Vec::new(), _row_ty: PhantomData }
    }

    pub fn inverse(&self) -> SparseBitMatrix<C, R> {
        let mut res = SparseBitMatrix::new(self.num_columns, self.num_columns);
        for (row, data) in self.rows.iter().enumerate() {
            for column in data.iter() {
                res.insert(column, R::from(row));
            }
        }

        res
    }

    pub fn ensure_row(&mut self, row: R) -> &mut HybridBitSet<C> {
        // Instantiate any missing rows up to and including row `row` with an empty HybridBitSet.
        self.rows.ensure_contains_elem(row.into(), HybridBitSet::new_empty);
        &mut self.rows[row.into()]
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
        (0..self.rows.len()).map(R::from)
    }

    /// Iterates through all the columns set to true in a given row of
    /// the matrix.
    pub fn iter(&self, row: R) -> impl Iterator<Item = C> + '_ {
        self.row(row).into_iter().flat_map(|r| r.iter())
    }

    pub fn row(&self, row: R) -> Option<&HybridBitSet<C>> {
        self.rows.get(row.into())
    }
}

impl<R, C> SparseBitMatrix<R, C>
where
    R: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug + PartialEq,
    C: From<usize> + Into<usize> + Copy + PartialOrd + PartialEq + Debug,
{
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
        let (read_row, write_row) = self.rows.pick2_mut(read.into(), write.into());
        write_row.union(read_row, self.num_columns)
    }
}
