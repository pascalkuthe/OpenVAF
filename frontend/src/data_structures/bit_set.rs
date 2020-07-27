/*
 * ******************************************************************************************
 *  Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
*/

// Provides a wrapper around fixed bit_set similar to index vec

use core::fmt::Binary;
use core::iter::FromIterator;
use core::marker::PhantomData;
use core::ops::{BitAndAssign, BitOr, BitXor, BitXorAssign, Index};
use fixedbitset::{FixedBitSet, IndexRange};
use index_vec::Idx;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::{BitAnd, BitOrAssign};

mod hybrid;
mod matrix;
mod sparse;

use crate::data_structures::bit_set::sparse::{SparseBitSet, SPARSE_MAX};
pub use hybrid::HybridBitSet;
pub use matrix::SparseBitSetMatrix;
use std::mem::size_of;

//TODO switch to usize/u64
//BLOCKS upstream
type Block = u32;
const BITS: usize = size_of::<Block>() * 8;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BitSet<I: Idx + From<usize>> {
    internal: FixedBitSet,
    tag: PhantomData<fn(&I)>,
}

impl<I: Idx + From<usize>> BitSet<I> {
    /// Create a new empty `BitSet` which can hold elements `x < end_index`
    pub fn new_empty(end_index: I) -> Self {
        Self {
            internal: FixedBitSet::with_capacity(end_index.index()),
            tag: PhantomData,
        }
    }

    /// Create a new empty `BitSet` which can hold elements `x < end_index`
    pub fn new_filled(end_index: I) -> Self {
        let mut res = Self {
            internal: FixedBitSet::with_capacity(end_index.index()),
            tag: PhantomData,
        };
        res.enable_all();
        res
    }

    /// Create a new `BitSet` with a specific number of bits,
    /// initialized from provided blocks.
    ///
    /// If the blocks are not the exact size needed for the capacity
    /// they will be padded with zeros (if shorter) or truncated to
    /// the capacity (if longer).
    pub fn with_capacity_and_blocks<Iter: IntoIterator<Item = Block>>(
        bits: I,
        blocks: Iter,
    ) -> Self {
        Self {
            internal: FixedBitSet::with_capacity_and_blocks(bits.index(), blocks),
            tag: PhantomData,
        }
    }

    /// Grow capacity to **bits**, all new bits initialized to zero
    pub fn grow(&mut self, len_idx: I) {
        self.internal.grow(len_idx.index())
    }

    /// Return the length of the `FixedBitSet` in bits.
    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        self.internal.len()
    }

    /// Return the length of the `FixedBitSet` in bits.
    #[inline]
    #[must_use]
    pub fn len_idx(&self) -> I {
        self.internal.len().into()
    }
    /// Return the length of the `FixedBitSet` in bits.
    #[inline]
    #[must_use]
    pub fn max(&self) -> I {
        I::from_usize(self.internal.len())
    }

    /// Return **true** if the bit is enabled in the `BitSet`,
    /// **false** otherwise.
    ///
    /// Note: bits outside the capacity are always disabled.
    ///
    /// Note: Also available with index syntax: `bitset[bit]`.
    #[inline]
    pub fn contains(&self, bit: I) -> bool {
        self.internal.contains(bit.index())
    }

    /// Clear all bits.
    #[inline]
    pub fn clear(&mut self) {
        self.internal.clear()
    }

    /// Enable `bit`.
    ///
    /// **Panics** if **bit** is out of bounds.
    #[inline]
    pub fn insert(&mut self, bit: I) {
        self.internal.insert(bit.index())
    }

    /// Disable `bit`.
    ///
    /// **Panics** if **bit** is out of bounds.
    #[inline]
    pub fn remove(&mut self, bit: I) -> bool {
        let prev = self.internal[bit.index()];
        self.internal.set(bit.index(), false);
        prev
    }

    /// Enable `bit`, and return its previous value.
    ///
    /// **Panics** if `bit` is out of bounds.
    ///
    #[inline]
    pub fn put(&mut self, bit: I) -> bool {
        self.internal.put(bit.index())
    }

    /// Toggle `bit` (inverting its state).
    ///
    /// `Panics` if `bit` is out of bounds
    #[inline]
    pub fn toggle(&mut self, bit: I) {
        self.internal.toggle(bit.index())
    }

    /// **Panics** if **bit** is out of bounds.
    #[inline]
    pub fn set(&mut self, bit: I, enabled: bool) {
        self.internal.set(bit.index(), enabled)
    }

    /// Copies boolean value from specified bit to the specified bit.
    ///
    /// **Panics** if **to** is out of bounds.
    #[inline]
    pub fn copy_bit(&mut self, from: I, to: I) {
        self.internal.copy_bit(from.index(), to.index())
    }

    /// Count the number of set bits in the given bit range.
    ///
    /// Use `..` to count the whole content of the bitset.
    ///
    /// # Panics
    /// if the range extends past the end of the bitset.
    #[inline]
    pub fn count_ones<T: IndexRange<I>>(&self, range: T) -> usize {
        let start = range.start().map_or(0, index_vec::Idx::index);
        let end = range
            .end()
            .map_or(self.internal.len(), index_vec::Idx::index);
        self.internal.count_ones(start..end)
    }

    /// Sets every bit in the given range to the given state (`enabled`)
    ///
    /// Use `..` to set the whole bitset.
    ///
    /// **Panics** if the range extends past the end of the bitset.
    #[inline]
    pub fn set_range<T: IndexRange<I>>(&mut self, range: T, enabled: bool) {
        let start = range.start().map_or(0, index_vec::Idx::index);
        let end = range
            .end()
            .map_or(self.internal.len(), index_vec::Idx::index);
        self.internal.set_range(start..end, enabled)
    }

    /// Enables every bit in the given range.
    ///
    /// Use `..` to make the whole bitset ones.
    ///
    /// **Panics** if the range extends past the end of the bitset.
    #[inline]
    pub fn insert_range<T: IndexRange<I>>(&mut self, range: T) {
        let start = range.start().map_or(0, index_vec::Idx::index);
        let end = range
            .end()
            .map_or(self.internal.len(), index_vec::Idx::index);
        self.internal.insert_range(start..end)
    }

    /// Equivalent to [`insert_range(..)`](crate::data_structures::bit_set::BitSet::insert_range) but faster
    #[inline]
    pub fn enable_all(&mut self) {
        let len = self.as_slice().len();

        for block in self.as_mut_slice()[..len - 1].iter_mut() {
            *block = Block::MAX
        }

        self.as_mut_slice()[len - 1] = 2u32.pow((self.len() % BITS) as u32) - 1;
    }

    /// Toggles (inverts) every bit in the given range.
    ///
    /// Use `..` to toggle the whole bitset.
    ///
    /// **Panics** if the range extends past the end of the bitset.
    #[inline]
    pub fn toggle_range<T: IndexRange<I>>(&mut self, range: T) {
        let start = range.start().map_or(0, |idx| idx.index());
        let end = range
            .end()
            .map_or(self.internal.len(), index_vec::Idx::index);
        self.internal.toggle_range(start..end)
    }

    /// View the bitset as a slice of `u32` blocks
    #[inline]
    #[must_use]
    pub fn as_slice(&self) -> &[u32] {
        self.internal.as_slice()
    }

    #[inline]
    pub fn union_with<T: BitSetOperations<I>>(&mut self, other: &T) {
        other.union_into(self)
    }

    #[inline]
    pub fn intersect_with<T: BitSetOperations<I>>(&mut self, other: &T) {
        other.intersect_into(self)
    }

    #[inline]
    pub fn difference_with<T: BitSetOperations<I>>(&mut self, other: &T) {
        other.difference_into(self)
    }

    #[inline]
    pub fn symmetric_difference_with<T: BitSetOperations<I>>(&mut self, other: &T) {
        other.symmetric_difference_into(self)
    }

    /// View the bitset as a mutable slice of `u32` blocks. Writing past the bitlength in the last
    /// will cause `contains` to return potentially incorrect results for bits past the bitlength.
    #[inline]
    #[must_use]
    pub fn as_mut_slice(&mut self) -> &mut [u32] {
        self.internal.as_mut_slice()
    }

    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.as_slice().iter().all(|block| *block == 0)
    }

    /// Iterates over all enabled bits.
    ///
    /// Iterator element is the index of the `1` bit, type `usize`.
    #[inline]
    #[must_use]
    pub fn ones(&self) -> Ones<'_, I> {
        Ones {
            iter: self.internal.ones(),
            tag: PhantomData,
        }
    }

    /// Returns a lazy iterator over the intersection of two `FixedBitSet`s
    #[must_use]
    pub fn intersection<'a>(&'a self, other: &'a Self) -> Intersection<'a, I> {
        Intersection {
            iter: self.internal.intersection(&other.internal),
            tag: PhantomData,
        }
    }

    /// Returns a lazy iterator over the union of two `FixedBitSet`s.
    #[must_use]
    pub fn union<'a>(&'a self, other: &'a Self) -> Union<'a, I> {
        Union {
            iter: self.internal.union(&other.internal),
            tag: PhantomData,
        }
    }

    /// Returns a lazy iterator over the difference of two `FixedBitSet`s. The difference of `a`
    /// and `b` is the elements of `a` which are not in `b`.
    #[must_use]
    pub fn difference<'a>(&'a self, other: &'a Self) -> Difference<'a, I> {
        Difference {
            iter: self.internal.difference(&other.internal),
            tag: PhantomData,
        }
    }

    /// Returns a lazy iterator over the symmetric difference of two `FixedBitSet`s.
    /// The symmetric difference of `a` and `b` is the elements of one, but not both, sets.
    #[must_use]
    pub fn symmetric_difference<'a>(&'a self, other: &'a Self) -> SymmetricDifference<'a, I> {
        SymmetricDifference {
            iter: self.internal.symmetric_difference(&other.internal),
            tag: PhantomData,
        }
    }

    /// Returns `true` if `self` has no elements in common with `other`. This
    /// is equivalent to checking for an empty intersection.
    #[must_use]
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.internal.is_disjoint(&other.internal)
    }

    /// Returns `true` if the set is a subset of another, i.e. `other` contains
    /// at least all the values in `self`.
    #[must_use]
    pub fn is_subset(&self, other: &Self) -> bool {
        self.internal.is_subset(&other.internal)
    }

    /// Returns `true` if the set is a superset of another, i.e. `self` contains
    /// at least all the values in `other`.
    #[must_use]
    pub fn is_superset(&self, other: &Self) -> bool {
        self.internal.is_superset(&other.internal)
    }

    /// Tries to convert `self` to a more compact representation
    /// # Returns
    /// `None` if this set is empty
    /// `Some(HybridBitSet::Sparse)` if self is sparse
    /// `Some(HybridBitSet::Dense(self.clone))`
    pub fn to_hybrid(&self) -> Option<HybridBitSet<I>> {
        match self.count_ones(..) {
            0 => None,
            1..=SPARSE_MAX => {
                let mut res = SparseBitSet::new_empty(self.len_idx());
                for x in self.ones() {
                    res.insert(x)
                }
                Some(HybridBitSet::Sparse(res))
            }
            _ => Some(HybridBitSet::Dense(self.clone())),
        }
    }
}

impl<I: Idx + From<usize>> Binary for BitSet<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        Binary::fmt(&self.internal, f)
    }
}

impl<I: Idx + From<usize> + Display> Display for BitSet<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str("{")?;
        for idx in self.ones() {
            Display::fmt(&idx, f)?;
            f.write_str(",")?;
        }
        f.write_str("}")
    }
}

/// An iterator producing elements in the difference of two sets.
///
/// This struct is created by the [`FixedBitSet::difference`] method.
pub struct Difference<'a, I: Idx + From<usize>> {
    iter: fixedbitset::Difference<'a>,
    tag: PhantomData<fn(&I)>,
}

impl<'a, I: Idx + From<usize>> Iterator for Difference<'a, I> {
    type Item = I;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|idx| idx.into())
    }
}

/// An iterator producing elements in the symmetric difference of two sets.
///
/// This struct is created by the [`FixedBitSet::symmetric_difference`] method.
pub struct SymmetricDifference<'a, I: Idx + From<usize>> {
    iter: fixedbitset::SymmetricDifference<'a>,
    tag: PhantomData<fn(&I)>,
}

impl<'a, I: Idx + From<usize>> Iterator for SymmetricDifference<'a, I> {
    type Item = I;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|idx| idx.into())
    }
}

/// An iterator producing elements in the intersection of two sets.
///
/// This struct is created by the [`FixedBitSet::intersection`] method.
pub struct Intersection<'a, I: Idx + From<usize>> {
    iter: fixedbitset::Intersection<'a>,
    tag: PhantomData<fn(&I)>,
}

impl<'a, I: Idx + From<usize>> Iterator for Intersection<'a, I> {
    type Item = I; // the bit position of the '1'

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|idx| idx.into())
    }
}

/// An iterator producing elements in the union of two sets.
///
/// This struct is created by the [`FixedBitSet::union`] method.
pub struct Union<'a, I: Idx + From<usize>> {
    iter: fixedbitset::Union<'a>,
    tag: PhantomData<fn(&I)>,
}

impl<'a, I: Idx + From<usize>> Iterator for Union<'a, I> {
    type Item = I;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|idx| idx.into())
    }
}

/// An  iterator producing the indices of the set bit in a set.
///
/// This struct is created by the [`FixedBitSet::ones`] method.
pub struct Ones<'a, I: Idx + From<usize>> {
    iter: fixedbitset::Ones<'a>,
    tag: PhantomData<fn(&I)>,
}

impl<'a, I: Idx + From<usize>> Iterator for Ones<'a, I> {
    type Item = I; // the bit position of the '1'

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|idx| idx.into())
    }
}

impl<I: Idx + From<usize>> Clone for BitSet<I> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            internal: self.internal.clone(),
            tag: PhantomData,
        }
    }
}

/// Return `true` if the bit is enabled in the bitset,
/// or `false` otherwise.
///
/// Note: bits outside the capacity are always disabled, and thus
/// indexing a `FixedBitSet` will not panic.
impl<I: Idx + From<usize>> Index<I> for BitSet<I> {
    type Output = bool;

    #[inline]
    fn index(&self, bit: I) -> &bool {
        &self.internal[bit.index()]
    }
}

/// Sets the bit at index `i` to `true` for each item `i` in the input `src`.
impl<I: Idx + From<usize>> Extend<I> for BitSet<I> {
    fn extend<Iter: IntoIterator<Item = I>>(&mut self, src: Iter) {
        let src = src.into_iter().map(|id| id.index());
        self.internal.extend(src)
    }
}

/// Return a `FixedBitSet` containing bits set to **true** for every bit index in
/// the iterator, other bits are set to **false**.
impl<I: Idx + From<usize>> FromIterator<I> for BitSet<I> {
    fn from_iter<Iter: IntoIterator<Item = I>>(src: Iter) -> Self {
        let iter = src.into_iter();
        let mut res = BitSet::new_empty(iter.size_hint().0.into());
        res.extend(iter);
        res
    }
}

impl<'a, I: Idx + From<usize>> BitAnd for &'a BitSet<I> {
    type Output = BitSet<I>;
    fn bitand(self, other: &BitSet<I>) -> BitSet<I> {
        BitSet {
            internal: (&self.internal) & (&other.internal),
            tag: PhantomData,
        }
    }
}

impl<'a, I: Idx + From<usize>> BitAndAssign for BitSet<I> {
    fn bitand_assign(&mut self, other: Self) {
        self.internal &= other.internal
    }
}

impl<'a, I: Idx + From<usize>> BitOr for &'a BitSet<I> {
    type Output = BitSet<I>;
    fn bitor(self, other: &BitSet<I>) -> BitSet<I> {
        BitSet {
            internal: (&self.internal) | (&other.internal),
            tag: PhantomData,
        }
    }
}

impl<'a, I: Idx + From<usize>> BitOrAssign for BitSet<I> {
    fn bitor_assign(&mut self, other: Self) {
        self.internal |= other.internal
    }
}

impl<'a, I: Idx + From<usize>> BitXor for &'a BitSet<I> {
    type Output = BitSet<I>;
    fn bitxor(self, other: &BitSet<I>) -> BitSet<I> {
        BitSet {
            internal: (&self.internal) ^ (&other.internal),
            tag: PhantomData,
        }
    }
}

impl<'a, I: Idx + From<usize>> BitXorAssign for BitSet<I> {
    fn bitxor_assign(&mut self, other: Self) {
        self.internal ^= other.internal
    }
}

pub trait BitSetOperations<I: Idx + From<usize>> {
    /// In-place union into a BitSet
    fn union_into(&self, dst: &mut BitSet<I>);

    /// In-place intersection into a BitSet
    fn intersect_into(&self, dst: &mut BitSet<I>);

    /// In-place difference into a BitSet
    fn difference_into(&self, dst: &mut BitSet<I>);

    /// In-place symmetric difference into a BitSet
    fn symmetric_difference_into(&self, dst: &mut BitSet<I>);
}

impl<I: Idx + From<usize>> BitSetOperations<I> for BitSet<I> {
    /// In-place union of two `FixedBitSet`s.
    ///
    /// On calling this method, `dst`'s capacity may be increased to match `self`'s.
    fn union_into(&self, dst: &mut Self) {
        dst.internal.union_with(&self.internal)
    }

    /// In-place intersection of two `FixedBitSet`s.
    ///
    /// On calling this method, `dst`'s capacity will remain the same as before.
    fn intersect_into(&self, dst: &mut Self) {
        dst.internal.intersect_with(&self.internal)
    }

    /// In-place difference of two `FixedBitSet`s.
    ///
    /// On calling this method, `dst`'s capacity will remain the same as before.
    fn difference_into(&self, dst: &mut Self) {
        dst.internal.difference_with(&self.internal)
    }

    /// In-place symmetric difference of two `FixedBitSet`s.
    ///
    /// On calling this method, `dst`'s capacity may be increased to match `self`'s.
    fn symmetric_difference_into(&self, dst: &mut Self) {
        dst.internal.symmetric_difference_with(&self.internal)
    }
}

impl<I: Idx + From<usize>> Default for BitSet<I> {
    fn default() -> Self {
        Self {
            internal: FixedBitSet::default(),
            tag: PhantomData,
        }
    }
}
