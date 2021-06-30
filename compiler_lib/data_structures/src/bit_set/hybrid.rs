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

use crate::bit_set::sparse::SPARSE_MAX;
use crate::bit_set::{
    BitIter, BitSet, SparseBitSet, SubtractFromBitSet, SubtractFromHybridBitSet, UnionIntoBitSet,
    UnionIntoHybridBitSet,
};
use crate::index_vec::Idx;
use std::{fmt, slice};

/// A fixed-size bitset type with a hybrid representation: sparse when there
/// are up to a `SPARSE_MAX` elements in the set, but dense when there are more
/// than `SPARSE_MAX`.
///
/// This type is especially efficient for sets that typically have a small
/// number of elements, but a large `domain_size`, and are cleared frequently.
///
/// `T` is an index type, typically a newtyped `usize` wrapper, but it can also
/// just be `usize`.
///
/// All operations that involve an element will panic if the element is equal
/// to or greater than the domain size. All operations that involve two bitsets
/// will panic if the bitsets have differing domain sizes.
#[derive(Clone, PartialEq, Eq)]
pub enum HybridBitSet<T> {
    Sparse(SparseBitSet<T>),
    Dense(BitSet<T>),
}

impl<T: Idx> Default for HybridBitSet<T> {
    fn default() -> Self {
        Self::new_empty()
    }
}

impl<T: Idx> fmt::Debug for HybridBitSet<T> {
    fn fmt(&self, w: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sparse(b) => b.fmt(w),
            Self::Dense(b) => b.fmt(w),
        }
    }
}

impl<T> HybridBitSet<T> {
    pub const fn new_empty() -> Self {
        HybridBitSet::Sparse(SparseBitSet::new_empty())
    }
}

impl<T: Idx> HybridBitSet<T> {
    // pub fn domain_size(&self) -> usize {
    //     match self {
    //         ybridBitSet::Sparse(sparse) => sparse.domain_size,
    //         ybridBitSet::Dense(dense) => dense.domain_size,
    //     }
    // }

    pub fn clear(&mut self) {
        *self = HybridBitSet::new_empty();
    }

    pub fn contains(&self, elem: T) -> bool {
        match self {
            HybridBitSet::Sparse(sparse) => sparse.contains(elem),
            HybridBitSet::Dense(dense) => dense.contains(elem),
        }
    }

    pub fn superset(&self, other: &HybridBitSet<T>) -> bool {
        match (self, other) {
            (HybridBitSet::Dense(self_dense), HybridBitSet::Dense(other_dense)) => {
                self_dense.superset(other_dense)
            }
            _ => other.iter().all(|elem| self.contains(elem)),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            HybridBitSet::Sparse(sparse) => sparse.is_empty(),
            HybridBitSet::Dense(dense) => dense.is_empty(),
        }
    }

    pub fn insert(&mut self, elem: T, domain_size: usize) -> bool {
        // No need to check `elem` against `self.domain_size` here because all
        // the match cases check it, one way or another.
        match self {
            HybridBitSet::Sparse(sparse) if sparse.len() < SPARSE_MAX => {
                // The set is sparse and has space for `elem`.
                sparse.insert(elem)
            }
            HybridBitSet::Sparse(sparse) if sparse.contains(elem) => {
                // The set is sparse and does not have space for `elem`, but
                // that doesn't matter because `elem` is already present.
                false
            }
            HybridBitSet::Sparse(sparse) => {
                // The set is sparse and full. Convert to a dense set.
                let mut dense = sparse.to_dense(domain_size);
                let changed = dense.insert(elem);
                assert!(changed);
                *self = HybridBitSet::Dense(dense);
                changed
            }
            HybridBitSet::Dense(dense) => dense.insert(elem),
        }
    }

    pub fn insert_all(&mut self, domain_size: usize) {
        match self {
            HybridBitSet::Sparse(_) => {
                *self = HybridBitSet::Dense(BitSet::new_filled(domain_size));
            }
            HybridBitSet::Dense(dense) => dense.insert_all(),
        }
    }

    pub fn remove(&mut self, elem: T) -> bool {
        // Note: we currently don't bother going from Dense back to Sparse.
        match self {
            HybridBitSet::Sparse(sparse) => sparse.remove(elem),
            HybridBitSet::Dense(dense) => dense.remove(elem),
        }
    }

    /// Sets `self = self | other` and returns `true` if `self` changed
    /// (i.e., if new bits were added).
    pub fn union(&mut self, other: &impl UnionIntoHybridBitSet<T>, domain_size: usize) -> bool {
        other.union_into(self, domain_size)
    }

    /// Sets `self = self - other` and returns `true` if `self` changed.
    /// (i.e., if any bits were removed).
    pub fn subtract(&mut self, other: &impl SubtractFromHybridBitSet<T>) -> bool {
        other.subtract_from(self)
    }

    /// Converts to a dense set, consuming itself in the process.
    pub fn into_dense(self, domain_size: usize) -> BitSet<T> {
        match self {
            HybridBitSet::Sparse(sparse) => sparse.to_dense(domain_size),
            HybridBitSet::Dense(dense) => dense,
        }
    }

    pub fn iter(&self) -> HybridIter<'_, T> {
        match self {
            HybridBitSet::Sparse(sparse) => HybridIter::Sparse(sparse.iter()),
            HybridBitSet::Dense(dense) => HybridIter::Dense(dense.iter()),
        }
    }
}

impl<T: Idx> UnionIntoHybridBitSet<T> for HybridBitSet<T> {
    fn union_into(&self, other: &mut HybridBitSet<T>, domain_size: usize) -> bool {
        match other {
            HybridBitSet::Sparse(other_sparse) => {
                match self {
                    HybridBitSet::Sparse(self_sparse) => {
                        // Both sets are sparse. Add the elements in
                        // `other_sparse` to `self` one at a time. This
                        // may or may not cause `self` to be densified.
                        let mut changed = false;
                        for elem in self_sparse.iter() {
                            changed |= other.insert(*elem, domain_size);
                        }
                        changed
                    }
                    HybridBitSet::Dense(other_dense) => {
                        // `self` is sparse and `other` is dense. To
                        // merge them, we have two available strategies:
                        // * Densify `self` then merge other
                        // * Clone other then integrate bits from `self`
                        // The second strategy requires dedicated method
                        // since the usual `union` returns the wrong
                        // result. In the dedicated case the computation
                        // is slightly faster if the bits of the sparse
                        // bitset map to only few words of the dense
                        // representation, i.e. indices are near each
                        // other.
                        //
                        // Benchmarking seems to suggest that the second
                        // option is worth it.
                        let mut new_dense = other_dense.clone();
                        let changed = new_dense.reverse_union_sparse(other_sparse);
                        *other = HybridBitSet::Dense(new_dense);
                        changed
                    }
                }
            }

            HybridBitSet::Dense(other_dense) => other_dense.union(self),
        }
    }
}

impl<T: Idx> SubtractFromHybridBitSet<T> for HybridBitSet<T> {
    fn subtract_from(&self, other: &mut HybridBitSet<T>) -> bool {
        // Note: we currently don't bother going from Dense back to Sparse.
        match other {
            Self::Sparse(sparse) => {
                let mut changed = false;
                // todo smarter algorithem for sparse case? -- Probably not worth it due to all the extra branches require to make this actually work
                sparse.elems.retain(|&mut x| {
                    if self.contains(x) {
                        changed = true;
                        false
                    } else {
                        true
                    }
                });
                changed
            }
            Self::Dense(dense) => dense.subtract(self),
        }
    }
}

impl<T: Idx> UnionIntoBitSet<T> for HybridBitSet<T> {
    fn union_into(&self, other: &mut BitSet<T>) -> bool {
        match self {
            HybridBitSet::Sparse(sparse) => sparse.union_into(other),
            HybridBitSet::Dense(dense) => other.union(dense),
        }
    }
}

impl<T: Idx> SubtractFromBitSet<T> for HybridBitSet<T> {
    fn subtract_from(&self, other: &mut BitSet<T>) -> bool {
        match self {
            HybridBitSet::Sparse(sparse) => sparse.subtract_from(other),
            HybridBitSet::Dense(dense) => other.subtract(dense),
        }
    }
}

impl<T: Idx> UnionIntoHybridBitSet<T> for BitSet<T> {
    fn union_into(&self, other: &mut HybridBitSet<T>, domain_size: usize) -> bool {
        debug_assert_eq!(self.domain_size, domain_size);
        match other {
            HybridBitSet::Sparse(sparse) => {
                // `self` is sparse and `other` is dense. To
                // merge them, we have two available strategies:
                // * Densify `self` then merge other
                // * Clone other then integrate bits from `self`
                // The second strategy requires dedicated method
                // since the usual `union` returns the wrong
                // result. In the dedicated case the computation
                // is slightly faster if the bits of the sparse
                // bitset map to only few words of the dense
                // representation, i.e. indices are near each
                // other.
                //
                // Benchmarking seems to suggest that the second
                // option is worth it.
                let mut new_dense = self.clone();
                let changed = new_dense.reverse_union_sparse(sparse);
                *other = HybridBitSet::Dense(new_dense);
                changed
            }
            HybridBitSet::Dense(other_dense) => other_dense.union(self),
        }
    }
}

impl<T: Idx> SubtractFromHybridBitSet<T> for BitSet<T> {
    fn subtract_from(&self, other: &mut HybridBitSet<T>) -> bool {
        match other {
            HybridBitSet::Sparse(sparse) => {
                let mut changed = false;

                sparse.elems.retain(|&mut x| {
                    if self.contains(x) {
                        changed = true;
                        false
                    } else {
                        true
                    }
                });
                changed
            }
            HybridBitSet::Dense(other_dense) => other_dense.subtract(self),
        }
    }
}

pub enum HybridIter<'a, T: Idx> {
    Sparse(slice::Iter<'a, T>),
    Dense(BitIter<'a, T>),
}

impl<'a, T: Idx> Iterator for HybridIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        match self {
            HybridIter::Sparse(sparse) => sparse.next().copied(),
            HybridIter::Dense(dense) => dense.next(),
        }
    }
}
