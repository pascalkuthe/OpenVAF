/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::bit_set::sparse::{SparseBitSet, SPARSE_MAX};
use crate::bit_set::{BitSetOperations, Ones};
use crate::BitSet;
use index_vec::Idx;
use std::cmp::Ordering;
use std::slice;

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
#[derive(Clone, Debug)]
pub enum HybridBitSet<T: Idx + From<usize>> {
    Sparse(SparseBitSet<T>),
    Dense(BitSet<T>),
}

impl<T: Idx + From<usize>> HybridBitSet<T> {
    pub fn new_empty() -> Self {
        Self::Sparse(SparseBitSet::new_empty())
    }

    fn len_idx(&self) -> T {
        match self {
            Self::Sparse(sparse) => sparse.len_idx(),
            Self::Dense(dense) => dense.len_idx(),
        }
    }

    pub fn clear(&mut self) {
        match self {
            Self::Sparse(x) => *x = SparseBitSet::new_empty(),
            Self::Dense(x) => x.clear(),
        }
    }

    pub fn contains(&self, elem: T) -> bool {
        match self {
            Self::Sparse(sparse) => sparse.contains(elem),
            Self::Dense(dense) => dense.contains(elem),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Self::Sparse(sparse) => sparse.is_empty(),
            Self::Dense(dense) => dense.is_empty(),
        }
    }

    pub fn put(&mut self, elem: T, domain_size: T) -> bool {
        match self {
            Self::Sparse(sparse) if sparse.len() < SPARSE_MAX => {
                // The set is sparse and has space for `elem`.
                sparse.put(elem)
            }
            Self::Sparse(sparse) if sparse.contains(elem) => {
                // The set is sparse and does not have space for `elem`, but
                // that doesn't matter because `elem` is already present.
                true
            }
            Self::Sparse(sparse) => {
                // The set is sparse and full. Convert to a dense set.
                let mut dense = sparse.to_dense(domain_size);
                let old = dense.put(elem);
                debug_assert!(!old);
                *self = HybridBitSet::Dense(dense);
                old
            }
            Self::Dense(dense) => dense.put(elem),
        }
    }

    pub fn insert(&mut self, elem: T, domain_size: T) {
        self.put(elem, domain_size);
    }

    pub fn enable_all(&mut self) {
        let domain_size = self.len_idx();
        match self {
            Self::Sparse(_) => {
                *self = HybridBitSet::Dense(BitSet::new_filled(domain_size));
            }
            Self::Dense(dense) => dense.enable_all(),
        }
    }

    pub fn remove(&mut self, elem: T) -> bool {
        // Note: we currently don't bother going from Dense back to Sparse.
        match self {
            Self::Sparse(sparse) => sparse.remove(elem),
            Self::Dense(dense) => dense.remove(elem),
        }
    }

    pub fn difference_with(&mut self, other: &Self) {
        // Note: we currently don't bother going from Dense back to Sparse.
        match self {
            Self::Sparse(sparse) => {
                match other {
                    Self::Dense(dense) => {
                        // There are two approaches to this:
                        // * iterate sparse and remove all elements that are contained in dense
                        // * iterate dense and remove all elements that are contained in sparse
                        // since sparse is generally smaller and bitset lookups are fairly quick the first option should be faster

                        sparse.0.retain(|&mut x| !dense.contains(x))
                    }
                    Self::Sparse(other) => {
                        // On(n+m) difference algorithm possible because sparse sets are always sorted and have unique members
                        // really fast (in practice 5-20% total execution time for real world models)
                        let mut pos = 0;
                        sparse.0.retain(|x| {
                            while pos < other.len() {
                                match other.0[pos].cmp(x) {
                                    Ordering::Equal => {
                                        pos += 1;
                                        return false;
                                    }
                                    Ordering::Less => pos += 1,
                                    Ordering::Greater => return true,
                                }
                            }
                            false
                        });
                    }
                }
            }
            Self::Dense(dense) => dense.difference_with(other),
        }
    }

    /// Converts to a dense set, consuming itself in the process.
    pub fn into_dense(self, domain_size: T) -> BitSet<T> {
        match self {
            Self::Sparse(sparse) => sparse.to_dense(domain_size),
            Self::Dense(dense) => dense,
        }
    }

    pub fn intersection<'lt>(
        &'lt self,
        other: &'lt Self,
    ) -> Intersection<'lt, T, HybridIter<'lt, T>> {
        Intersection {
            src: self.ones(),
            other,
        }
    }

    pub fn ones(&self) -> HybridIter<T> {
        match self {
            Self::Sparse(sparse) => HybridIter::Sparse(sparse.ones()),
            Self::Dense(dense) => HybridIter::Dense(dense.ones()),
        }
    }

    pub fn union_with(&mut self, other: &Self, domain_size: T) {
        match self {
            Self::Sparse(self_sparse) => {
                match other {
                    Self::Sparse(other_sparse) => {
                        for elem in other_sparse.ones() {
                            self.insert(*elem, domain_size);
                        }
                    }
                    Self::Dense(other_dense) => {
                        // `self` is sparse and `other` is dense. To
                        // merge them, we have two available strategies:
                        // * Densify `self` then merge other
                        // * Clone other then insert bits from `self`
                        //
                        // Benchmarking seems to suggest that the second
                        // option is faster
                        let mut new_dense = other_dense.clone();
                        for elem in self_sparse.ones() {
                            new_dense.insert(*elem);
                        }
                        *self = Self::Dense(new_dense);
                    }
                }
            }

            Self::Dense(self_dense) => self_dense.union_with(other),
        }
    }
}

impl<T: Idx + From<usize>> BitSetOperations<T> for HybridBitSet<T> {
    fn union_into(&self, dst: &mut BitSet<T>) {
        match self {
            HybridBitSet::Dense(src) => src.union_into(dst),
            HybridBitSet::Sparse(src) => {
                for x in src.ones().copied() {
                    dst.insert(x)
                }
            }
        }
    }

    fn intersect_into(&self, dst: &mut BitSet<T>) {
        match self {
            HybridBitSet::Dense(src) => src.intersect_into(dst),
            HybridBitSet::Sparse(src) => src.clone().to_dense(dst.len_idx()).intersect_into(dst),
        }
    }

    fn difference_into(&self, dst: &mut BitSet<T>) {
        match self {
            HybridBitSet::Dense(src) => src.difference_into(dst),
            HybridBitSet::Sparse(src) => {
                for x in src.ones().copied() {
                    dst.set(x, false)
                }
            }
        }
    }

    fn symmetric_difference_into(&self, dst: &mut BitSet<T>) {
        match self {
            HybridBitSet::Dense(src) => src.symmetric_difference_into(dst),
            HybridBitSet::Sparse(src) => src
                .clone()
                .to_dense(dst.len_idx())
                .symmetric_difference_into(dst),
        }
    }
}

pub enum HybridIter<'a, T: Idx + From<usize>> {
    Sparse(slice::Iter<'a, T>),
    Dense(Ones<'a, T>),
}

impl<'a, T: Idx + From<usize>> Iterator for HybridIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        match self {
            HybridIter::Sparse(sparse) => sparse.next().copied(),
            HybridIter::Dense(dense) => dense.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            HybridIter::Sparse(sparse) => sparse.size_hint(),
            HybridIter::Dense(dense) => dense.size_hint(),
        }
    }
}

pub struct Intersection<'lt, T: Idx + From<usize>, I: Iterator<Item = T> + 'lt> {
    src: I,
    other: &'lt HybridBitSet<T>,
}

impl<'lt, T: Idx + From<usize>, I: Iterator<Item = T> + 'lt> Iterator for Intersection<'lt, T, I> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(i) = self.src.next() {
            if self.other.contains(i) {
                return Some(i);
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if let Some(src_upper) = self.src.size_hint().1 {
            (0, Some(src_upper.max(self.other.len_idx().index())))
        } else {
            // it is assumed that src is an iterator over a set so x may not contain repretions
            (0, Some(self.other.len_idx().index()))
        }
    }
}
