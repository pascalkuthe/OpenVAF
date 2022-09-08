use std::fmt::{Debug, Formatter};
use std::{fmt, slice};

use arrayvec::ArrayVec;

use crate::{BitSet, SubtractFromBitSet, UnionIntoBitSet};

pub(super) const SPARSE_MAX: usize = 8;

/// A fixed-size bitset type with a sparse representation and a maximum of
/// `SPARSE_MAX` elements. The elements are stored as a sorted `ArrayVec` with
/// no duplicates.
///
/// This type is used by `HybridBitSet`; do not use directly.
#[derive(PartialEq, Eq)]
pub struct SparseBitSet<T> {
    pub(super) elems: ArrayVec<T, SPARSE_MAX>,
}

impl<T: Clone> Clone for SparseBitSet<T> {
    fn clone_from(&mut self, source: &Self) {
        self.elems.clone_from(&source.elems)
    }

    fn clone(&self) -> Self {
        Self { elems: self.elems.clone() }
    }
}

impl<T: Debug> Debug for SparseBitSet<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.elems.fmt(f)
    }
}

impl<T> SparseBitSet<T> {
    pub(super) const fn new_empty() -> Self {
        SparseBitSet { elems: ArrayVec::new_const() }
    }
}

impl<T> SparseBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    pub(super) fn to_dense(&self, domain_size: usize) -> BitSet<T> {
        let mut dense = BitSet::new_empty(domain_size);
        for elem in self.elems.iter() {
            dense.insert(*elem);
        }
        dense
    }

    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.elems.iter()
    }
}

impl<T> UnionIntoBitSet<T> for SparseBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    fn union_into(&self, other: &mut BitSet<T>) -> bool {
        let mut changed = false;
        for elem in self.iter() {
            changed |= other.insert(*elem);
        }
        changed
    }
}

impl<T> SparseBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    pub fn insert(&mut self, elem: T) -> bool {
        let changed = if let Some(i) = self.elems.iter().position(|&e| e.into() >= elem.into()) {
            if self.elems[i] == elem {
                // `elem` is already in the set.
                false
            } else {
                // `elem` is smaller than one or more existing elements.
                self.elems.insert(i, elem);
                true
            }
        } else {
            // `elem` is larger than all existing elements.
            self.elems.push(elem);
            true
        };
        changed
    }
}

impl<T> SparseBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    pub fn remove(&mut self, elem: T) -> bool {
        if let Some(i) = self.elems.iter().position(|&e| e == elem) {
            self.elems.remove(i);
            true
        } else {
            false
        }
    }

    pub fn contains(&self, elem: T) -> bool {
        self.elems.contains(&elem)
    }
}

impl<T> SubtractFromBitSet<T> for SparseBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    fn subtract_from(&self, other: &mut BitSet<T>) -> bool {
        let mut changed = false;
        for elem in self.iter() {
            changed |= other.remove(*elem);
        }
        changed
    }
}
