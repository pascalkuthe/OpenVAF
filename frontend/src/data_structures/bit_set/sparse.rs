use crate::data_structures::BitSet;
use index_vec::Idx;
use more_asserts::*;
use std::slice;

pub const SPARSE_MAX: usize = 32;

/// A fixed-size bitset type with a sparse representation and a maximum of
/// `SPARSE_MAX` elements. The elements are stored as a sorted `Vec` with
/// no duplicates
///
/// This type is used by `HybridBitSet`; do not use directly.
#[derive(Clone, Debug, Default)]
pub struct SparseBitSet<T: Idx + From<usize>> {
    pub(super) elem: Vec<T>,
    pub len_idx: T,
}

impl<T: Idx + From<usize>> SparseBitSet<T> {
    pub fn new_empty(len_idx: T) -> Self {
        Self {
            elem: Vec::with_capacity(SPARSE_MAX),
            len_idx,
        }
    }

    pub fn len(&self) -> usize {
        self.elem.len()
    }

    pub fn is_empty(&self) -> bool {
        self.elem.is_empty()
    }

    pub fn contains(&self, elem: T) -> bool {
        self.elem.contains(&elem)
    }

    pub fn put(&mut self, elem: T) -> bool {
        let changed = if let Some(i) = self.elem.iter().position(|&e| e >= elem) {
            if self.elem[i] == elem {
                // `elem` is already in the set.
                false
            } else {
                // `elem` is smaller than one or more existing elements.
                self.elem.insert(i, elem);
                true
            }
        } else {
            // `elem` is larger than all existing elements.
            self.elem.push(elem);
            true
        };
        debug_assert_le!(self.len(), SPARSE_MAX);
        !changed
    }

    pub fn insert(&mut self, elem: T) {
        self.put(elem);
    }

    pub fn remove(&mut self, elem: T) -> bool {
        if let Some(i) = self.elem.iter().position(|&e| e == elem) {
            self.elem.remove(i);
            true
        } else {
            false
        }
    }

    pub fn to_dense(&self) -> BitSet<T> {
        let mut dense = BitSet::new_empty(self.len_idx);
        for elem in self.elem.iter() {
            dense.insert(*elem);
        }
        dense
    }

    pub fn ones(&self) -> slice::Iter<'_, T> {
        self.elem.iter()
    }
}
