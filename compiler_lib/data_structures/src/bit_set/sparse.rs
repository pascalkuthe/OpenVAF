/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::BitSet;
use arrayvec::ArrayVec;
use fixedbitset::FixedBitSet;
use index_vec::Idx;
use std::mem::size_of;
use std::slice;

/// A fixed-size bitset type with a sparse representation and a maximum of
/// `SPARSE_MAX` elements. The elements are stored as a sorted `Vec` with
/// no duplicates
///
/// This type is used by `HybridBitSet`; do not use directly.
#[derive(Clone, Debug)]
pub struct SparseBitSet<T>(pub(super) ArrayVec<T, SPARSE_MAX>);

const fn sparse_max<T>() -> usize {
    let x = (2 * size_of::<FixedBitSet>() - 1) / size_of::<T>();
    if x > 3 {
        x
    } else {
        3
    }
}

// TODO calculate this dynamically per type
pub const SPARSE_MAX: usize = sparse_max::<u32>();

impl<T: Idx + From<usize>> SparseBitSet<T> {
    pub fn new_empty() -> Self {
        Self(ArrayVec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn len_idx(&self) -> T {
        self.0.len().into()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn contains(&self, elem: T) -> bool {
        self.0.contains(&elem)
    }

    pub fn put(&mut self, elem: T) -> bool {
        let changed = if let Some(i) = self.0.iter().position(|&e| e >= elem) {
            if self.0[i] == elem {
                // `elem` is already in the set.
                false
            } else {
                // `elem` is smaller than one or more existing elements.
                self.0.insert(i, elem);
                true
            }
        } else {
            // `elem` is larger than all existing elements.
            self.0.push(elem);
            true
        };
        !changed
    }

    pub fn insert(&mut self, elem: T) {
        self.put(elem);
    }

    pub fn remove(&mut self, elem: T) -> bool {
        if let Some(i) = self.0.iter().position(|&e| e == elem) {
            self.0.remove(i);
            true
        } else {
            false
        }
    }

    pub fn to_dense(&self, domain_size: T) -> BitSet<T> {
        let mut dense = BitSet::new_empty(domain_size);
        for elem in self.0.iter() {
            dense.insert(*elem);
        }
        dense
    }

    pub fn ones(&self) -> slice::Iter<'_, T> {
        self.0.iter()
    }
}
