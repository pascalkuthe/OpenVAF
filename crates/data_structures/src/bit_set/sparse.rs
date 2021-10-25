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

use crate::arrayvec::ArrayVec;
use crate::bit_set::{BitSet, SubtractFromBitSet, UnionIntoBitSet};
use crate::index_vec::Idx;
use std::fmt::{Debug, Formatter};
use std::{fmt, slice};

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

impl<T: Idx> SparseBitSet<T> {
    pub(super) fn len(&self) -> usize {
        self.elems.len()
    }

    pub(super) fn is_empty(&self) -> bool {
        self.elems.len() == 0
    }

    pub(super) fn contains(&self, elem: T) -> bool {
        self.elems.contains(&elem)
    }

    pub(super) fn insert(&mut self, elem: T) -> bool {
        let changed = if let Some(i) = self.elems.iter().position(|&e| e >= elem) {
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

    pub(super) fn remove(&mut self, elem: T) -> bool {
        if let Some(i) = self.elems.iter().position(|&e| e == elem) {
            self.elems.remove(i);
            true
        } else {
            false
        }
    }

    pub(super) fn to_dense(&self, domain_size: usize) -> BitSet<T> {
        let mut dense = BitSet::new_empty(domain_size);
        for elem in self.elems.iter() {
            dense.insert(*elem);
        }
        dense
    }

    pub(super) fn iter(&self) -> slice::Iter<'_, T> {
        self.elems.iter()
    }
}

impl<T: Idx> UnionIntoBitSet<T> for SparseBitSet<T> {
    fn union_into(&self, other: &mut BitSet<T>) -> bool {
        let mut changed = false;
        for elem in self.iter() {
            changed |= other.insert(*elem);
        }
        changed
    }
}

impl<T: Idx> SubtractFromBitSet<T> for SparseBitSet<T> {
    fn subtract_from(&self, other: &mut BitSet<T>) -> bool {
        let mut changed = false;
        for elem in self.iter() {
            changed |= other.remove(*elem);
        }
        changed
    }
}
