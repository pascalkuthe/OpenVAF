/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
*/

use crate::bit_set::hybrid::HybridBitSet;
use crate::{BitSet, BitSetOperations};
use core::ops::{Index, IndexMut};
use index_vec::{index_vec, Idx, IndexVec};
use std::{iter, slice};

pub type EnumartedRows<'lt, C, V> = iter::FilterMap<
    iter::Enumerate<slice::Iter<'lt, Option<HybridBitSet<V>>>>,
    fn((usize, &'lt Option<HybridBitSet<V>>)) -> Option<(C, &'lt HybridBitSet<V>)>,
>;
pub type Rows<'lt, V> = iter::FilterMap<
    slice::Iter<'lt, Option<HybridBitSet<V>>>,
    fn(&'lt Option<HybridBitSet<V>>) -> Option<&'lt HybridBitSet<V>>,
>;

#[derive(Debug, Clone, Default)]
pub struct SparseBitSetMatrix<C: Idx + From<usize>, V: Idx + From<usize>> {
    data: IndexVec<C, Option<HybridBitSet<V>>>,
    y_len_idx: V,
}

impl<C: Idx + From<usize>, V: Idx + From<usize>> SparseBitSetMatrix<C, V> {
    pub fn new_empty(x_len_idx: C, y_len_idx: V) -> Self {
        Self {
            data: index_vec!(None; x_len_idx.index()),
            y_len_idx,
        }
    }

    pub fn x_len_idx(&self) -> C {
        self.data.len_idx()
    }

    pub fn y_len_idx(&self) -> V {
        self.y_len_idx
    }

    pub fn contains(&self, x: C, y: V) -> bool {
        if let Some(row) = &self.row(x) {
            row.contains(y)
        } else {
            false
        }
    }

    pub fn row(&self, x: C) -> &Option<HybridBitSet<V>> {
        &self.data[x]
    }

    pub fn row_mut(&mut self, x: C) -> &mut Option<HybridBitSet<V>> {
        &mut self.data[x]
    }

    pub fn insert(&mut self, x: C, y: V) {
        if let Some(row) = &mut self.data[x] {
            row.insert(y, self.y_len_idx)
        } else {
            let mut row = HybridBitSet::new_empty();
            row.insert(y, self.y_len_idx);
            self.data[x] = Some(row)
        }
    }
    pub fn rows_enumerated(&self) -> EnumartedRows<'_, C, V> {
        self.data
            .iter()
            .enumerate()
            .filter_map(|(i, t)| Some((C::from_usize(i), t.as_ref()?)))
    }

    pub fn rows(&self) -> Rows<'_, V> {
        self.data.iter().filter_map(Option::as_ref)
    }
}

impl<T: Idx + From<usize>> BitSetOperations<T> for Option<HybridBitSet<T>> {
    fn union_into(&self, dst: &mut BitSet<T>) {
        if let Some(src) = self {
            src.union_into(dst)
        }
    }

    fn intersect_into(&self, dst: &mut BitSet<T>) {
        if let Some(src) = self {
            src.intersect_into(dst)
        }
    }

    fn difference_into(&self, dst: &mut BitSet<T>) {
        if let Some(src) = self {
            src.difference_into(dst)
        }
    }

    fn symmetric_difference_into(&self, dst: &mut BitSet<T>) {
        if let Some(src) = self {
            src.symmetric_difference_into(dst)
        }
    }
}

impl<C: Idx + From<usize>, V: Idx + From<usize>> Index<C> for SparseBitSetMatrix<C, V> {
    type Output = Option<HybridBitSet<V>>;

    fn index(&self, index: C) -> &Self::Output {
        &self.data[index]
    }
}

impl<C: Idx + From<usize>, V: Idx + From<usize>> IndexMut<C> for SparseBitSetMatrix<C, V> {
    fn index_mut(&mut self, index: C) -> &mut Self::Output {
        &mut self.data[index]
    }
}
