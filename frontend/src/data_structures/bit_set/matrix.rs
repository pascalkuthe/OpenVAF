use crate::data_structures::bit_set::hybrid::HybridBitSet;
use crate::data_structures::{BitSet, BitSetOperations};
use core::ops::{Index, IndexMut};
use index_vec::{index_vec, Idx, IndexVec};

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
        if let Some(row) = self.row_mut(x) {
            row.insert(y)
        } else {
            let mut row = HybridBitSet::new_empty(self.y_len_idx);
            row.insert(y);
            self.data[x] = Some(row)
        }
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