use std::fmt::{self, Debug};
use std::slice;

use crate::sparse::SPARSE_MAX;
use crate::{
    BitIter, BitSet, SparseBitSet, SubtractFromBitSet, SubtractFromHybridBitSet, UnionIntoBitSet,
    UnionIntoHybridBitSet,
};

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

// TODO check if implementing clone_from here improves performance
#[derive(PartialEq, Eq)]
pub enum HybridBitSet<T> {
    Sparse(SparseBitSet<T>),
    Dense(BitSet<T>),
}

impl<T: From<usize> + Into<usize> + Copy + PartialEq + Debug> Clone for HybridBitSet<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Sparse(arg0) => Self::Sparse(arg0.clone()),
            Self::Dense(arg0) => Self::Dense(arg0.clone()),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        if let HybridBitSet::Dense(dst) = self {
            match source {
                HybridBitSet::Sparse(src) => {
                    dst.clear();
                    dst.reverse_union_sparse(src);
                }
                HybridBitSet::Dense(src) => dst.clone_from(src),
            }
        } else {
            *self = source.clone()
        }
    }
}

impl<T> Default for HybridBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    fn default() -> Self {
        Self::new_empty()
    }
}

impl<T> fmt::Debug for HybridBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
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

impl<T> HybridBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug + PartialEq + Copy,
{
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

    pub fn insert_growable(&mut self, elem: T, domain_size: usize) -> bool {
        if let HybridBitSet::Dense(dense) = self {
            dense.ensure(elem.into() + 1)
        }
        self.insert(elem, domain_size)
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
        other.subtract_from_h(self)
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

    pub fn intersect(&mut self, other: &HybridBitSet<T>) {
        let (src, other) = match (&mut *self, other) {
            (HybridBitSet::Dense(dst), HybridBitSet::Dense(src)) => {
                dst.intersect(src);
                return;
            }
            (HybridBitSet::Sparse(src), other) => (&*src, other),
            (other, HybridBitSet::Sparse(src)) => (src, &*other),
        };

        let mut res = SparseBitSet::new_empty();
        for src in src.iter() {
            if other.contains(*src) {
                res.elems.push(*src)
            }
        }
        *self = HybridBitSet::Sparse(res)
    }
}

impl<T> UnionIntoHybridBitSet<T> for HybridBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
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
                        new_dense.ensure(domain_size);
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

impl<T> SubtractFromHybridBitSet<T> for SparseBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    fn subtract_from_h(&self, other: &mut HybridBitSet<T>) -> bool {
        // Note: we currently don't bother going from Dense back to Sparse.
        match other {
            HybridBitSet::Sparse(sparse) => {
                let mut changed = false;
                // todo smarter algorithm for sparse case? -- Probably not worth it due to all the extra branches require to make this actually work
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
            HybridBitSet::Dense(dense) => dense.subtract(self),
        }
    }
}

impl<T> SubtractFromHybridBitSet<T> for HybridBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    fn subtract_from_h(&self, other: &mut HybridBitSet<T>) -> bool {
        // Note: we currently don't bother going from Dense back to Sparse.
        match other {
            Self::Sparse(sparse) => {
                let mut changed = false;
                // todo smarter algorithm for sparse case? -- Probably not worth it due to all the extra branches require to make this actually work
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

impl<T> UnionIntoBitSet<T> for HybridBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    fn union_into(&self, other: &mut BitSet<T>) -> bool {
        match self {
            HybridBitSet::Sparse(sparse) => sparse.union_into(other),
            HybridBitSet::Dense(dense) => other.union(dense),
        }
    }
}

impl<T> SubtractFromBitSet<T> for HybridBitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    fn subtract_from(&self, other: &mut BitSet<T>) -> bool {
        match self {
            HybridBitSet::Sparse(sparse) => sparse.subtract_from(other),
            HybridBitSet::Dense(dense) => other.subtract(dense),
        }
    }
}

impl<T> UnionIntoHybridBitSet<T> for BitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
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

impl<T> SubtractFromHybridBitSet<T> for BitSet<T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    fn subtract_from_h(&self, other: &mut HybridBitSet<T>) -> bool {
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

pub enum HybridIter<'a, T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    Sparse(slice::Iter<'a, T>),
    Dense(BitIter<'a, T>),
}

impl<'a, T> Iterator for HybridIter<'a, T>
where
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        match self {
            HybridIter::Sparse(sparse) => sparse.next().copied(),
            HybridIter::Dense(dense) => dense.next(),
        }
    }
}
