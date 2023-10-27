//! Forest of sets.
#[cfg(test)]
mod tests;

use super::{Comparator, Forest, Node, NodeData, NodePool, Path, SetValue, INNER_SIZE};
use crate::packed_option::PackedOption;
use core::marker::PhantomData;

/// Tag type defining forest types for a set.
struct SetTypes<K>(PhantomData<K>);

impl<K> Forest for SetTypes<K>
where
    K: Copy,
{
    type Key = K;
    type Value = SetValue;
    type LeafKeys = [K; 2 * INNER_SIZE - 1];
    type LeafValues = [SetValue; 2 * INNER_SIZE - 1];

    fn splat_key(key: Self::Key) -> Self::LeafKeys {
        [key; 2 * INNER_SIZE - 1]
    }

    fn splat_value(value: Self::Value) -> Self::LeafValues {
        [value; 2 * INNER_SIZE - 1]
    }
}

/// Memory pool for a forest of `Set` instances.
pub struct SetForest<K>
where
    K: Copy,
{
    nodes: NodePool<SetTypes<K>>,
}

impl<K: Copy> Clone for SetForest<K> {
    fn clone(&self) -> Self {
        Self { nodes: self.nodes.clone() }
    }
}

impl<K> SetForest<K>
where
    K: Copy,
{
    /// Create a new empty forest.
    pub fn new() -> Self {
        Self { nodes: NodePool::new() }
    }

    /// Clear all sets in the forest.
    ///
    /// All `Set` instances belong to this forest are invalidated and should no longer be used.
    pub fn clear(&mut self) {
        self.nodes.clear();
    }
}

impl<K> Default for SetForest<K>
where
    K: Copy,
{
    fn default() -> Self {
        Self::new()
    }
}

/// B-tree representing an ordered set of `K`s using `C` for comparing elements.
///
/// This is not a general-purpose replacement for `BTreeSet`. See the [module
/// documentation](index.html) for more information about design tradeoffs.
///
/// Sets can be cloned, but that operation should only be used as part of cloning the whole forest
/// they belong to. *Cloning a set does not allocate new memory for the clone*. It creates an alias
/// of the same memory.
#[derive(Clone, Copy)]
pub struct Set<K>
where
    K: Copy,
{
    root: PackedOption<Node>,
    unused: PhantomData<K>,
}

impl<K> Set<K>
where
    K: Copy,
{
    /// Make an empty set.
    pub fn new() -> Self {
        Self { root: None.into(), unused: PhantomData }
    }

    /// Is this an empty set?
    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }

    /// Does the set contain `key`?.
    pub fn contains<C: Comparator<K>>(&self, key: K, forest: &SetForest<K>, comp: &C) -> bool {
        self.root
            .expand()
            .and_then(|root| Path::default().find(key, root, &forest.nodes, comp))
            .is_some()
    }

    /// Try to insert `key` into the set.
    ///
    /// If the set did not contain `key`, insert it and return true.
    ///
    /// If `key` is already present, don't change the set and return false.
    pub fn insert<C: Comparator<K>>(
        &mut self,
        key: K,
        forest: &mut SetForest<K>,
        comp: &C,
    ) -> bool {
        self.cursor(forest, comp).insert(key)
    }

    /// Remove `key` from the set and return true.
    ///
    /// If `key` was not present in the set, return false.
    pub fn remove<C: Comparator<K>>(
        &mut self,
        key: K,
        forest: &mut SetForest<K>,
        comp: &C,
    ) -> bool {
        let mut c = self.cursor(forest, comp);
        if c.goto(key) {
            c.remove();
            true
        } else {
            false
        }
    }

    /// Remove all entries.
    pub fn clear(&mut self, forest: &mut SetForest<K>) {
        if let Some(root) = self.root.take() {
            forest.nodes.free_tree(root);
        }
    }

    /// Retains only the elements specified by the predicate.
    ///
    /// Remove all elements where the predicate returns false.
    pub fn retain<F>(&mut self, forest: &mut SetForest<K>, mut predicate: F)
    where
        F: FnMut(K) -> bool,
    {
        let mut path = Path::default();
        if let Some(root) = self.root.expand() {
            path.first(root, &forest.nodes);
        }
        while let Some((node, entry)) = path.leaf_pos() {
            if predicate(forest.nodes[node].unwrap_leaf().0[entry]) {
                path.next(&forest.nodes);
            } else {
                self.root = path.remove(&mut forest.nodes).into();
            }
        }
    }

    /// Create a cursor for navigating this set. The cursor is initially positioned off the end of
    /// the set.
    pub fn cursor<'a, C: Comparator<K>>(
        &'a mut self,
        forest: &'a mut SetForest<K>,
        comp: &'a C,
    ) -> SetCursor<'a, K, C> {
        SetCursor::new(self, forest, comp)
    }

    /// Create an iterator traversing this set. The iterator type is `K`.
    pub fn iter<'a>(&'a self, forest: &'a SetForest<K>) -> SetIter<'a, K> {
        SetIter { root: self.root, pool: &forest.nodes, path: Path::default() }
    }

    /// Create an iterator traversing this set. The iterator type is `K`.
    pub fn iter_rev<'a>(&'a self, forest: &'a SetForest<K>) -> RevSetIter<'a, K> {
        RevSetIter { root: self.root, pool: &forest.nodes, path: Path::default() }
    }

    /// create a `SetPos` which allows streaming iteration of the set
    pub fn read_cursor(self) -> SetPos<K> {
        SetPos { root: self.root, path: Path::default() }
    }
}

impl<K> Default for Set<K>
where
    K: Copy,
{
    fn default() -> Self {
        Self::new()
    }
}

/// A position in a `Set` used to navigate and modify the ordered set.
///
/// A cursor always points at an element in the set, or "off the end" which is a position after the
/// last element in the set.
pub struct SetCursor<'a, K, C>
where
    K: 'a + Copy,
    C: 'a + Comparator<K>,
{
    root: &'a mut PackedOption<Node>,
    pool: &'a mut NodePool<SetTypes<K>>,
    comp: &'a C,
    path: Path<SetTypes<K>>,
}

impl<'a, K, C> SetCursor<'a, K, C>
where
    K: Copy,
    C: Comparator<K>,
{
    /// Create a cursor with a default (invalid) location.
    fn new(container: &'a mut Set<K>, forest: &'a mut SetForest<K>, comp: &'a C) -> Self {
        Self { root: &mut container.root, pool: &mut forest.nodes, comp, path: Path::default() }
    }

    /// Is this cursor pointing to an empty set?
    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }

    /// Move cursor to the next element and return it.
    ///
    /// If the cursor reaches the end, return `None` and leave the cursor at the off-the-end
    /// position.
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::should_implement_trait))]
    pub fn next(&mut self) -> Option<K> {
        self.path.next(self.pool).map(|(k, _)| k)
    }

    /// Move cursor to the previous element and return it.
    ///
    /// If the cursor is already pointing at the first element, leave it there and return `None`.
    pub fn prev(&mut self) -> Option<K> {
        self.root.expand().and_then(|root| self.path.prev(root, self.pool).map(|(k, _)| k))
    }

    /// Get the current element, or `None` if the cursor is at the end.
    pub fn elem(&self) -> Option<K> {
        self.path
            .leaf_pos()
            .and_then(|(node, entry)| self.pool[node].unwrap_leaf().0.get(entry).cloned())
    }

    /// Move this cursor to `elem`.
    ///
    /// If `elem` is in the set, place the cursor at `elem` and return true.
    ///
    /// If `elem` is not in the set, place the cursor at the next larger element (or the end) and
    /// return false.
    pub fn goto(&mut self, elem: K) -> bool {
        match self.root.expand() {
            None => false,
            Some(root) => {
                if self.path.find(elem, root, self.pool, self.comp).is_some() {
                    true
                } else {
                    self.path.normalize(self.pool);
                    false
                }
            }
        }
    }

    /// Move this cursor to the first element.
    pub fn goto_first(&mut self) -> Option<K> {
        self.root.map(|root| self.path.first(root, self.pool).0)
    }

    /// Try to insert `elem` into the set and leave the cursor at the inserted element.
    ///
    /// If the set did not contain `elem`, insert it and return true.
    ///
    /// If `elem` is already present, don't change the set, place the cursor at `goto(elem)`, and
    /// return false.
    pub fn insert(&mut self, elem: K) -> bool {
        match self.root.expand() {
            None => {
                let root = self.pool.alloc_node(NodeData::leaf(elem, SetValue()));
                *self.root = root.into();
                self.path.set_root_node(root);
                true
            }
            Some(root) => {
                // TODO: Optimize the case where `self.path` is already at the correct insert pos.
                if self.path.find(elem, root, self.pool, self.comp).is_none() {
                    *self.root = self.path.insert(elem, SetValue(), self.pool).into();
                    true
                } else {
                    false
                }
            }
        }
    }

    /// Remove the current element (if any) and return it.
    /// This advances the cursor to the next element after the removed one.
    pub fn remove(&mut self) -> Option<K> {
        let elem = self.elem();
        if elem.is_some() {
            *self.root = self.path.remove(self.pool).into();
        }
        elem
    }
}

/// An iterator visiting the elements of a `Set`.
#[derive(Clone, Copy)]
pub struct SetIter<'a, K>
where
    K: 'a + Copy,
{
    root: PackedOption<Node>,
    pool: &'a NodePool<SetTypes<K>>,
    path: Path<SetTypes<K>>,
}

impl<'a, K> Iterator for SetIter<'a, K>
where
    K: 'a + Copy,
{
    type Item = K;

    fn next(&mut self) -> Option<Self::Item> {
        // We use `self.root` to indicate if we need to go to the first element. Reset to `None`
        // once we've returned the first element. This also works for an empty tree since the
        // `path.next()` call returns `None` when the path is empty. This also fuses the iterator.
        match self.root.take() {
            Some(root) => Some(self.path.first(root, self.pool).0),
            None => self.path.next(self.pool).map(|(k, _)| k),
        }
    }
}

/// An iterator visiting the elements of a `Set`.
#[derive(Clone, Copy)]
pub struct RevSetIter<'a, K>
where
    K: 'a + Copy,
{
    root: PackedOption<Node>,
    pool: &'a NodePool<SetTypes<K>>,
    path: Path<SetTypes<K>>,
}

impl<'a, K> Iterator for RevSetIter<'a, K>
where
    K: 'a + Copy,
{
    type Item = K;

    fn next(&mut self) -> Option<Self::Item> {
        self.path.prev(self.root.expand()?, self.pool).map(|(k, _)| k)
    }
}

/// An iterator visiting the elements of a `Set`.
#[derive(Clone, Copy)]
pub struct SetPos<K>
where
    K: Copy,
{
    root: PackedOption<Node>,
    path: Path<SetTypes<K>>,
}

impl<K> SetPos<K>
where
    K: Copy,
{
    pub fn next(&mut self, forest: &SetForest<K>) -> Option<K> {
        // We use `self.root` to indicate if we need to go to the first element. Reset to `None`
        // once we've returned the first element. This also works for an empty tree since the
        // `path.next()` call returns `None` when the path is empty. This also fuses the iterator.
        match self.root.take() {
            Some(root) => Some(self.path.first(root, &forest.nodes).0),
            None => self.path.next(&forest.nodes).map(|(k, _)| k),
        }
    }
}
