//! Forest of maps.

use super::{Comparator, Forest, Node, NodeData, NodePool, Path, INNER_SIZE};
use crate::packed_option::PackedOption;
use core::marker::PhantomData;

#[cfg(test)]
mod tests;

/// Tag type defining forest types for a map.
struct MapTypes<K, V>(PhantomData<(K, V)>);

impl<K, V> Forest for MapTypes<K, V>
where
    K: Copy,
    V: Copy,
{
    type Key = K;
    type Value = V;
    type LeafKeys = [K; INNER_SIZE - 1];
    type LeafValues = [V; INNER_SIZE - 1];

    fn splat_key(key: Self::Key) -> Self::LeafKeys {
        [key; INNER_SIZE - 1]
    }

    fn splat_value(value: Self::Value) -> Self::LeafValues {
        [value; INNER_SIZE - 1]
    }
}

#[derive(Clone)]
/// Memory pool for a forest of `Map` instances.
pub struct MapForest<K, V>
where
    K: Copy,
    V: Copy,
{
    nodes: NodePool<MapTypes<K, V>>,
}

impl<K, V> MapForest<K, V>
where
    K: Copy,
    V: Copy,
{
    /// Create a new empty forest.
    pub fn new() -> Self {
        Self { nodes: NodePool::new() }
    }

    /// Clear all maps in the forest.
    ///
    /// All `Map` instances belong to this forest are invalidated and should no longer be used.
    pub fn clear(&mut self) {
        self.nodes.clear();
    }
}

impl<K, V> Default for MapForest<K, V>
where
    K: Copy,
    V: Copy,
{
    fn default() -> Self {
        Self::new()
    }
}

/// B-tree mapping from `K` to `V`.
///
/// This is not a general-purpose replacement for `BTreeMap`. See the [module
/// documentation](index.html) for more information about design tradeoffs.
///
/// Maps can be cloned, but that operation should only be used as part of cloning the whole forest
/// they belong to. *Cloning a map does not allocate new memory for the clone*. It creates an alias
/// of the same memory.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Map<K, V>
where
    K: Copy,
    V: Copy,
{
    root: PackedOption<Node>,
    unused: PhantomData<(K, V)>,
}

impl<K, V> Map<K, V>
where
    K: Copy,
    V: Copy,
{
    /// Make an empty map.
    #[inline]
    pub fn new() -> Self {
        Self { root: None.into(), unused: PhantomData }
    }

    /// Is this an empty map?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }

    /// Get the value stored for `key`.
    #[inline]
    pub fn get<C: Comparator<K>>(&self, key: K, forest: &MapForest<K, V>, comp: &C) -> Option<V> {
        self.root.expand().and_then(|root| Path::default().find(key, root, &forest.nodes, comp))
    }

    /// Look up the value stored for `key`.
    ///
    /// If it exists, return the stored key-value pair.
    ///
    /// Otherwise, return the last key-value pair with a key that is less than or equal to `key`.
    ///
    /// If no stored keys are less than or equal to `key`, return `None`.
    pub fn get_or_less<C: Comparator<K>>(
        &self,
        key: K,
        forest: &MapForest<K, V>,
        comp: &C,
    ) -> Option<(K, V)> {
        self.root.expand().and_then(|root| {
            let mut path = Path::default();
            match path.find(key, root, &forest.nodes, comp) {
                Some(v) => Some((key, v)),
                None => path.prev(root, &forest.nodes),
            }
        })
    }

    /// Merges `other` into `self`
    ///
    /// For entries that are in both maps the values from `other` is used
    pub fn merge<C: Comparator<K>>(
        &mut self,
        other: Self,
        forest: &mut MapForest<K, V>,
        comp: &C,
        f: impl FnMut(Option<V>, V) -> V,
    ) {
        let mut src_path = Path::default();
        let mut root = other.root;
        self.insert_sorted(
            |forest| match root.take() {
                Some(root) => Some(src_path.first(root, &forest.nodes)),
                None => src_path.next(&forest.nodes),
            },
            forest,
            comp,
            f,
        );
    }

    /// Merges `other` into `self`
    ///
    /// # Note
    ///
    /// for this function to produce correct results
    /// * `src` must provides its values in ascending key order
    /// * `src` must not contain any duplicates
    pub fn insert_sorted_iter<C: Comparator<K>, T>(
        &mut self,
        sorted_iter: impl IntoIterator<Item = (K, T)>,
        forest: &mut MapForest<K, V>,
        comp: &C,
        f: impl FnMut(Option<V>, T) -> V,
    ) {
        let mut iter = sorted_iter.into_iter();
        self.insert_sorted(|_| iter.next(), forest, comp, f)
    }

    /// Merges `other` into `self`
    ///
    /// # Note
    ///
    /// for this function to produce correct results
    /// * `next_src` must provides its values in ascending key order
    /// * `next_src` must not contain any duplicates
    pub fn insert_sorted<C: Comparator<K>, T>(
        &mut self,
        mut next_src: impl FnMut(&MapForest<K, V>) -> Option<(K, T)>,
        forest: &mut MapForest<K, V>,
        comp: &C,
        mut f: impl FnMut(Option<V>, T) -> V,
    ) {
        let mut dst_path = Path::default();

        if let Some(root) = self.root.expand() {
            dst_path.set_root_node(root)
        }

        if let Some((key, val)) = next_src(forest) {
            if let Some(root) = self.root.expand() {
                if let Some(old) = dst_path.find(key, root, &forest.nodes, comp) {
                    let val = f(Some(old), val);
                    *dst_path.value_mut(&mut forest.nodes) = val;
                } else {
                    let val = f(None, val);
                    dst_path.insert(key, val, &mut forest.nodes);
                }
            } else {
                let val = f(None, val);
                let root = forest.nodes.alloc_node(NodeData::leaf(key, val));
                dst_path.set_root_node(root);
            }
        } else {
            return;
        }

        while let Some(src_entry) = next_src(forest) {
            match dst_path.advance_to_insert_pos(src_entry.0, &mut forest.nodes, comp) {
                Some(val) => *dst_path.value_mut(&mut forest.nodes) = f(Some(val), src_entry.1),
                None => {
                    let val = f(None, src_entry.1);
                    dst_path.insert(src_entry.0, val, &mut forest.nodes);
                }
            }
        }

        self.root = dst_path.get_root_node().into();
    }

    #[inline]
    /// Insert `key, value` into the map and return the old value stored for `key`, if any.
    pub fn insert<C: Comparator<K>>(
        &mut self,
        key: K,
        value: V,
        forest: &mut MapForest<K, V>,
        comp: &C,
    ) -> Option<V> {
        self.cursor(forest, comp).insert(key, value)
    }

    #[inline]
    /// Return the value associtated with `key` or insert the value retruned by `value` if not present
    pub fn get_or_insert_with<C: Comparator<K>>(
        &mut self,
        key: K,
        value: impl FnOnce() -> V,
        forest: &mut MapForest<K, V>,
        comp: &C,
    ) -> V {
        self.cursor(forest, comp).get_or_insert_with(key, value)
    }

    #[inline]
    /// Call update for the value associtated with `key` or insert the value retruned by `value` if not present
    pub fn update_or_insert_with<C: Comparator<K>>(
        &mut self,
        key: K,
        update_or_insert: impl FnOnce(Option<&mut V>) -> V,
        forest: &mut MapForest<K, V>,
        comp: &C,
    ) {
        self.cursor(forest, comp).update_or_insert_with(key, update_or_insert)
    }

    #[inline]
    /// Remove `key` from the map and return the removed value for `key`, if any.
    pub fn remove<C: Comparator<K>>(
        &mut self,
        key: K,
        forest: &mut MapForest<K, V>,
        comp: &C,
    ) -> Option<V> {
        let mut c = self.cursor(forest, comp);
        if c.goto(key).is_some() {
            c.remove()
        } else {
            None
        }
    }

    /// Remove all entries.
    pub fn clear(&mut self, forest: &mut MapForest<K, V>) {
        if let Some(root) = self.root.take() {
            forest.nodes.free_tree(root);
        }
    }

    /// Retains only the elements specified by the predicate.
    ///
    /// Remove all key-value pairs where the predicate returns false.
    ///
    /// The predicate is allowed to update the values stored in the map.
    pub fn retain<F>(&mut self, forest: &mut MapForest<K, V>, mut predicate: F)
    where
        F: FnMut(K, &mut V) -> bool,
    {
        let mut path = Path::default();
        if let Some(root) = self.root.expand() {
            path.first(root, &forest.nodes);
        }
        while let Some((node, entry)) = path.leaf_pos() {
            let keep = {
                let (ks, vs) = forest.nodes[node].unwrap_leaf_mut();
                predicate(ks[entry], &mut vs[entry])
            };
            if keep {
                path.next(&forest.nodes);
            } else {
                self.root = path.remove(&mut forest.nodes).into();
            }
        }
    }

    /// Create a cursor for navigating this map. The cursor is initially positioned off the end of
    /// the map.
    pub fn cursor<'a, C: Comparator<K>>(
        &'a mut self,
        forest: &'a mut MapForest<K, V>,
        comp: &'a C,
    ) -> MapCursor<'a, K, V, C> {
        MapCursor::new(self, forest, comp)
    }

    /// Create an iterator traversing this map. The iterator type is `(K, V)`.
    pub fn iter<'a>(&self, forest: &'a MapForest<K, V>) -> MapIter<'a, K, V> {
        MapIter { root: self.root, pool: &forest.nodes, path: Path::default() }
    }
}

impl<K, V> Default for Map<K, V>
where
    K: Copy,
    V: Copy,
{
    fn default() -> Self {
        Self::new()
    }
}

/// A position in a `Map` used to navigate and modify the ordered map.
///
/// A cursor always points at a key-value pair in the map, or "off the end" which is a position
/// after the last entry in the map.
pub struct MapCursor<'a, K, V, C>
where
    K: 'a + Copy,
    V: 'a + Copy,
    C: 'a + Comparator<K>,
{
    root: &'a mut PackedOption<Node>,
    pool: &'a mut NodePool<MapTypes<K, V>>,
    comp: &'a C,
    path: Path<MapTypes<K, V>>,
}

impl<'a, K, V, C> MapCursor<'a, K, V, C>
where
    K: Copy,
    V: Copy,
    C: Comparator<K>,
{
    /// Create a cursor with a default (off-the-end) location.
    fn new(container: &'a mut Map<K, V>, forest: &'a mut MapForest<K, V>, comp: &'a C) -> Self {
        Self { root: &mut container.root, pool: &mut forest.nodes, comp, path: Path::default() }
    }

    /// Is this cursor pointing to an empty map?
    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }

    /// Move cursor to the next key-value pair and return it.
    ///
    /// If the cursor reaches the end, return `None` and leave the cursor at the off-the-end
    /// position.
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::should_implement_trait))]
    pub fn next(&mut self) -> Option<(K, V)> {
        self.path.next(self.pool)
    }

    /// Move cursor to the previous key-value pair and return it.
    ///
    /// If the cursor is already pointing at the first entry, leave it there and return `None`.
    pub fn prev(&mut self) -> Option<(K, V)> {
        self.root.expand().and_then(|root| self.path.prev(root, self.pool))
    }

    /// Get the current key, or `None` if the cursor is at the end.
    pub fn key(&self) -> Option<K> {
        self.path
            .leaf_pos()
            .and_then(|(node, entry)| self.pool[node].unwrap_leaf().0.get(entry).cloned())
    }

    /// Get the current value, or `None` if the cursor is at the end.
    pub fn value(&self) -> Option<V> {
        self.path
            .leaf_pos()
            .and_then(|(node, entry)| self.pool[node].unwrap_leaf().1.get(entry).cloned())
    }

    /// Get a mutable reference to the current value, or `None` if the cursor is at the end.
    pub fn value_mut(&mut self) -> Option<&mut V> {
        self.path
            .leaf_pos()
            .and_then(move |(node, entry)| self.pool[node].unwrap_leaf_mut().1.get_mut(entry))
    }

    /// Move this cursor to `key`.
    ///
    /// If `key` is in the map, place the cursor at `key` and return the corresponding value.
    ///
    /// If `key` is not in the set, place the cursor at the next larger element (or the end) and
    /// return `None`.
    pub fn goto(&mut self, elem: K) -> Option<V> {
        self.root.expand().and_then(|root| {
            let v = self.path.find(elem, root, self.pool, self.comp);
            if v.is_none() {
                self.path.normalize(self.pool);
            }
            v
        })
    }

    /// Move this cursor to the first element.
    pub fn goto_first(&mut self) -> Option<V> {
        self.root.map(|root| self.path.first(root, self.pool).1)
    }

    /// Insert `(key, value))` into the map and leave the cursor at the inserted pair.
    ///
    /// If the map did not contain `key`, return `None`.
    ///
    /// If `key` is already present, replace the existing with `value` and return the old value.
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        match self.root.expand() {
            None => {
                let root = self.pool.alloc_node(NodeData::leaf(key, value));
                *self.root = root.into();
                self.path.set_root_node(root);
                None
            }
            Some(root) => {
                // TODO: Optimize the case where `self.path` is already at the correct insert pos.
                let old = self.path.find(key, root, self.pool, self.comp);
                if old.is_some() {
                    *self.path.value_mut(self.pool) = value;
                } else {
                    *self.root = self.path.insert(key, value, self.pool).into();
                }
                old
            }
        }
    }

    /// Insert `(key, value))` into the map and leave the cursor at the inserted pair.
    ///
    /// If the map did not contain `key`, return `None`.
    ///
    /// If `key` is already present, replace the existing with `value` and return the old value.
    pub fn get_or_insert_with(&mut self, key: K, value: impl FnOnce() -> V) -> V {
        match self.root.expand() {
            None => {
                let val = value();
                let root = self.pool.alloc_node(NodeData::leaf(key, val));
                *self.root = root.into();
                self.path.set_root_node(root);
                val
            }
            Some(root) => {
                // TODO: Optimize the case where `self.path` is already at the correct insert pos.
                let old = self.path.find(key, root, self.pool, self.comp);
                if old.is_some() {
                    *self.path.value_mut(self.pool)
                } else {
                    let val = value();
                    *self.root = self.path.insert(key, val, self.pool).into();
                    val
                }
            }
        }
    }

    /// Insert `(key, value))` into the map and leave the cursor at the inserted pair.
    ///
    /// If the map did not contain `key`, return `None`.
    ///
    /// If `key` is already present, replace the existing with `value` and return the old value.
    pub fn update_or_insert_with(
        &mut self,
        key: K,
        update_or_insert: impl FnOnce(Option<&mut V>) -> V,
    ) {
        match self.root.expand() {
            None => {
                let val = update_or_insert(None);
                let root = self.pool.alloc_node(NodeData::leaf(key, val));
                *self.root = root.into();
                self.path.set_root_node(root);
            }
            Some(root) => {
                // TODO: Optimize the case where `self.path` is already at the correct insert pos.
                let old = self.path.find(key, root, self.pool, self.comp);
                if old.is_some() {
                    update_or_insert(Some(self.path.value_mut(self.pool)));
                } else {
                    let val = update_or_insert(None);
                    *self.root = self.path.insert(key, val, self.pool).into();
                }
            }
        }
    }

    ///// Insert `(key, value))` into the map and leave the cursor at the inserted pair.
    /////
    ///// If the map did not contain `key`, return `None`.
    /////
    ///// If `key` is already present, replace the existing with `value` and return the old value.
    //pub fn get_or_insert_with(&mut self, key: K, value: impl FnOnce() -> V) -> V {
    //    match self.root.expand() {
    //        None => {
    //            let val = value();
    //            let root = self.pool.alloc_node(NodeData::leaf(key, val));
    //            *self.root = root.into();
    //            self.path.set_root_node(root);
    //            val
    //        }
    //        Some(root) => {
    //            // TODO: Optimize the case where `self.path` is already at the correct insert pos.
    //            let old = self.path.find(key, root, self.pool, self.comp);
    //            if old.is_some() {
    //                *self.path.value_mut(self.pool)
    //            } else {
    //                let val = value();
    //                *self.root = self.path.insert(key, val, self.pool).into();
    //                val
    //            }
    //        }
    //    }
    //}

    /// Remove the current entry (if any) and return the mapped value.
    /// This advances the cursor to the next entry after the removed one.
    pub fn remove(&mut self) -> Option<V> {
        let value = self.value();
        if value.is_some() {
            *self.root = self.path.remove(self.pool).into();
        }
        value
    }
}

#[derive(Clone, Copy)]
/// An iterator visiting the key-value pairs of a `Map`.
pub struct MapIter<'a, K, V>
where
    K: 'a + Copy,
    V: 'a + Copy,
{
    root: PackedOption<Node>,
    pool: &'a NodePool<MapTypes<K, V>>,
    path: Path<MapTypes<K, V>>,
}

impl<'a, K, V> Iterator for MapIter<'a, K, V>
where
    K: 'a + Copy,
    V: 'a + Copy,
{
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        // We use `self.root` to indicate if we need to go to the first element. Reset to `None`
        // once we've returned the first element. This also works for an empty tree since the
        // `path.next()` call returns `None` when the path is empty. This also fuses the iterator.
        match self.root.take() {
            Some(root) => Some(self.path.first(root, self.pool)),
            None => self.path.next(self.pool),
        }
    }
}
