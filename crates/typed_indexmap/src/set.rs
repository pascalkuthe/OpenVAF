use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Index;

use indexmap::IndexSet;

pub struct TiSet<K, V> {
    /// raw set property
    pub raw: IndexSet<V, ahash::RandomState>,
    _marker: PhantomData<fn(K) -> K>,
}

impl<K, V> Debug for TiSet<K, V>
where
    K: From<usize> + Debug,
    V: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter_enumerated()).finish()
    }
}

impl<K, V: Clone> Clone for TiSet<K, V> {
    fn clone(&self) -> Self {
        Self { raw: self.raw.clone(), _marker: self._marker }
    }
}

impl<K, V> Default for TiSet<K, V> {
    fn default() -> Self {
        Self { raw: IndexSet::default(), _marker: PhantomData }
    }
}

impl<K, V> Eq for TiSet<K, V> where V: Hash + Eq {}

impl<K, V> PartialEq for TiSet<K, V>
where
    V: Hash + Eq,
{
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<K, V> TiSet<K, V> {
    pub fn len(&self) -> usize {
        self.raw.len()
    }

    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn clear(&mut self) {
        self.raw.clear()
    }

    pub fn first(&self) -> Option<&V> {
        self.raw.first()
    }

    pub fn last(&self) -> Option<&V> {
        self.raw.last()
    }
}

impl<K, V> TiSet<K, V>
where
    K: Into<usize>,
    V: Eq + Hash,
{
    pub fn insert(&mut self, val: V) -> bool {
        self.raw.insert(val)
    }

    pub fn replace(&mut self, index: K, new_val: V) -> V {
        assert!(self.insert(new_val));
        self.raw.swap_remove_index(index.into()).unwrap()
    }
}

impl<K: From<usize>, V> TiSet<K, V> {
    pub fn iter_enumerated(&self) -> impl Iterator<Item = (K, &V)> {
        self.raw.iter().enumerate().map(|(index, val)| (index.into(), val))
    }
}

impl<K, V> TiSet<K, V>
where
    K: From<usize>,
    V: Eq + Hash,
{
    pub fn ensure(&mut self, val: V) -> (K, bool) {
        let (id, changed) = self.raw.insert_full(val);
        (id.into(), changed)
    }

    pub fn index(&self, val: &V) -> Option<K> {
        self.raw.get_index_of(val).map(K::from)
    }

    pub fn contains(&self, val: &V) -> bool {
        self.raw.contains(val)
    }
}

impl<K, V> Index<K> for TiSet<K, V>
where
    K: Into<usize>,
    V: Eq + Hash,
{
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        self.raw.get_index(index.into()).unwrap()
    }
}
