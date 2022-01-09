use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Index;

use indexmap::IndexMap;

pub struct TiMap<I, K, V> {
    /// raw set property
    pub raw: IndexMap<K, V, ahash::RandomState>,
    _marker: PhantomData<fn(I) -> I>,
}

impl<I, K, V> Debug for TiMap<I, K, V>
where
    I: From<usize> + Debug,
    K: Debug,
    V: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter_enumerated()).finish()
    }
}

impl<I, K: Clone, V: Clone> Clone for TiMap<I, K, V> {
    fn clone(&self) -> Self {
        Self { raw: self.raw.clone(), _marker: self._marker }
    }
}

impl<I, K, V> Default for TiMap<I, K, V> {
    fn default() -> Self {
        Self { raw: IndexMap::default(), _marker: PhantomData }
    }
}

impl<I, K, V> Eq for TiMap<I, K, V>
where
    K: Hash + Eq,
    V: Eq,
{
}

impl<I, K, V> PartialEq for TiMap<I, K, V>
where
    K: Hash + Eq,
    V: Eq,
{
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<I, K, V> TiMap<I, K, V> {
    pub fn len(&self) -> usize {
        self.raw.len()
    }

    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn clear(&mut self) {
        self.raw.clear()
    }

    pub fn first(&self) -> Option<(&K, &V)> {
        self.raw.first()
    }

    pub fn last(&self) -> Option<(&K, &V)> {
        self.raw.last()
    }
}

impl<I, K, V> TiMap<I, K, V>
where
    K: Eq + Hash,
    V: Eq,
{
    pub fn insert(&mut self, key: K, val: V) -> Option<V> {
        self.raw.insert(key, val)
    }

}

impl<I: From<usize>, K, V> TiMap<I, K, V> {
    pub fn iter_enumerated(&self) -> impl Iterator<Item = (I, (&K, &V))> {
        self.raw.iter().enumerate().map(|(index, val)| (index.into(), val))
    }
}

impl<I, K, V> TiMap<I, K, V>
where
    I: From<usize>,
    K: Eq + Hash,
    V: Eq,
{
    pub fn index(&self, key: &K) -> Option<I> {
        self.raw.get_index_of(key).map(I::from)
    }

    pub fn index_and_val(&self, key: &K) -> Option<(I, &V)> {
        self.raw.get_full(key).map(|(index, _, val)| (index.into(), val))
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.raw.contains_key(key)
    }
}

impl<I, K, V> Index<I> for TiMap<I, K, V>
where
    I: Into<usize>,
    K: Eq + Hash,
    V: Eq,
{
    type Output = V;

    fn index(&self, index: I) -> &Self::Output {
        self.raw.get_index(index.into()).unwrap().1
    }
}

impl<I, K, V> From<IndexMap<K, V, ahash::RandomState>> for TiMap<I, K, V> {
    fn from(raw: IndexMap<K, V, ahash::RandomState>) -> Self {
        TiMap { raw, _marker: PhantomData }
    }
}
