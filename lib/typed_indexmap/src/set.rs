use std::fmt::Debug;
use std::hash::Hash;
use std::iter;
use std::marker::PhantomData;
use std::ops::Index;

use indexmap::{Equivalent, IndexSet};

pub type Iter<'a, K, V> =
    iter::Map<iter::Enumerate<indexmap::set::Iter<'a, V>>, fn((usize, &'a V)) -> (K, &'a V)>;

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

impl<K, V> TiSet<K, V> {
    pub fn with_capacity(cap: usize) -> TiSet<K, V> {
        TiSet {
            raw: IndexSet::with_capacity_and_hasher(cap, ahash::RandomState::default()),
            _marker: PhantomData,
        }
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
    pub fn iter_enumerated(&self) -> Iter<K, V> {
        self.iter().enumerate().map(|(index, val)| (index.into(), val))
    }
}

impl<K, V> TiSet<K, V> {
    pub fn iter(&self) -> indexmap::set::Iter<'_, V> {
        self.raw.iter()
    }
}

impl<K, V> TiSet<K, V>
where
    K: From<usize> + Debug,
    V: Eq + Hash + Debug,
{
    pub fn unwrap_index(&self, val: &V) -> K {
        match self.raw.get_index_of(val) {
            Some(i) => i.into(),
            None => unreachable!("Called unwrap_index on {:?}\nNot found in {:?}", val, self),
        }
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

    pub fn index<Q: ?Sized>(&self, val: &Q) -> Option<K>
    where
        Q: Hash + Equivalent<V>,
    {
        self.raw.get_index_of(val).map(K::from)
    }

    pub fn contains<Q: ?Sized>(&self, val: &Q) -> bool
    where
        Q: Hash + Equivalent<V>,
    {
        self.raw.contains(val)
    }

    pub fn retain(&mut self, mut f: impl FnMut(K, &V) -> bool) {
        let mut i = 0;
        self.raw.retain(|val| {
            let res = f(i.into(), val);
            i += 1;
            res
        })
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

impl<K, V> FromIterator<V> for TiSet<K, V>
where
    V: Eq + Hash,
{
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Self { raw: iter.into_iter().collect(), _marker: PhantomData }
    }
}
