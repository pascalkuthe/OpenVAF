use std::cell::UnsafeCell;

use bitset::HybridBitSet;
use cfg::{Callback, CfgParam};
use indexmap::IndexMap;
use stdx::impl_idx_from;
use typed_indexmap::{TiMap, TiSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Unkown(u32);
impl_idx_from!(Unkown(u32));

impl From<FirstOrderUnkown> for Unkown {
    fn from(unkown: FirstOrderUnkown) -> Unkown {
        Unkown(unkown.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FirstOrderUnkown(u32);
impl_idx_from!(FirstOrderUnkown(u32));

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NthOrderUnkown(u32);
impl_idx_from!(NthOrderUnkown(u32));

pub type FirstOrderUnkownInfo = Box<[(CfgParam, u64)]>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NthOrderUnkownInfo {
    previous_order: Unkown,
    base: FirstOrderUnkown,
}

pub struct Unkowns {
    first_order_unkowns: TiMap<FirstOrderUnkown, Callback, FirstOrderUnkownInfo>,
    higher_order_unkowns: UnsafeCell<TiSet<NthOrderUnkown, NthOrderUnkownInfo>>,
}

impl Unkowns {
    pub fn len(&self) -> usize {
        self.first_order_unkowns.len() + unsafe { &*self.higher_order_unkowns.get() }.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn new(unkowns: impl IntoIterator<Item = (Callback, FirstOrderUnkownInfo)>) -> Unkowns {
        let first_order_unkowns: IndexMap<_, _, ahash::RandomState> = unkowns.into_iter().collect();
        Self {
            first_order_unkowns: first_order_unkowns.into(),
            higher_order_unkowns: UnsafeCell::new(TiSet::default()),
        }
    }

    // pub fn new(unkowns: TiMap<NthOrderUnkownInfo, Callback, FirstOrderUnkownInfo>) -> Unkowns {
    //     Unkowns { first_order_unkowns: unkowns, higher_order_unkowns: UnsafeCell::new(Vec::new()) }
    // }

    pub fn derive_param(&self, unkown: Unkown, param: CfgParam) -> f64 {
        let base_unkown = self.to_first_order(unkown);
        self.first_order_unkowns[base_unkown]
            .iter()
            .find(|(it, _)| *it == param)
            .map_or(0.0, |(_, val)| f64::from_bits(*val))
    }

    pub fn to_first_order(&self, unkown: Unkown) -> FirstOrderUnkown {
        if (unkown.0 as usize) < self.first_order_unkowns.len() {
            unkown.0.into()
        } else {
            self.nth_order_info((usize::from(unkown) - self.first_order_unkowns.len()).into()).base
        }
    }

    fn nth_order_info(&self, unkown: NthOrderUnkown) -> NthOrderUnkownInfo {
        // This is save since we never hand out a reference (only a copy)
        let unkowns = unsafe { &*self.higher_order_unkowns.get() };
        unkowns[unkown]
    }

    pub fn raise_order<'a>(
        &'a self,
        previous_order: &HybridBitSet<Unkown>,
        dst: &mut Vec<Unkown>,
        base: FirstOrderUnkown,
    ) {
        // This is save since we never hand out a reference
        let higher_order_unkowns = unsafe { &mut *self.higher_order_unkowns.get() };
        for previous_order in previous_order.iter() {
            let higher_order_unkown =
                higher_order_unkowns.ensure(NthOrderUnkownInfo { previous_order, base }).0;
            let unkown = (usize::from(higher_order_unkown) + self.first_order_unkowns.len()).into();
            dst.push(unkown)
        }
    }

    pub fn callback_unkown(&self, callback: Callback) -> Option<FirstOrderUnkown> {
        self.first_order_unkowns.index(&callback)
    }
}
