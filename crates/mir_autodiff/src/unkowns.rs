use std::fmt::Debug;
use std::iter;
use std::mem::take;

use ahash::AHashMap;
use bitset::HybridBitSet;
use mir::{DerivativeInfo, FuncRef, Value};
use stdx::{impl_debug, impl_idx_from};
use typed_indexmap::TiSet;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Unkown(u32);
impl_idx_from!(Unkown(u32));

impl Unkown {
    pub fn assert_first_order(self) -> FirstOrderUnkown {
        self.0.into()
    }
}

impl From<FirstOrderUnkown> for Unkown {
    fn from(unkown: FirstOrderUnkown) -> Unkown {
        Unkown(unkown.0)
    }
}

impl_debug! {match Unkown{
    Unkown(raw) => "unkown{}",raw;
}}

pub use mir::Unkown as FirstOrderUnkown;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NthOrderUnkown(u32);
impl_idx_from!(NthOrderUnkown(u32));

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NthOrderUnkownInfo {
    pub previous_order: Unkown,
    pub base: FirstOrderUnkown,
}

pub struct Unkowns<'a> {
    pub first_order_unkowns: &'a TiSet<FirstOrderUnkown, Value>,
    pub ddx_calls:
        &'a AHashMap<FuncRef, (HybridBitSet<FirstOrderUnkown>, HybridBitSet<FirstOrderUnkown>)>,
    // pub standin_calls: &'a AHashMap<FuncRef, u32>,
    pub higher_order_unkowns: TiSet<NthOrderUnkown, NthOrderUnkownInfo>,
    buf: Vec<FirstOrderUnkown>,
}

impl<'a> Unkowns<'a> {
    pub fn higher_order_unkowns(&self) -> impl Iterator<Item = (Unkown, &NthOrderUnkownInfo)> {
        self.higher_order_unkowns.iter_enumerated().map(|(unkown, info)| {
            let unkown = Unkown(unkown.0 + self.first_order_unkowns.len() as u32);
            (unkown, info)
        })
    }

    pub fn len(&self) -> usize {
        self.first_order_unkowns.len() + self.higher_order_unkowns.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn new(info: &'a DerivativeInfo) -> Unkowns<'a> {
        Self {
            first_order_unkowns: &info.unkowns,
            ddx_calls: &info.ddx_calls,
            higher_order_unkowns: TiSet::default(),
            // standin_calls: &info.standin_calls,
            // don't expect more than 8. th order derivative in most code
            buf: Vec::with_capacity(8),
        }
    }

    pub fn is_unkown(&self, param: Value, unkown: FirstOrderUnkown) -> bool {
        self.first_order_unkowns[unkown] == param
    }

    pub fn previous_order(&self, unkown: Unkown) -> Option<Unkown> {
        if (unkown.0 as usize) < self.first_order_unkowns.len()
            || (unkown.0 as usize)
                >= self.higher_order_unkowns.len() + self.first_order_unkowns.len()
        {
            None
        } else {
            Some(
                self.nth_order_info((usize::from(unkown) - self.first_order_unkowns.len()).into())
                    .previous_order,
            )
        }
    }

    pub fn to_first_order(&self, unkown: Unkown) -> FirstOrderUnkown {
        if (unkown.0 as usize) < self.first_order_unkowns.len()
            || (unkown.0 as usize)
                >= self.higher_order_unkowns.len() + self.first_order_unkowns.len()
        {
            unkown.0.into()
        } else {
            self.nth_order_info((usize::from(unkown) - self.first_order_unkowns.len()).into()).base
        }
    }

    pub fn first_order_unkowns(
        &self,
        unkown: Unkown,
    ) -> impl Iterator<Item = FirstOrderUnkown> + '_ {
        iter::successors(Some(unkown), |it| self.previous_order(*it))
            .map(|unkown| self.to_first_order(unkown))
    }

    #[allow(clippy::needless_collect)] // false positive can't revese successors
    pub fn first_order_unkowns_rev(
        &self,
        unkown: Unkown,
    ) -> impl Iterator<Item = FirstOrderUnkown> + '_ {
        let unkowns: Vec<_> = iter::successors(Some(unkown), |it| self.previous_order(*it))
            .map(|unkown| self.to_first_order(unkown))
            .collect();
        unkowns.into_iter().rev()
    }

    fn nth_order_info(&self, unkown: NthOrderUnkown) -> NthOrderUnkownInfo {
        self.higher_order_unkowns[unkown]
    }

    pub fn raise_order(&mut self, unkown: Unkown, next_unkown: FirstOrderUnkown) -> Unkown {
        self.raise_order_with(unkown, next_unkown, |_, _| true).unwrap()
    }

    pub fn raise_order_with(
        &mut self,
        unkown: Unkown,
        next_unkown: FirstOrderUnkown,
        mut f: impl FnMut(Unkown, Option<NthOrderUnkownInfo>) -> bool,
    ) -> Option<Unkown> {
        // This is save since we never hand out a reference

        let mut prev_orders = take(&mut self.buf);
        prev_orders.extend(self.first_order_unkowns(unkown));

        let mut curr = next_unkown.into();
        for base in prev_orders.drain(..).rev() {
            if !f(base.into(), None) {
                return None;
            }
            let info = NthOrderUnkownInfo { previous_order: curr, base };
            let (unkown, changed) = self.higher_order_unkowns.ensure(info);
            curr = Unkown(unkown.0 + self.first_order_unkowns.len() as u32);

            if changed {
                f(curr, Some(info));
            }
        }

        self.buf = prev_orders;

        Some(curr)
    }
}
