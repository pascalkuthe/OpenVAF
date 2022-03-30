use std::fmt::Debug;
use std::iter;
use std::mem::take;

use mir::{FuncRef, Value, F_ZERO};
use stdx::{impl_debug, impl_idx_from};
use typed_indexmap::{TiMap, TiSet};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Unkown(u32);
impl_idx_from!(Unkown(u32));

impl From<FirstOrderUnkown> for Unkown {
    fn from(unkown: FirstOrderUnkown) -> Unkown {
        Unkown(unkown.0)
    }
}

impl_debug! {match Unkown{
    Unkown(raw) => "unkown{}",raw;
}}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FirstOrderUnkown(u32);
impl_idx_from!(FirstOrderUnkown(u32));

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NthOrderUnkown(u32);
impl_idx_from!(NthOrderUnkown(u32));

pub type FirstOrderUnkownInfo = Box<[(Value, Value)]>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NthOrderUnkownInfo {
    previous_order: Unkown,
    base: FirstOrderUnkown,
}

pub struct Unkowns<'a> {
    pub first_order_unkowns: &'a TiMap<FirstOrderUnkown, FuncRef, FirstOrderUnkownInfo>,
    higher_order_unkowns: TiSet<NthOrderUnkown, NthOrderUnkownInfo>,
    buf: Vec<FirstOrderUnkown>,
}

impl<'a> Unkowns<'a> {
    pub fn len(&self) -> usize {
        self.first_order_unkowns.len() + self.higher_order_unkowns.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn new(
        first_order_unkowns: &'a TiMap<FirstOrderUnkown, FuncRef, FirstOrderUnkownInfo>,
    ) -> Unkowns<'a> {
        Self {
            first_order_unkowns,
            higher_order_unkowns: TiSet::default(),
            // don't expect more than 8. th order derivative in most code
            buf: Vec::with_capacity(8),
        }
    }

    // pub fn new(unkowns: TiMap<NthOrderUnkownInfo, Callback, FirstOrderUnkownInfo>) -> Unkowns {
    //     Unkowns { first_order_unkowns: unkowns, higher_order_unkowns: UnsafeCell::new(Vec::new()) }
    // }

    pub fn param_derivative(&self, param: Value, unkown: FirstOrderUnkown) -> Value {
        self.first_order_unkowns[unkown]
            .iter()
            .find(|(it, _)| *it == param)
            .map_or(F_ZERO, |(_, val)| *val)
    }

    pub fn previous_order(&self, unkown: Unkown) -> Option<Unkown> {
        if (unkown.0 as usize) < self.first_order_unkowns.len() {
            None
        } else {
            Some(
                self.nth_order_info((usize::from(unkown) - self.first_order_unkowns.len()).into())
                    .previous_order,
            )
        }
    }

    pub fn to_first_order(&self, unkown: Unkown) -> FirstOrderUnkown {
        if (unkown.0 as usize) < self.first_order_unkowns.len() {
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
        // This is save since we never hand out a reference

        let mut prev_orders = take(&mut self.buf);
        prev_orders.extend(self.first_order_unkowns(unkown));

        let mut curr = next_unkown.into();
        for base in prev_orders.drain(..).rev() {
            let unkown = &mut self
                .higher_order_unkowns
                .ensure(NthOrderUnkownInfo { previous_order: curr, base })
                .0;
            curr = Unkown(unkown.0 + self.first_order_unkowns.len() as u32);
        }

        self.buf = prev_orders;

        curr
    }

    pub fn callback_unkown(&self, callback: FuncRef) -> Option<FirstOrderUnkown> {
        self.first_order_unkowns.index(&callback)
    }
}
