use std::fmt::Debug;
use std::iter;
use std::mem::take;

use ahash::AHashMap;
use bitset::HybridBitSet;
use mir::Unknown;
use mir::{FuncRef, KnownDerivatives, Value};
use stdx::{impl_debug, impl_idx_from};
use typed_indexmap::TiSet;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Derivative(u32);
impl_idx_from!(Derivative(u32));

impl Derivative {
    pub fn assert_first_order(self) -> Unknown {
        self.0.into()
    }
}

impl_debug! {match Derivative{
    Derivative(raw) => "derivative{}",raw;
}}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DerivativeInfo {
    pub previous_order: Option<Derivative>,
    pub base: Unknown,
}

pub struct DerivativeIntern<'a> {
    pub unknowns: TiSet<Unknown, Value>,
    pub ddx_calls: &'a AHashMap<FuncRef, (HybridBitSet<Unknown>, HybridBitSet<Unknown>)>,
    pub derivatives: TiSet<Derivative, DerivativeInfo>,
    buf: Vec<Unknown>,
}

impl<'a> DerivativeIntern<'a> {
    pub fn num_derivatives(&self) -> usize {
        self.derivatives.len()
    }

    pub fn num_unknowns(&self) -> usize {
        self.unknowns.len()
    }

    pub fn ensure_unknown(&mut self, val: Value) -> Unknown {
        let unknown = self.unknowns.ensure(val).0;
        self.derivatives.ensure(DerivativeInfo { previous_order: None, base: unknown });
        unknown
    }

    pub fn new(known: &'a KnownDerivatives) -> DerivativeIntern<'a> {
        let derivatives = known
            .unknowns
            .iter_enumerated()
            .map(|(base, _)| DerivativeInfo { base, previous_order: None })
            .collect();

        Self {
            unknowns: known.unknowns.clone(),
            ddx_calls: &known.ddx_calls,
            derivatives,
            // standin_calls: &info.standin_calls,
            // don't expect more than 8. th order derivative in most code
            buf: Vec::with_capacity(8),
        }
    }

    pub fn previous_order(&self, derivative: Derivative) -> Option<Derivative> {
        self.derivatives[derivative].previous_order
    }

    pub fn get_unknown(&self, derivative: Derivative) -> Unknown {
        self.derivatives[derivative].base
    }

    pub fn get_base_derivative(&self, derivative: Derivative) -> Derivative {
        self.to_derivative(self.derivatives[derivative].base)
    }

    pub fn to_derivative(&self, base: Unknown) -> Derivative {
        self.derivatives.unwrap_index(&DerivativeInfo { previous_order: None, base })
    }

    pub fn unknowns(&self, derivative: Derivative) -> impl Iterator<Item = Unknown> + '_ {
        iter::successors(Some(derivative), |it| self.previous_order(*it))
            .map(|unknown| self.get_unknown(unknown))
    }

    #[allow(clippy::needless_collect)] // false positive can't revese successors
    pub fn unknowns_rev(&self, derivative: Derivative) -> impl Iterator<Item = Unknown> + '_ {
        let unknowns: Vec<_> = iter::successors(Some(derivative), |it| self.previous_order(*it))
            .map(|unknown| self.get_unknown(unknown))
            .collect();
        unknowns.into_iter().rev()
    }

    pub fn raise_order(&mut self, derivative: Derivative, next_unknown: Unknown) -> Derivative {
        self.raise_order_with(derivative, next_unknown, |_| true).unwrap().0
    }

    pub fn intern(&mut self, derivative: DerivativeInfo) -> (Derivative, bool) {
        self.derivatives.ensure(derivative)
    }

    pub fn raise_order_with(
        &mut self,
        derivative: Derivative,
        next_unknown: Unknown,
        f: impl Fn(Derivative) -> bool,
    ) -> Option<(Derivative, bool)> {
        // This is save since we never hand out a reference

        let mut changed = false;
        let mut prev_orders = take(&mut self.buf);
        prev_orders.extend(self.unknowns(derivative));

        let mut curr = self.to_derivative(next_unknown);
        for base in prev_orders.drain(..).rev() {
            if !f(self.to_derivative(base)) {
                return None;
            }
            let info = DerivativeInfo { previous_order: Some(curr), base };
            (curr, changed) = self.intern(info);
        }

        self.buf = prev_orders;

        Some((curr, changed))
    }
}
