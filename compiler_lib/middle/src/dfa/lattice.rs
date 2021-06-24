/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

/*
    Adapted from https://github.com/rust-lang/rust/blob/master/compiler/rustc_mir/src/dataflow/framework/lattice.rs under MIT-License

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
    ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
    TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
    PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
    SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
    IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
*/

//! Traits used to represent [lattices] for use as the domain of a dataflow analysis.
//!
//! # Overview
//!
//! The most common lattice is a powerset of some set `S`, ordered by [set inclusion]. The [Hasse
//! diagram] for the powerset of a set with two elements (`X` and `Y`) is shown below. Note that
//! distinct elements at the same height in a Hasse diagram (e.g. `{X}` and `{Y}`) are
//! *incomparable*, not equal.
//!
//! ```text
//!      {X, Y}    <- top
//!       /  \
//!    {X}    {Y}
//!       \  /
//!        {}      <- bottom
//!
//! ```
//!
//! The defining characteristic of a lattice—the one that differentiates it from a [partially
//! ordered set][poset]—is the existence of a *unique* least upper and greatest lower bound for
//! every pair of elements. The lattice join operator (`∨`) returns the least upper bound, and the
//! lattice meet operator (`∧`) returns the greatest lower bound. Types that implement one operator
//! but not the other are known as semilattices. Dataflow analysis only uses the join operator and
//! will work with any join-semilattice, but both should be specified when possible.
//!
//! ## `PartialOrd`
//!
//! Given that they represent partially ordered sets, you may be surprised that [`JoinSemiLattice`]
//! and [`MeetSemiLattice`] do not have [`PartialOrd`][std::cmp::PartialOrd] as a supertrait. This
//! is because most standard library types use lexicographic ordering instead of set inclusion for
//! their `PartialOrd` impl. Since we do not actually need to compare lattice elements to run a
//! dataflow analysis, there's no need for a newtype wrapper with a custom `PartialOrd` impl. The
//! only benefit would be the ability to check that the least upper (or greatest lower) bound
//! returned by the lattice join (or meet) operator was in fact greater (or lower) than the inputs.
//!
//! [lattices]: https://en.wikipedia.org/wiki/Lattice_(order)
//! [set inclusion]: https://en.wikipedia.org/wiki/Subset
//! [Hasse diagram]: https://en.wikipedia.org/wiki/Hasse_diagram
//! [poset]: https://en.wikipedia.org/wiki/Partially_ordered_set

use openvaf_data_structures::bit_set::BitSet;
use openvaf_data_structures::index_vec::{Idx, IndexVec};
use openvaf_data_structures::{iter, HashMap};
use std::fmt::Debug;
use std::hash::Hash;
use std::option::Option::Some;

/// A [partially ordered set][poset] that has a [least upper bound][lub] for any pair of elements
/// in the set.
///
/// [lub]: https://en.wikipedia.org/wiki/Infimum_and_supremum
/// [poset]: https://en.wikipedia.org/wiki/Partially_ordered_set
pub trait JoinSemiLattice: PartialEq {
    /// Computes the least upper bound of two elements, storing the result in `self` and returning
    /// `true` if `self` has changed.
    ///
    /// The lattice join operator is abbreviated as `∨`.
    fn join(&mut self, other: &Self) -> bool;
}

/// A [partially ordered set][poset] that has a [greatest lower bound][glb] for any pair of
/// elements in the set.
///
/// Dataflow analyses only require that their domains implement [`JoinSemiLattice`], not
/// `MeetSemiLattice`. However, types that will be used as dataflow domains should implement both
/// so that they can be used with [`Dual`].
///
/// [glb]: https://en.wikipedia.org/wiki/Infimum_and_supremum
/// [poset]: https://en.wikipedia.org/wiki/Partially_ordered_set
pub trait MeetSemiLattice: PartialEq {
    /// Computes the greatest lower bound of two elements, storing the result in `self` and
    /// returning `true` if `self` has changed.
    ///
    /// The lattice meet operator is abbreviated as `∧`.
    fn meet(&mut self, other: &Self) -> bool;
}

/// A `bool` is a "two-point" lattice with `true` as the top element and `false` as the bottom:
///
/// ```text
///      true
///        |
///      false
/// ```
impl JoinSemiLattice for bool {
    fn join(&mut self, other: &Self) -> bool {
        if let (false, true) = (*self, *other) {
            *self = true;
            return true;
        }

        false
    }
}

impl MeetSemiLattice for bool {
    fn meet(&mut self, other: &Self) -> bool {
        if let (true, false) = (*self, *other) {
            *self = false;
            return true;
        }

        false
    }
}

/// A tuple (or list) of lattices is itself a lattice whose least upper bound is the concatenation
/// of the least upper bounds of each element of the tuple (or list).
///
/// In other words:
///     (A₀, A₁, ..., Aₙ) ∨ (B₀, B₁, ..., Bₙ) = (A₀∨B₀, A₁∨B₁, ..., Aₙ∨Bₙ)
impl<I: Idx, T: JoinSemiLattice> JoinSemiLattice for IndexVec<I, T> {
    fn join(&mut self, other: &Self) -> bool {
        debug_assert_eq!(self.len(), other.len());

        let mut changed = false;
        for (a, b) in iter::zip(self, other) {
            changed |= a.join(b);
        }
        changed
    }
}

impl<I: Idx, T: MeetSemiLattice> MeetSemiLattice for IndexVec<I, T> {
    fn meet(&mut self, other: &Self) -> bool {
        debug_assert_eq!(self.len(), other.len());

        let mut changed = false;
        for (a, b) in iter::zip(self, other) {
            changed |= a.meet(b);
        }
        changed
    }
}

/// A `BitSet` represents the lattice formed by the powerset of all possible values of
/// the index type `T` ordered by inclusion. Equivalently, it is a tuple of "two-point" lattices,
/// one for each possible value of `T`.
impl<T: Idx> JoinSemiLattice for BitSet<T> {
    fn join(&mut self, other: &Self) -> bool {
        self.union(other)
    }
}

impl<T: Idx> MeetSemiLattice for BitSet<T> {
    fn meet(&mut self, other: &Self) -> bool {
        self.intersect(other)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SparseFlatSetMap<K: Hash + Eq + Idx, V: PartialEq + Clone> {
    pub element_sets: HashMap<K, V>,
    pub top_sets: BitSet<K>,
}

impl<K: Hash + Eq + Idx, V: PartialEq + Clone> SparseFlatSetMap<K, V> {
    pub fn new_empty(domain_size: usize) -> Self {
        Self {
            element_sets: HashMap::new(),
            top_sets: BitSet::new_empty(domain_size),
        }
    }
    pub fn get_flat_set(&self, key: K) -> FlatSet<&V> {
        if self.top_sets.contains(key) {
            FlatSet::Top
        } else if let Some(val) = self.element_sets.get(&key) {
            FlatSet::Elem(val)
        } else {
            FlatSet::Bottom
        }
    }

    pub fn get_cloned_flat_set(&self, key: K) -> FlatSet<V> {
        if self.top_sets.contains(key) {
            FlatSet::Top
        } else if let Some(val) = self.element_sets.get(&key) {
            FlatSet::Elem(val.clone())
        } else {
            FlatSet::Bottom
        }
    }

    pub fn set_flat_set(&mut self, key: K, val: FlatSet<V>) {
        match val {
            FlatSet::Top => {
                self.element_sets.remove(&key);
                self.top_sets.insert(key);
            }
            FlatSet::Elem(val) => {
                self.element_sets.insert(key, val);
                self.top_sets.remove(key);
            }
            FlatSet::Bottom => {
                self.element_sets.remove(&key);
                self.top_sets.remove(key);
            }
        }
    }

    pub fn join_into(&self, key: K, dst: &mut FlatSet<V>) -> bool {
        match dst {
            FlatSet::Bottom => {
                if self.top_sets.contains(key) {
                    *dst = FlatSet::Top;
                    true
                } else if let Some(val) = self.element_sets.get(&key) {
                    *dst = FlatSet::Elem(val.clone());
                    true
                } else {
                    false
                }
            }
            FlatSet::Elem(x) => {
                if self.top_sets.contains(key) {
                    *dst = FlatSet::Top;
                    true
                } else {
                    match self.element_sets.get(&key) {
                        Some(val) if val != x => {
                            *dst = FlatSet::Top;
                            true
                        }
                        _ => false,
                    }
                }
            }
            FlatSet::Top => false,
        }
    }
}

impl<K: Hash + Eq + Idx, V: PartialEq + Clone> JoinSemiLattice for SparseFlatSetMap<K, V> {
    fn join(&mut self, other: &Self) -> bool {
        // Join top sets
        let mut changed = self.top_sets.join(&other.top_sets);

        // Merge elements
        for (key, val) in &other.element_sets {
            self.element_sets.entry(key.clone()).or_insert_with(|| {
                changed = true;
                val.clone()
            });
        }

        // Remove elements that were already tops sets
        // Change elements to top sets that missmatch
        let top_sets = &mut self.top_sets;
        self.element_sets
            .retain(|key, val| match other.element_sets.get(key) {
                Some(other) if other != val => {
                    top_sets.insert(*key);
                    changed = true;
                    false
                }
                Some(_) | None if top_sets.contains(*key) => false,
                Some(_) | None => true,
            });

        changed
    }
}

/// The counterpart of a given semilattice `T` using the [inverse order].
///
/// The dual of a join-semilattice is a meet-semilattice and vice versa. For example, the dual of a
/// powerset has the empty set as its top element and the full set as its bottom element and uses
/// set *intersection* as its join operator.
///
/// [inverse order]: https://en.wikipedia.org/wiki/Duality_(order_theory)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Dual<T>(pub T);

impl<T> std::borrow::Borrow<T> for Dual<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<T> std::borrow::BorrowMut<T> for Dual<T> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T: MeetSemiLattice> JoinSemiLattice for Dual<T> {
    fn join(&mut self, other: &Self) -> bool {
        self.0.meet(&other.0)
    }
}

impl<T: JoinSemiLattice> MeetSemiLattice for Dual<T> {
    fn meet(&mut self, other: &Self) -> bool {
        self.0.join(&other.0)
    }
}

/// Extends a type `T` with top and bottom elements to make it a partially ordered set in which no
/// value of `T` is comparable with any other. A flat set has the following [Hasse diagram]:
///
/// ```text
///         top
///       / /  \ \
/// all possible values of `T`
///       \ \  / /
///        bottom
/// ```
///
/// [Hasse diagram]: https://en.wikipedia.org/wiki/Hasse_diagram
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FlatSet<T: PartialEq> {
    Bottom,
    Elem(T),
    Top,
}

impl<T: Clone + Eq> JoinSemiLattice for FlatSet<T> {
    fn join(&mut self, other: &Self) -> bool {
        let result = match (&*self, other) {
            (Self::Top, _) | (_, Self::Bottom) => return false,
            (Self::Elem(a), Self::Elem(b)) if a == b => return false,

            (Self::Bottom, Self::Elem(x)) => Self::Elem(x.clone()),

            _ => Self::Top,
        };

        *self = result;
        true
    }
}

impl<T: Clone + Eq> MeetSemiLattice for FlatSet<T> {
    fn meet(&mut self, other: &Self) -> bool {
        let result = match (&*self, other) {
            (Self::Bottom, _) | (_, Self::Top) => return false,
            (Self::Elem(ref a), Self::Elem(ref b)) if a == b => return false,

            (Self::Top, Self::Elem(ref x)) => Self::Elem(x.clone()),

            _ => Self::Bottom,
        };

        *self = result;
        true
    }
}

impl<T: PartialEq> FlatSet<T> {
    pub fn to_option(&self) -> Option<&T> {
        if let Self::Elem(val) = self {
            Some(val)
        } else {
            None
        }
    }

    pub fn into_option(self) -> Option<T> {
        if let Self::Elem(val) = self {
            Some(val)
        } else {
            None
        }
    }

    pub fn map<Y: PartialEq>(self, f: impl FnOnce(T) -> Y) -> FlatSet<Y> {
        match self {
            Self::Elem(x) => FlatSet::Elem(f(x)),
            Self::Top => FlatSet::Top,
            Self::Bottom => FlatSet::Bottom,
        }
    }

    pub fn and_then<Y: PartialEq>(self, f: impl FnOnce(T) -> FlatSet<Y>) -> FlatSet<Y> {
        match self {
            Self::Elem(x) => f(x),
            Self::Top => FlatSet::Top,
            Self::Bottom => FlatSet::Bottom,
        }
    }

    pub fn apply_binary_op(self, other: Self, f: impl FnOnce(T, T) -> T) -> Self {
        match (self, other) {
            (Self::Elem(arg1), Self::Elem(arg2)) => FlatSet::Elem(f(arg1, arg2)),
            (Self::Top, _) | (_, Self::Top) => FlatSet::Top,
            _ => FlatSet::Bottom,
        }
    }
}
impl<T: PartialEq + Clone> FlatSet<T> {
    pub fn join_elem(&mut self, val: &T) -> bool {
        match self {
            Self::Elem(x) if &*x == val => false,
            Self::Top => false,
            Self::Bottom => {
                *self = Self::Elem(val.clone());
                true
            }
            dst => {
                *dst = Self::Top;
                true
            }
        }
    }
}

impl<T: Debug + PartialEq> FlatSet<T> {
    pub fn expect_elem(self, msg: &'static str) -> T {
        if let Self::Elem(val) = self {
            val
        } else {
            panic!("Expected a value found {:?}: {}", self, msg)
        }
    }

    pub fn unwrap(self) -> T {
        if let Self::Elem(val) = self {
            val
        } else {
            panic!("Expected a constant value bound found {:?}!", self)
        }
    }
}

impl<T: Clone + PartialEq> FlatSet<&T> {
    pub fn cloned(&self) -> FlatSet<T> {
        match *self {
            FlatSet::Bottom => FlatSet::Bottom,
            FlatSet::Elem(x) => FlatSet::Elem(x.clone()),
            FlatSet::Top => FlatSet::Top,
        }
    }
}
