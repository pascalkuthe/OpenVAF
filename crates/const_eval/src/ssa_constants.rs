//! Most locals in the CFG do not correspond to a place in memory.
//! In that case they are immutable SSA values and may only be assigned once
//! Therefore it does not make sense to track these values for each block individually.
//! Instead these values are tracked globally. Because data_flow analysis usually discourages local
//! state this is realized with interior mutability.
//! This a bit ugly but ultimately worth as this is the single most impactful optimization in the compiler.
//! Without this conditional constant propagation quickly exhausts all available system memory (with
//! this optimization at most 1Gb ist used)

use std::cell::UnsafeCell;
use std::fmt::Debug;

use cfg::{Const, Local};
use data_flow::lattice::{FlatSet, SparseFlatSetMap};

pub struct SsaConstants {
    vals: UnsafeCell<SparseFlatSetMap<Local, Const>>,
}

impl SsaConstants {
    pub fn new(local_cnt: usize) -> Self {
        Self { vals: UnsafeCell::new(SparseFlatSetMap::new_empty(local_cnt)) }
    }

    pub fn get(&self, local: Local) -> FlatSet<Const> {
        // Access to const_temporaries is save here since the reference immediately and constant_temporaries is sealed within this module so no other references can exist
        unsafe { &*self.vals.get() }.get_cloned_flat_set(local)
    }

    pub fn get_option(&self, local: Local) -> Option<Const> {
        // Access to const_temporaries is save here since the reference immediately and constant_temporaries is sealed within this module so no other references can exist
        unsafe { &*self.vals.get() }.element_sets.get(&local).cloned()
    }

    pub(super) fn join_into(&self, local: Local, dst: &mut FlatSet<Const>) -> bool {
        // Access to const_temporaries is save here since the reference immediately and constant_temporaries is sealed within this module so no other references can exist
        unsafe { &mut *self.vals.get() }.join_into(local, dst)
    }

    pub(super) fn set(&self, dst: Local, val: FlatSet<Const>) -> bool {
        unsafe { &mut *self.vals.get() }.set_flat_set(dst, val)
    }
}

impl Debug for SsaConstants {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { &*self.vals.get() }.fmt(f)
    }
}
