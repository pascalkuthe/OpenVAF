//! Most locals in the CFG do not correspond to a place in memory.
//! In that case they are immutable SSA values and may only be assigned once
//! Therefore it does not make sense to track these values for each block individually.
//! Instead these valus are tracked globally. Because data_flow analysis usually discourages local
//! state this is realized with interior mutabiliy.
//! This a bit ugly but ultemitly worth as this is the signle most impactful optimization in the compiler.
//! Without this conditonal constant propagation quickly exhausts all available system memory (with
//! this optimization at most 1G ist used)

use std::cell::UnsafeCell;

use ahash::AHashMap;
use cfg::{Const, Local};
use data_flow::lattice::{FlatSet, SparseFlatSetMap};

pub struct SsaConstants {
    vals: UnsafeCell<SparseFlatSetMap<Local, Const>>,
}

impl SsaConstants {
    pub fn new(local_cnt: usize) -> Self {
        Self { vals: UnsafeCell::new(SparseFlatSetMap::new_empty(local_cnt)) }
    }

    pub(super) fn get(&self, local: Local) -> Option<Const> {
        // Access to const_temporaries is save here since the reference immediately and constant_temporaries is sealed within this module so no other references can exist
        unsafe { &mut *self.vals.get() }.get_cloned_flat_set(uu)
    }

    pub(super) fn set(&self, dst: Local, val: FlatSet<Const>) -> bool {
        if let FlatSet::Elem(val) = val {
            let values = unsafe { &mut *self.vals.get() };
            if let Some(old_val) = values.insert(dst, val.clone()) {
                // Temporary is already present
                // Nothing changed
                debug_assert_eq!(val, old_val);
                false
            } else {
                true
            }
        } else {
            false
        }
    }
}
