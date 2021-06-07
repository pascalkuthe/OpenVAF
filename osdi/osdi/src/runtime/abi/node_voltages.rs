//  * ******************************************************************************************
//  *  Copyright (c) 2020 Pascal Kuthe. This file is part of the OSDI project.
//  *  It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/osdi/blob/master/LICENSE.
//  *  No part of OpenVAF-Types, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::model_info_store::{NodeId, Voltage};
use crate::ModelInfoStore;
use index_vec::{index_box, IndexSlice};
use std::ops::{Deref, DerefMut};

pub struct NodePotentialOffsets(Box<IndexSlice<NodeId, [usize]>>);

impl NodePotentialOffsets {
    pub fn new(size: usize) -> Self {
        Self(index_box![0;size])
    }

    pub fn from_info_store(info_store: &ModelInfoStore) -> Self {
        Self::new(info_store.nodes.len())
    }

    #[inline]
    pub unsafe fn get_pot_raw(&self, base: *const f64, voltage: Voltage) -> f64 {
        let lo_ptr = base.add(self[voltage.lo]);
        let hi_ptr = base.add(self[voltage.hi]);

        std::ptr::read(hi_ptr) - std::ptr::read(lo_ptr)
    }

    #[inline]
    pub unsafe fn increment_raw(&self, base: *mut f64, node: NodeId, val: f64) {
        let ptr = base.add(self[node]);
        std::ptr::write(ptr, std::ptr::read(ptr) + val)
    }

    #[inline]
    pub fn get_voltage(&self, node_potentials: &[f64], voltage: Voltage) -> f64 {
        node_potentials[self[voltage.hi]] - node_potentials[self[voltage.lo]]
    }
}

impl Deref for NodePotentialOffsets {
    type Target = IndexSlice<NodeId, [usize]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for NodePotentialOffsets {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
