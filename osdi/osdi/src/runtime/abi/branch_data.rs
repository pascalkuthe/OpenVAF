//  * ******************************************************************************************
//  *  Copyright (c) 2020 Pascal Kuthe. This file is part of the OSDI project.
//  *  It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/osdi/blob/master/LICENSE.
//  *  No part of OpenVAF-Types, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::model_info_store::{Current, LimitId, Stamp};
use crate::ModelInfoStore;
use std::ops::{Deref, DerefMut};

#[repr(transparent)]
pub struct BranchCurrentsAndConductance(Box<[f64]>);

impl BranchCurrentsAndConductance {
    pub fn new(size: usize) -> Self {
        Self(vec![0.0; size].into_boxed_slice())
    }

    pub fn from_info_store(info_store: &ModelInfoStore) -> Self {
        Self::new(info_store.current_and_conductance_size)
    }

    #[inline]
    pub fn get_current(&self, branch: &Current) -> f64 {
        self.0[branch.pos]
    }

    // #[inline]
    // pub fn stamps<'lt>(
    //     &'lt self,
    //     branch: &'lt Current,
    // ) -> impl Iterator<Item = (&Stamp, f64)> + 'lt + ExactSizeIterator {
    //     branch
    //         .stamps
    //         .iter()
    //         .zip(&self.0[branch.pos+1..])
    //         .map(|(stamp, val)| (stamp, *val))
    // }

    #[inline]
    pub fn non_linearity_sensitivities<'lt>(
        &'lt self,
        branch: &'lt Current,
    ) -> impl Iterator<Item = (&LimitId, f64)> + 'lt + ExactSizeIterator {
        // Non linearaties that affact a branch have by definition a non zero partial derivative
        // OSDI stores non linearaties first for performant iteration
        branch
            .non_linearties
            .iter()
            .zip(&self.0[branch.pos + 1..])
            .map(|(stamp, val)| (stamp, *val))
    }
}

impl Deref for BranchCurrentsAndConductance {
    type Target = [f64];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BranchCurrentsAndConductance {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
