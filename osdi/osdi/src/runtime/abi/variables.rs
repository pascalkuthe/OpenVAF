//  * ******************************************************************************************
//  *  Copyright (c) 2020 Pascal Kuthe. This file is part of the OSDI project.
//  *  It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/osdi/blob/master/LICENSE.
//  *  No part of OpenVAF-Types, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::ModelInfoStore;
use std::alloc::{alloc, dealloc, Layout};
use std::ptr::NonNull;

pub struct InternalData {
    data: NonNull<u8>,
    layout: Layout,
}

impl InternalData {
    pub fn new(layout: Layout) -> Self {
        let data = unsafe { alloc(layout) };
        Self {
            data: NonNull::new(data).expect("Allocation failed"),
            layout,
        }
    }

    pub fn new_instance_variable(info_store: &ModelInfoStore) -> Self {
        Self::new(info_store.instance_variable_layout.0)
    }

    pub fn new_model_variable(info_store: &ModelInfoStore) -> Self {
        Self::new(info_store.model_variable_layout.0)
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.data.as_ptr()
    }

    pub fn as_ptr_mut(&mut self) -> *mut u8 {
        self.data.as_ptr()
    }
}
impl Drop for InternalData {
    fn drop(&mut self) {
        unsafe { dealloc(self.data.as_ptr(), self.layout) }
    }
}
