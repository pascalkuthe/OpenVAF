//  * ******************************************************************************************
//  *  Copyright (c) 2020 Pascal Kuthe. This file is part of the OSDI project.
//  *  It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/osdi/blob/master/LICENSE.
//  *  No part of OpenVAF-Types, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::model_info_store::{Parameter, ParameterId};

use super::OSDIAbi;
use crate::ModelInfoStore;
use osdi_types::Type;

use std::alloc::{alloc, dealloc, Layout};
use std::ptr::NonNull;
use thiserror::Error;

type BLOCK = u64;
const PARAM_GIVEN_BLOCK_SIZE_U16: u16 = 64;
const PARAM_GIVEN_BLOCK_SIZE_USIZE: usize = PARAM_GIVEN_BLOCK_SIZE_U16 as usize;

const fn mask(param: u16) -> BLOCK {
    1 << (param % PARAM_GIVEN_BLOCK_SIZE_U16)
}

pub struct ModelCard {
    data: NonNull<u8>,
    data_layout: Layout,
    given: Box<[u64]>,
}

impl ModelCard {
    pub fn new(param_count: usize, data_layout: Layout) -> Self {
        // Upwards rounding integer division
        let given_size =
            (param_count + PARAM_GIVEN_BLOCK_SIZE_USIZE - 1) / PARAM_GIVEN_BLOCK_SIZE_USIZE;

        // Rust allocator always procudes valid allocations u8 is always properly aligned
        let data = unsafe { alloc(data_layout) };

        Self {
            data: NonNull::new(data).expect("Allocation failed"),
            data_layout,
            given: vec![0; given_size].into_boxed_slice(),
        }
    }

    pub fn from_info_store(info_store: &ModelInfoStore) -> Self {
        Self::new(info_store.parameters.len(), info_store.modelcard_layout.0)
    }

    pub fn data(&self) -> *const u8 {
        self.data.as_ptr()
    }

    pub fn data_mut(&mut self) -> *mut u8 {
        self.data.as_ptr()
    }

    pub fn paras_given(&self) -> &[u64] {
        &self.given
    }

    pub fn is_parameter_set(&self, param: ParameterId) -> bool {
        (self.given[param.index() / PARAM_GIVEN_BLOCK_SIZE_USIZE] & mask(param.raw())) != 0
    }

    /// Directly writes data into the modelcard
    /// **WARNING** DOES NOT SET THE PARAMETER AS GIVEN (use set_unchecked instead)
    ///
    /// # Safety
    /// param must belong to the same layout that this ModelCard was created with
    /// and the type of T must match that of the parameter
    pub unsafe fn write_unchecked<T: OSDIAbi>(&mut self, param: &Parameter, val: T) {
        val.write(self.data.as_ptr().add(param.mc_offset))
    }

    /// Sets a parameter and marks its values as given
    /// Callers should check that the correct data type (parameter.ty ==
    ///
    /// # Safety
    /// param must belong to the same layout that this ModelCard was created with
    /// and the type of T must match that of the parameter
    pub unsafe fn set_unchecked<T: OSDIAbi>(&mut self, param: &Parameter, val: T) {
        self.given[param.id.index() / PARAM_GIVEN_BLOCK_SIZE_USIZE] |= mask(param.id.raw());
        self.write_unchecked(param, val)
    }

    // /// # Safety
    // /// param must belong to the same layout that this ModelCard was created with
    // pub unsafe fn set<T: OSDIAbi>(&mut self, param: &Parameter, val: T)->Result<(),ModelCardWriteError>{
    //     let found = T::osdi_type();
    //     if param.info.ty != found{
    //         Err(ModelCardWriteError::TypeMissmatch{
    //             name: param.info.name.contents(),
    //             expected: param.info.ty,
    //             found,
    //         })
    //     }else {
    //         self.set_unchecked(param, val);
    //
    //         Ok(())
    //     }
    // }
    //
    // /// # Safety
    // /// param must belong to the same layout that this ModelCard was created with
    // pub unsafe fn insert<T: OSDIAbi>(&mut self, param: &Parameter, val: T)->Result<(),ModelCardWriteError>{
    //     let found = T::osdi_type();
    //     if param.info.ty != found{
    //         Err(ModelCardWriteError::TypeMissmatch{
    //             name: &param.info.name.contents(),
    //             expected: param.info.ty,
    //             found,
    //         })
    //     }else if self.is_parameter_set(param.id) {
    //         Err(ModelCardWriteError::AlreadySet(param.info.name.contents()))
    //     }else {
    //         self.set_unchecked(param, val);
    //         Ok(())
    //     }
    // }
}

impl Drop for ModelCard {
    fn drop(&mut self) {
        unsafe { dealloc(self.data.as_ptr(), self.data_layout) }
    }
}
