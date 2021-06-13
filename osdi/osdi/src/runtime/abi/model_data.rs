/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::model_info_store::{OpaqueData, ParameterId};

use crate::ModelInfoStore;

use crate::runtime::abi::OSDIAbi;
use std::mem::size_of;

type BLOCK = u64;
const PARAM_GIVEN_BLOCK_SIZE_U16: u16 = 64;
const PARAM_GIVEN_BLOCK_SIZE_USIZE: usize = PARAM_GIVEN_BLOCK_SIZE_U16 as usize;

const fn mask(param: u16) -> BLOCK {
    1 << (param % PARAM_GIVEN_BLOCK_SIZE_U16)
}

pub struct ModelData(OpaqueData);

impl ModelData {
    pub unsafe fn new(info_store: &ModelInfoStore) -> Self {
        // Rust allocator always produces valid allocations and the same layout is used so this is perfectly save
        let data = info_store.model_data.alloc();
        Self(data)
    }

    pub unsafe fn from_data(raw: OpaqueData) -> Self {
        Self(raw)
    }

    pub fn is_parameter_set(&self, param: ParameterId, info_store: &ModelInfoStore) -> bool {
        let block: BLOCK = unsafe {
            self.read(
                info_store.param_given_offset
                    + (param.index() / PARAM_GIVEN_BLOCK_SIZE_USIZE) * size_of::<BLOCK>(),
            )
        };
        block & mask(param.raw()) != 0
    }

    unsafe fn read<T: Copy>(&self, offset: usize) -> T {
        (self.0.access().as_ptr().add(offset) as *mut T).read()
    }

    pub fn mark_parameter_as_set(
        &self,
        param: ParameterId,
        info_store: &ModelInfoStore,
        set: bool,
    ) {
        let block: &mut BLOCK = unsafe {
            self.get_mut(
                info_store.param_given_offset
                    + (param.index() / PARAM_GIVEN_BLOCK_SIZE_USIZE) * size_of::<BLOCK>(),
            )
        };
        if set {
            *block |= mask(param.raw());
        } else {
            *block &= !mask(param.raw());
        }
    }

    unsafe fn get_mut<T>(&self, offset: usize) -> &mut T {
        &mut *(self.0.access().as_ptr().add(offset) as *mut T)
    }

    /// Directly writes data into the modelcard
    /// **WARNING** DOES NOT SET THE PARAMETER AS GIVEN (use set_unchecked instead)
    ///
    /// # Safety
    /// param must belong to the same layout that this ModelCard was created with
    /// and the type of T must match that of the parameter
    #[inline]
    pub unsafe fn write_unchecked<T: OSDIAbi>(
        &mut self,
        param: ParameterId,
        info_store: &ModelInfoStore,
        val: T,
    ) {
        val.write(
            self.0
                .access()
                .as_ptr()
                .add(info_store.parameters[param].model_data_offset),
        )
    }

    /// Sets a parameter and marks its values as given
    /// Callers should check that the correct data type (parameter.ty ==
    ///
    /// # Safety
    /// param must belong to the same layout that this ModelCard was created with
    /// and the type of T must match that of the parameter

    #[inline]
    pub unsafe fn set_unchecked<T: OSDIAbi>(
        &mut self,
        param: ParameterId,
        info_store: &ModelInfoStore,
        val: T,
    ) {
        self.mark_parameter_as_set(param, info_store, true);
        val.write(
            self.0
                .access()
                .as_ptr()
                .add(info_store.parameters[param].model_data_offset),
        )
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
