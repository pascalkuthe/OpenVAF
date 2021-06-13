/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

//pub mod abi;
//pub mod registry;
use std::path::Path;

use libloading::{Library, Symbol};

use crate::ModelInfoStore;

pub mod abi;

/// Definitions of osdi model libary functions
/// For more detailed description read the OSDI standard!
pub mod model_lib_fn {

    use osdi_types::Complex64;

    // fn model_init(model_data: &mut ModelData)
    pub type ModelInit = unsafe extern "C" fn(*mut u8);

    // fn model_temp_update(model_data: &mut ModelData, temp: f64)
    pub type ModelTempUpdate = unsafe extern "C" fn(*mut u8, f64);

    // fn instance_init(model_data: &ModelData, instance_data: &mut InstanceData)
    pub type InstanceInit = unsafe extern "C" fn(*mut u8, *mut u8);

    // fn instance_temp_update(model_data: &ModelData, instance_data: &mut InstanceData, temp: f64)
    pub type InstanceTempUpdate = unsafe extern "C" fn(*mut u8, *mut u8, f64);

    pub type DcLoadFunction = unsafe extern "C" fn(
        *mut *mut f64, // Jacobian ptrs
        *mut *mut u64, // state vectors
        *mut f64,      // voltages start
        *mut usize,    // voltage positions
        *mut f64,      // model_data
        *mut f64,      // instance_data
        f64,           // temp
    );

    pub type AcLoadFunction = unsafe extern "C" fn(
        *mut *mut Complex64, // Jacobian ptrs
        *mut *mut u64,       // state vectors
        *mut f64,            // voltages start
        *mut usize,          // voltage positions
        *mut f64,            // model_data
        *mut f64,            // instance_data
        f64,                 // temp
    );
}

pub struct OsdiModel {
    pub model_init: Symbol<'static, model_lib_fn::ModelInit>,
    pub model_temp_update: Symbol<'static, model_lib_fn::ModelTempUpdate>,
    pub instance_init: Symbol<'static, model_lib_fn::InstanceInit>,
    pub instance_temp_update: Symbol<'static, model_lib_fn::InstanceTempUpdate>,
    pub dc_load: Symbol<'static, model_lib_fn::DcLoadFunction>,
    pub ac_load: Symbol<'static, model_lib_fn::AcLoadFunction>,
    pub info_store: ModelInfoStore,
}

impl OsdiModel {
    pub fn init(path: impl AsRef<Path>) -> Result<Self, libloading::Error> {
        let lib = Library::new(path.as_ref())?;
        let lib: &'static Library = Box::leak(Box::new(lib));
        let model_init = unsafe { lib.get(b"model_init\0")? };
        let model_temp_update = unsafe { lib.get(b"model_temp_update\0")? };

        let instance_init = unsafe { lib.get(b"instance_init\0")? };
        let instance_temp_update = unsafe { lib.get(b"instance_temp_update\0")? };
        let dc_load = unsafe { lib.get(b"dc_load\0")? };
        let ac_load = unsafe { lib.get(b"dc_load\0")? };

        let info_store: Symbol<'static, &'static [u8]> = unsafe { lib.get(b"info_store\0")? };

        Ok(Self {
            model_init,
            model_temp_update,
            instance_init,
            instance_temp_update,
            dc_load,
            ac_load,
            info_store: ModelInfoStore::load(&*info_store).unwrap(), // TODO proper errror handeling
        })
    }
}
