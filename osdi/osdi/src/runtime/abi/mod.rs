//  * ******************************************************************************************
//  *  Copyright (c) 2020 Pascal Kuthe. This file is part of the OSDI project.
//  *  It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/osdi/blob/master/LICENSE.
//  *  No part of OpenVAF-Types, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************


use std::{ ptr};

mod branch_data;
mod modelcard;
mod node_voltages;
mod variables;

pub use branch_data::BranchCurrentsAndConductance;
pub use modelcard::{ModelCard};
pub use node_voltages::NodePotentialOffsets;
pub use variables::InternalData;
use osdi_types::{Type};
use std::convert::TryInto;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::mem::size_of;

/// Allows writing rust data types into select
pub trait OSDIAbi: Sized + Copy{
    fn osdi_type() -> Type;

    unsafe fn write(&self, dst: *mut u8){
        ptr::copy_nonoverlapping(self, dst as *mut Self,1)
    }

}

impl OSDIAbi for f64{
    fn osdi_type() -> Type{
        Type::REAL
    }
}
impl OSDIAbi for i64{
    fn osdi_type() -> Type{
        Type::INT
    }
}


impl<T: OSDIAbi, const N: usize> OSDIAbi for [T;N] {
    fn osdi_type() -> Type{
        T::osdi_type().with_info(|info|{
            let mut info = info.clone();
            info.dimensions.push(N.try_into().expect("OSDI supports at most u32::MAX size arrys"));
            Type::intern(info)
        })
    }


    unsafe fn write(&self, dst: *mut u8){
        for i in 0..N {
            self.get_unchecked(i).write(dst.add(i*size_of::<T>()))
        }
    }

}


impl OSDIAbi for &'static CStr {
    fn osdi_type() -> Type {
        Type::STRING
    }

    unsafe fn write(&self, dst: *mut u8){
        ptr::copy_nonoverlapping(&self.as_ptr(), dst as *mut *const c_char,  1)
    }
}
