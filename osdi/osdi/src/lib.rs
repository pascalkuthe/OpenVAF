//  * ******************************************************************************************
//  *  Copyright (c) 2020 Pascal Kuthe. This file is part of the OSDI project.
//  *  It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/osdi/blob/master/LICENSE.
//  *  No part of OpenVAF-Types, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

pub use string::OsdiStr;

pub use osdi_types as types;

pub mod model_info_store;

mod string;

mod ids;
mod serialization;

pub use crate::model_info_store::ModelInfoStore;
use bitflags::bitflags;
use cfg_if::cfg_if;

bitflags! {
    pub struct ReturnFlags: u64{
        const FINISH_ON_SUCCESS = 0000_0001;
        const ABORT = 0000_0010;
    }
}

bitflags! {
    pub struct LoadFlags: u32{
        const EMPTY = 0000_0000;
        const AC = 0000_0001;
    }
}

cfg_if! {
    if #[cfg(feature="simulator")]{
        pub mod runtime;
        use crate::runtime::{OsdiModelRuntime};

        pub struct OsdiModel{
            pub info_store: ModelInfoStore,
            pub runtime: OsdiModelRuntime,
        }

    }else{

        pub struct OsdiModel{
            pub info_store: ModelInfoStore,
        }

    }
}
