/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use cfg_if::cfg_if;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::ffi::{CStr, CString};
use std::ops::Deref;
use std::os::raw::c_char;

cfg_if! {
    if #[cfg(all(feature="simulator",feature="cstr"))]{

        #[derive(Clone, Copy, Debug)]
        pub struct OsdiStr(&'static CStr);

        impl OsdiStr {
            pub fn new(raw: String)->Self{
                Self::from_cstr(CString::new(raw).unwrap())
            }

            pub fn from_cstr(raw: CString)->Self{
                Self(Box::leak(raw.into_boxed_c_str()))
            }


            /// # Safety
            /// This function produces undefined behaviour if the contents of the returned string ptr is ever mutated
            /// While derefencing a ptr is already unsafe in rust care must be taiken when passsing this ptr to c that its contents must not be modified
            pub unsafe fn cstr(&self)->*mut c_char{
                self.0.as_ptr() as *mut c_char
            }

            pub fn contents(&self)->&'static str{
                self.0.to_str().unwrap()
            }
        }

        impl Serialize for OsdiStr {
            fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> where
                S: Serializer {
                // Don't serialize the null termination
                self.contents().serialize(serializer)
            }
        }
        impl<'de> Deserialize<'de> for OsdiStr {
            fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error> where
                D: Deserializer<'de> {
                Ok(Self::new(String::deserialize(deserializer)?))
            }
        }

    }else if #[cfg(feature="simulator")]{
        #[derive(Clone, Copy, Debug, Serialize)]
        pub struct OsdiStr(pub &'static str);

       impl OsdiStr{
            pub fn contents(&self)->&'static str{
                self.0
            }
       }
        impl<'de> Deserialize<'de> for OsdiStr {
            fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error> where
                D: Deserializer<'de> {
                Ok(Self(Box::leak(String::deserialize(deserializer)?.into_boxed_str())))
            }
        }



    }else{
        pub type OsdiStr = String;
    }
}
