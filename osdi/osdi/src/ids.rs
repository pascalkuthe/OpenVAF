//  * ******************************************************************************************
//  *  Copyright (c) 2020 Pascal Kuthe. This file is part of the OSDI project.
//  *  It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/osdi/blob/master/LICENSE.
//  *  No part of OpenVAF-Types, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************


#[macro_export]
macro_rules! id_type {
    ($(#[$attr:meta])* $name:ident($type:ident)) => {
        // see the index_vec documentation
        ::index_vec::define_index_type! {
            $(#[$attr])*
            pub struct $name = $type;

            DISPLAY_FORMAT = "{}";

            DEBUG_FORMAT = stringify!(<$name {}>);

            IMPL_RAW_CONVERSIONS = true;
        }

        impl ::serde::Serialize for $name{
            fn serialize<S>(&self, serializer: S) -> Result<<S as ::serde::Serializer>::Ok, <S as ::serde::Serializer>::Error> where
                S: ::serde::Serializer {
                self.index().serialize(serializer)
            }
        }

        impl<'de> ::serde::Deserialize<'de> for $name{
            fn deserialize<D>(deserializer: D) -> Result<Self, <D as ::serde::Deserializer<'de>>::Error> where
                D: ::serde::Deserializer<'de> {
                Ok(Self::from_raw_unchecked($type::deserialize(deserializer)?))
            }
        }
    };
}
