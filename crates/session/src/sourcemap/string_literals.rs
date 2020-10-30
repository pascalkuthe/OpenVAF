/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::sourcemap::SourceMap;
use crate::with_sourcemap;
use openvaf_data_structures::index_vec::define_index_type;

define_index_type! {
            pub struct StringLiteral = u16;

            DISPLAY_FORMAT = "{}";

            DEBUG_FORMAT = "<StringLiteral {}>";

            IMPL_RAW_CONVERSIONS = true;

            // Checks are done when literals are added
            DISABLE_MAX_INDEX_CHECK = ! cfg!(debug_assertions);
}

impl StringLiteral {
    pub const DUMMY: Self = Self::from_raw_unchecked(0);

    #[must_use]
    pub fn raw_contents(self, sm: &SourceMap) -> &str {
        sm.literals[self].src(sm)
    }

    #[must_use]
    pub fn with_raw_contents<T>(self, f: impl FnOnce(&str) -> T) -> T {
        with_sourcemap(|sm| f(self.raw_contents(sm)))
    }

    /// Returns the contents of the `StringLiteral` with escape character such as "\\" replaced by the correspoding character such as "\"
    /// Expansive call considerately
    #[must_use]
    pub fn unescaped_contents(self) -> String {
        with_sourcemap(|sm| unesacpe_string(self.raw_contents(sm)))
    }

    pub fn global_count() -> usize {
        with_sourcemap(|sm| sm.literals.len())
    }
}

pub fn unesacpe_string(raw: &str) -> String {
    raw.replace(r"\n", "\n")
        .replace(r"\\", "\\")
        .replace(r"\t", "\t")
        .replace(r#"\""#, "\"")
}

#[cfg(feature = "serde_dump")]
use serde::{Serialize, Serializer};

#[cfg(feature = "serde_dump")]
impl Serialize for StringLiteral {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.unescaped_contents())
    }
}
