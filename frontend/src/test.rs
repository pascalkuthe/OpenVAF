/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::diagnostic::UserResult;
use crate::parser::tokenstream::TokenStream;
use crate::preprocessor::{preprocess_user_facing, std_path};
use crate::sourcemap::FileId;
use crate::SourceMap;
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

pub const TEST_EXPANSION_HINT: &'static str =
    "This error occured inside a compiler directive. If you have trouble understanding why/where this error occurs use the 'ExpansionPrinter'";

pub struct PrettyError(Box<dyn Display>);

impl<I: Display + 'static> From<I> for PrettyError {
    fn from(val: I) -> Self {
        Self(Box::new(val))
    }
}

impl Debug for PrettyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub fn preprocess_test(
    sm: SourceMap,
    main_file: FileId,
) -> UserResult<(TokenStream, Arc<SourceMap>)> {
    preprocess_user_facing(
        sm,
        TEST_EXPANSION_HINT,
        main_file,
        std_path(
            "tests/std/constants.vams".into(),
            "tests/std/disciplines.vams".into(),
        ),
    )
}
