/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::convert::Into;
use std::fmt::{Debug, Display, Formatter};
use std::path::{Path, PathBuf};

use openvaf_parser::TokenStream;
use openvaf_preprocessor::{preprocess_user_facing, std_path};
use openvaf_session::openvaf_session;
use openvaf_session::sourcemap::SourceMap;

#[cfg(test)]
mod parser;

#[cfg(test)]
mod preprocessor;

#[cfg(test)]
mod extractions;

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

pub fn preprocess_test(file: impl Into<PathBuf>) -> Result<TokenStream, PrettyError> {
    let (sm, main_file) = SourceMap::new_with_mainfile(file)?;
    let res = preprocess_user_facing(
        sm,
        main_file,
        std_path(
            Path::new("std").join("constants.vams"),
            Path::new("std").join("disciplines.vams"),
        ),
    )?;
    Ok(res)
}

#[allow(unused_must_use)]
pub fn test_session<T>(
    file: impl Into<PathBuf>,
    test: impl FnOnce(TokenStream) -> Result<T, PrettyError>,
) -> Result<T, PrettyError> {
    fern::Dispatch::new()
        .format(|out, message, _| out.finish(*message))
        .level(log::LevelFilter::Info)
        .chain(std::io::stderr())
        .apply();

    openvaf_session(|| test(preprocess_test(file)?))
}
