//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use core::fmt;
use pest::error::ErrorVariant;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
pub mod preprocessor_error;
pub mod syntax_error;
//Some shorthands for common errors
pub type Result<R, T = ()> = std::result::Result<T, pest::error::Error<R>>;
/// A separate result is needed for the preprocessor since it produces a seperate error
pub type PreprocessorResult<T = ()> = std::result::Result<T, preprocessor_error::PreprocessorError>;
pub type SyntaxResult<T = ()> = std::result::Result<T, syntax_error::SyntaxError>;

/// Creates am `Err`(`pest::error::Error<E>`) with custom message highlighting the `Span`  
pub fn error<T, E: Ord + Hash + PartialEq + Copy + Debug>(
    message: &str,
    error_span: pest::Span,
) -> Result<E, T> {
    Err(error_message(message, error_span))
}

pub fn error_message<R: Ord + Hash + PartialEq + Copy + Debug>(
    error_message: &str,
    containing_rule: pest::Span,
) -> pest::error::Error<R> {
    pest::error::Error::new_from_span(
        ErrorVariant::CustomError {
            message: error_message.to_string(),
        },
        containing_rule,
    )
}

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Syntax(syntax_error::SyntaxError),
    Preprocessor(preprocessor_error::PreprocessorError),
    Simple(String),
}
impl From<std::io::Error> for Error {
    fn from(io_err: std::io::Error) -> Self {
        Error::Io(io_err)
    }
}

impl From<String> for Error {
    fn from(message: String) -> Self {
        Self::Simple(message)
    }
}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Simple(ref message) => write!(f, "{}", message),
            Self::Io(e) => write!(f, "{}", e),
            Self::Preprocessor(e) => write!(f, "{}", e),
            Self::Syntax(e) => write!(f, "{}", e),
        }
    }
}
impl std::error::Error for Error {}
