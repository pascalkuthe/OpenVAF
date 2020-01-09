use crate::{SourceMap, Span};
//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************
use crate::parser::lexer::Token;
use crate::parser::preprocessor::ArgumentIndex;

pub type Error = crate::error::Error<Type>;
pub(crate) type Warning = crate::error::Error<WarningType>;
pub type Result<T = ()> = std::result::Result<T, Error>;
pub type MultiResult<T = ()> = std::result::Result<T, MultiError>;
pub struct MultiError(pub Vec<Error>);

impl MultiError {
    pub fn merge(&mut self, mut other: Self) {
        self.0.append(&mut other.0)
    }
    pub fn add(&mut self, err: Error) {
        self.0.push(err)
    }
}
impl From<Error> for MultiError {
    fn from(err: Error) -> Self {
        MultiError(vec![err])
    }
}

#[derive(Debug)]
pub enum Type {
    ConditionEndWithoutStart,
    MacroEndTooEarly,
    UnclosedConditions(Vec<Span>),
    UnexpectedEof {
        expected: Vec<Token>,
    },
    EmptyListEntry(List),
    MacroArgumentCount {
        expected: ArgumentIndex,
        found: ArgumentIndex,
    },
    MacroNotFound,
    CompilerDirectiveSplit,
    //TODO recommendation
    IoErr(std::io::Error),
    UnexpectedToken {
        expected: Vec<Token>,
    },
    MacroRecursion,
}
impl From<std::io::Error> for Type {
    fn from(io_err: std::io::Error) -> Self {
        Self::IoErr(io_err)
    }
}
#[derive(Clone, Debug)]
pub(crate) enum WarningType {
    MacroOverwritten(Span),
}

#[derive(Debug)]
pub enum List {
    MacroArgument,
    FunctionArgument,
    Identifier,
}
impl Type {
    pub fn print<'source>(error: &'source Error, source_map: &'source SourceMap) -> String {
        let res = source_map.resolve_span(error.source);
        match error.error_type {
            Type::UnexpectedToken { ref expected } => format!(
                "Unexpected Token {}; Expected {:?} at {}",
                res.0, expected, res.1
            ),
            _ => unimplemented!("{}:{:?} at {}", res.0, error.error_type, res.1),
        }
    }
}
