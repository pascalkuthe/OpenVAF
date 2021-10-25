/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use std::io;

use crate::{lexer::RawToken, sourcemap::CtxSpan};
use derive_more::Display;
use vfs::VfsPath;

#[derive(Debug, PartialEq, Display, Clone, Eq, Hash)]
pub enum PreprocessorDiagnostic {
    #[display(fmt = "argument mismatch expected {} but found {}!", "expected", "found")]
    MacroArgumentCountMissmatch { expected: usize, found: usize, span: CtxSpan },

    #[display(fmt = "macro '`{}' has not been declared", "name")]
    MacroNotFound { name: String, span: CtxSpan },

    #[display(fmt = "macro '`{}' was called recursively", "name")]
    MacroRecursion { name: String, span: CtxSpan },

    #[display(fmt = "failed to read '{}': {}", "file", "std::io::Error::from(*error)")]
    IoError { file: VfsPath, error: io::ErrorKind, span: Option<CtxSpan> },

    #[display(fmt = "failed to read '{}': file not found", "file")]
    FileNotFound { span: Option<CtxSpan>, file: String },

    #[display(fmt = "failed to read {}: file contents are not valid text", "file")]
    InvalidTextFormat { span: Option<CtxSpan>, file: VfsPath },

    //General
    #[display(fmt = "unexpected EOF, expected {}", "expected")]
    UnexpectedEof { expected: RawToken, span: CtxSpan },

    #[display(fmt = "unexpected token, expected '{}'", "expected")]
    MissingOrUnexpectedToken { expected: RawToken, expected_at: CtxSpan, span: CtxSpan },

    #[display(fmt = "encountered unexpected token!")]
    UnexpectedToken(CtxSpan),

    #[display(fmt = "macro '`{}' was overwritten", "name")]
    MacroOverwritten { old: CtxSpan, new: CtxSpan, name: String },
}
