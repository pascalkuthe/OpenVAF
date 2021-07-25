/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::{lexer::RawToken, sourcemap::CtxSpan};
use derive_more::{Display, From};
use vfs::VfsPath;

#[derive(Debug, PartialEq, Display, From, Clone, Eq, Hash)]
pub enum PreprocessorDiagnostic {
    #[display(
        fmt = "macro argument count mismatch expected {} but found {}!",
        "expected",
        "found"
    )]
    MacroArgumentCountMissmatch { expected: usize, found: usize, span: CtxSpan },

    #[display(fmt = "macro '{}' has not been declared", "_0")]
    MacroNotFound(String, CtxSpan),

    #[display(fmt = "macro '{}' was called recursively", "_0")]
    MacroRecursion(String, CtxSpan),

    #[display(fmt = "failed to read '{}': An IO error occured!", "_0")]
    IoErrorRoot(VfsPath), 

    #[display(fmt = "failed to read '{}': File contents are not valid text (UTF-8 or UTF-16)", "_0")]
    InvalidTextFormatRoot(VfsPath), 

    #[display(fmt = "failed to include file: An IO error occured!")]
    IoError(CtxSpan), 

    #[display(fmt = "failed to include File: File contents are not valid text (UTF-8 or UTF-16)")]
    InvalidTextFormat(CtxSpan), 

    //General
    #[display(fmt = "unexpected EOF, expected '{}'", "expected")]
    UnexpectedEof { expected: RawToken, span: CtxSpan },

    #[display(fmt = "unexpected token, expected '{}'", "expected")]
    MissingOrUnexpectedToken { expected: RawToken, expected_at: CtxSpan, span: CtxSpan },

    #[display(fmt = "encountered unexpected token!")]
    UnexpectedToken(CtxSpan),

    #[from]
    MacroOverwritten(MacroOverwritten),

}

#[derive(Debug, Clone, Display, PartialEq,Eq, Hash)]
#[display(fmt = "macro '{}' was overwritten", "name")]
pub struct MacroOverwritten {
    pub old: CtxSpan,
    pub new: CtxSpan,
    pub name: String,
}





