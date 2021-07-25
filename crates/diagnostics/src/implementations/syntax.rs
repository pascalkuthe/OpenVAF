/*
 *  ************************ id: (), label: (), annotation_type: ()  label: (), annotation_type: ()  id: (), label: (), annotation_type: () ******************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use std::fmt::format;

use crate::{Diagnostic, DiagnosticSlice, FooterItem, Text};
use annotate_snippets::snippet::AnnotationType;
use basedb::{BaseDB, FileId};
use syntax::SyntaxError;

impl Diagnostic for SyntaxError {
    fn annotation_type(&self, _db: &dyn BaseDB, _root_file: FileId) -> Option<AnnotationType> {
        Some(AnnotationType::Error)
    }

    fn slices(&self, db: &dyn BaseDB, root_file: FileId) -> Vec<DiagnosticSlice> {
        let sm = &db.sourcemap(root_file);
        match self {
            SyntaxError::UnexpectedToken { expected, span, expected_at, .. } => {
                let span = db.parse(root_file).to_span(*span, sm);

                let main_message = if let Some(expected_at) = expected_at {
                    let expected_at = db.parse(root_file).to_span(*expected_at, sm);
                    DiagnosticSlice {
                        slice_span: expected_at.extend(span, sm),
                        messages: vec![
                            (
                                AnnotationType::Info,
                                Text::owned(format!("expected {}", expected)),
                                expected_at,
                            ),
                            (AnnotationType::Error, Text::const_str("unexpected token"), span),
                        ],
                        fold: true,
                    }
                } else {
                    let msg = if expected.data.len() < 4 {
                        Text::owned(format!("expected {}", expected))
                    } else {
                        Text::const_str("unexpected token")
                    };
                    DiagnosticSlice {
                        slice_span: span,
                        messages: vec![(AnnotationType::Error, msg, span)],
                        fold: false,
                    }
                };

                vec![main_message]
            }
        }
    }

    fn footer(&self, _db: &dyn BaseDB, _root_file: FileId) -> Vec<crate::FooterItem> {
        match self {
            SyntaxError::UnexpectedToken { missing_delimeter: true, .. } => {
                vec![FooterItem {
                    id: None,
                    annotation_type: AnnotationType::Help,
                    label: Text::const_str("you might be missing a 'begin' delimeter"),
                }]
            },
            _ => vec![],
        }
    }
}
