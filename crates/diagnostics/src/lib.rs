/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use std::fmt::Display;

use basedb::{BaseDB, FileId};
use data_structures::Cow;
use syntax::sourcemap::CtxSpan;

pub mod lints;
mod implementations;
mod render;

pub use render::{DiagnosticRender,DiagnosticsRender};

pub use annotate_snippets::snippet::AnnotationType;
pub type Text = Cow<'static, str>;

pub trait Diagnostic: Display + 'static {
    #[inline]
    fn to_render(&self, db: &dyn BaseDB, root_file: FileId) -> Option<DiagnosticRender> {
        self.annotation_type(db, root_file).map(|annotation_type| DiagnosticRender {
            id: self.id(),
            title: self.title(),
            annotation_type,
            slices: self.slices(db, root_file),
            footer: self.footer(db, root_file),
        })
    }

    #[inline(always)]
    fn id(&self) -> Option<&'static str> {
        None
    }

    #[inline]
    fn title(&self) -> Text {
        Text::owned(format!("{}", self))
    }

    fn annotation_type(&self, db: &dyn BaseDB, root_file: FileId) -> Option<AnnotationType>;

    fn slices(&self, db: &dyn BaseDB, root_file: FileId) -> Vec<DiagnosticSlice>;

    #[inline(always)]
    fn footer(&self, _db: &dyn BaseDB, root_file: FileId) -> Vec<FooterItem> {
        Vec::new()
    }
}
#[derive(Debug, Clone)]
pub struct FooterItem {
    pub id: Option<&'static str>,
    pub label: Text,
    pub annotation_type: AnnotationType,
}

#[derive(Clone, Debug)]
pub struct DiagnosticSlice {
    pub slice_span: CtxSpan,
    pub messages: Vec<(AnnotationType, Text, CtxSpan)>,
    pub fold: bool,
}

pub const HINT_UNSUPPORTED: &str = "this is allowed by VerilogAMS language spec but was purposefully excluded from the supported language subset\nmore details can be found in the OpenVAF documentation";
