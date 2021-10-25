
/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::{
    lints::{Lint, LintData, LintLevel, SyntaxCtx},
    BaseDB, FileId,
};

pub use sink::{print_all, ConsoleSink, DiagnosticSink};

mod preprocessor_error;
mod sink;
mod syntax_error;

// pub use ariadne::{Label, ReportKind};
// pub type ReportBuilder = ariadne::ReportBuilder<FileSpan>;
// pub type Report = ariadne::Report<FileSpan>;

pub type Report = codespan_reporting::diagnostic::Diagnostic<FileId>;
pub type Label = codespan_reporting::diagnostic::Label<FileId>;

pub use codespan_reporting::{
    diagnostic::{LabelStyle, Severity},
    term::Config,
};
use syntax::{
    sourcemap::{CtxSpan, FileSpan, SourceMap},
    Parse, SourceFile, TextRange,
};

pub trait Diagnostic {
    fn lint(&self) -> Option<(Lint, Option<SyntaxCtx>)> {
        None
    }

    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report;

    fn to_report(&self, root_file: FileId, db: &dyn BaseDB) -> Option<Report> {
        if let Some((lint, sctx)) = self.lint() {
            let (lvl, is_default) = db.lint_lvl(lint, root_file, sctx);
            let LintData { name, documentation_id, .. } = db.lint_data(lint);

            let seververity = match lvl {
                LintLevel::Deny | LintLevel::Forbid => Severity::Error,
                LintLevel::Warn => Severity::Warning,
                LintLevel::Allow => return None,
            };

            let mut report = self.build_report(root_file, db);

            if is_default {
                let hint = format!(
"the {} lint is set to {} by default.
This can be changed with an attribute such as (*openvaf_allow={}*) or with command line arguments (see --help)",
            name, lvl,name);
                report.notes.push(hint)
            }

            report.severity = seververity;
            Some(report.with_code(format!("L{:03}", documentation_id)))
        } else {
            Some(self.build_report(root_file, db))
        }
    }
}

pub const HINT_UNSUPPORTED: &str = "this is allowed by VerilogAMS language spec but was purposefully excluded from the supported language subset\nmore details can be found in the OpenVAF documentation";

// TODO support expansion backtrace

pub fn to_unified_spans<const N: usize>(
    sm: &SourceMap,
    mut spans: [CtxSpan; N],
) -> (FileId, [TextRange; N]) {
    assert!(N >= 2);
    let (file, ranges) = sm.to_file_spans(&mut spans);
    (file, ranges.try_into().unwrap())
}

pub fn to_unified_span_list(sm: &SourceMap, spans: &mut [CtxSpan]) -> (FileId, Vec<TextRange>) {
    match spans {
        [] => unimplemented!(),
        [span] => {
            let FileSpan { range, file } = span.to_file_span(sm);
            (file, vec![range])
        }
        mut spans => sm.to_file_spans(&mut spans),
    }
}

pub fn text_ranges_to_unified_spans<const N: usize>(
    sm: &SourceMap,
    parse: &Parse<SourceFile>,
    ranges: [TextRange; N],
) -> (FileId, [TextRange; N]) {
    let spans = ranges.map(|range| parse.to_ctx_span(range, sm));
    to_unified_spans(sm, spans)
}

pub fn text_range_list_to_unified_spans(
    sm: &SourceMap,
    parse: &Parse<SourceFile>,
    ranges: &[TextRange],
) -> (FileId, Vec<TextRange>) {
    let mut spans: Vec<_> = ranges.iter().map(|range| parse.to_ctx_span(*range, sm)).collect();
    to_unified_span_list(sm, &mut spans)
}

pub fn assert_empty_diagnostics(
    db: &dyn BaseDB,
    root_file: FileId,
    diagnostics: &[impl Diagnostic],
) {
    if !diagnostics.is_empty() {
        print_all(diagnostics, db, root_file, Config::default());
        panic!("found {} unexpected diagnostics", diagnostics.len())
    }
}
