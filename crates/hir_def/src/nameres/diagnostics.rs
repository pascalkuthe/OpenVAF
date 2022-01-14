use std::ops::Deref;

use basedb::diagnostics::{Diagnostic, Label, LabelStyle, Report};
use basedb::{AstIdMap, BaseDB, FileId};
use stdx::{impl_display, pretty};
use syntax::name::Name;
use syntax::sourcemap::{FileSpan, SourceMap};
use syntax::{Parse, SourceFile};

use crate::db::HirDefDB;

use super::{ResolvedPath, ScopeDefItem};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathResolveError {
    NotFound { name: Name },
    NotFoundIn { name: Name, scope: Name },
    ExpectedScope { name: Name, found: ScopeDefItem },
    ExpectedItemKind { name: Name, expected: &'static str, found: ResolvedPath },
    ExpectedNatureAttributeIdent { found: Box<[Name]> },
}

impl_display! {
    match PathResolveError{
        PathResolveError::NotFound {name} => "'{}' was not found in the current scope", name;
        PathResolveError::NotFoundIn {name, scope} => "'{}' was not found in '{}'", name, scope;
        PathResolveError::ExpectedScope {name, found} => "expected a scope but found {} '{}'", found.item_kind(), name;
        PathResolveError::ExpectedItemKind{name, expected, found} => "expected {} but found {} '{}'", expected, found, name;
        PathResolveError::ExpectedNatureAttributeIdent{found} => "expected a nature attribute identifier found path {}",  pretty::List::path(found.deref());
    }
}

impl PathResolveError {
    pub fn message(&self) -> String {
        match self {
            PathResolveError::NotFound { .. } | PathResolveError::NotFoundIn { .. } => {
                "not found".to_owned()
            }
            PathResolveError::ExpectedScope { .. }
            | PathResolveError::ExpectedNatureAttributeIdent { .. } => {
                "failed to resolve path".to_owned()
            }
            PathResolveError::ExpectedItemKind { expected, .. } => format!("expected {}", expected),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DefDiagnostic {
    AlreadyDeclard { old: ScopeDefItem, new: ScopeDefItem, name: Name },
}

pub struct DefDiagnosticWrapped<'a> {
    pub db: &'a dyn HirDefDB,
    pub diag: &'a DefDiagnostic,
    pub parse: &'a Parse<SourceFile>,
    pub sm: &'a SourceMap,
    pub ast_id_map: &'a AstIdMap,
}

impl Diagnostic for DefDiagnosticWrapped<'_> {
    fn build_report(&self, _root_file: FileId, _db: &dyn BaseDB) -> Report {
        match self.diag {
            DefDiagnostic::AlreadyDeclard { old, new, name } => {
                let FileSpan { range, file } = self.parse.to_file_span(
                    new.text_range(self.db, self.ast_id_map, self.parse).unwrap(),
                    self.sm,
                );

                let mut labels = vec![Label {
                    style: LabelStyle::Primary,
                    file_id: file,
                    range: range.into(),
                    message: "already declared in this scope".to_owned(),
                }];

                if let Some(def) = old.text_range(self.db, self.ast_id_map, self.parse) {
                    let FileSpan { range, file } = self.parse.to_file_span(def, self.sm);
                    labels.push(Label {
                        style: LabelStyle::Secondary,
                        file_id: file,
                        range: range.into(),
                        message: format!("help '{}' was first declared here", name),
                    })
                }
                Report::error()
                    .with_message(format!("'{}' was already declared in this scope", name))
                    .with_labels(labels)
            }
        }
    }
}
