use ahash::AHashMap;
use basedb::lints::{ErasedItemTreeId, Lint, LintLevel, LintRegistry, LintSrc};

use syntax::{
    ast::{self, AstToken, AttrIter, LiteralKind},
    AstNode, TextRange,
};

mod diagnostics;
pub use diagnostics::AttrDiagnostic;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LintAttrs {
    overwrites: AHashMap<Lint, LintLevel>,
    parent: ErasedItemTreeId,
}

impl LintAttrs {
    pub fn empty(parent: Option<ErasedItemTreeId>) -> LintAttrs {
        LintAttrs { parent: parent.unwrap_or(ErasedItemTreeId::ROOT), overwrites: AHashMap::new() }
    }

    // pub fn is_empty(&self) -> bool {
    //     self.overwrites.is_empty()
    // }

    pub fn resolve(
        registry: &LintRegistry,
        parent: Option<ErasedItemTreeId>,
        attrs: AttrIter,
        err: &mut Vec<AttrDiagnostic>,
    ) -> LintAttrs {
        let parent = parent.unwrap_or(ErasedItemTreeId::ROOT);
        fn insert_lint(
            lit: ast::Literal,
            err: &mut Vec<AttrDiagnostic>,
            registry: &LintRegistry,
            overwrites: &mut AHashMap<Lint, (LintLevel, TextRange)>,
            lvl: LintLevel,
            parent: ErasedItemTreeId,
        ) {
            match lit.kind() {
                LiteralKind::String(lit) => {
                    let lint_name = lit.unescaped_value();
                    let range = lit.syntax().text_range();
                    let lint = if let Some(lint) = registry.lint_from_name(&lint_name) {
                        lint
                    } else {
                        if !lint_name.contains("::") {
                            // Plugins use plugin::lint_name. Plugin lints for unused plugins are fine
                            err.push(AttrDiagnostic::UnknownLint {
                                range,
                                lint: lint_name,
                                item_tree: parent,
                            });
                        }
                        return;
                    };
                    if let Some((_, old)) = overwrites.insert(lint, (lvl, range)) {
                        err.push(AttrDiagnostic::LintOverwrite {
                            old,
                            new: range,
                            name: lint_name,
                            item_tree: parent,
                        })
                    }
                }

                _ => err.push(AttrDiagnostic::ExpectedLiteral {
                    range: lit.syntax().text_range(),
                    attr: lvl.attr(),
                }),
            }
        }
        let mut overwrites = AHashMap::new();
        for attr in attrs {
            let lvl = match attr.name() {
                Some(name) if name.text() == "openvaf_allow" => LintLevel::Allow,
                Some(name) if name.text() == "openvaf_warn" => LintLevel::Warn,
                Some(name) if name.text() == "openvaf_deny" => LintLevel::Deny,
                _ => continue,
            };

            match attr.val() {
                Some(ast::Expr::Literal(lit)) if matches!(lit.kind(), LiteralKind::String(_)) => {
                    insert_lint(lit, err, registry, &mut overwrites, lvl, parent)
                }

                Some(ast::Expr::ArrayExpr(e)) => {
                    for expr in e.exprs() {
                        if let ast::Expr::Literal(lit) = expr {
                            insert_lint(lit, err, registry, &mut overwrites, lvl, parent)
                        } else {
                            err.push(AttrDiagnostic::ExpectedLiteral {
                                range: expr.syntax().text_range(),
                                attr: lvl.attr(),
                            });
                        }
                    }
                }
                Some(e) => err.push(AttrDiagnostic::ExpectedArrayOrLiteral {
                    range: e.syntax().text_range(),
                    attr: lvl.attr(),
                }),

                None => err.push(AttrDiagnostic::ExpectedArrayOrLiteral {
                    range: attr.syntax().text_range(),
                    attr: lvl.attr(),
                }),
            }
        }

        LintAttrs {
            parent,
            overwrites: overwrites.into_iter().map(|(lint, (lvl, _))| (lint, lvl)).collect(),
        }
    }

    pub fn lint_src(&self, lint: Lint) -> LintSrc {
        LintSrc { overwrite: self.level(lint), item_tree: self.parent() }
    }

    pub fn level(&self, lint: Lint) -> Option<LintLevel> {
        self.overwrites.get(&lint).copied()
    }

    pub fn parent(&self) -> Option<ErasedItemTreeId> {
        if self.parent == ErasedItemTreeId::ROOT {
            None
        } else {
            Some(self.parent)
        }
    }
}

// pub fn is_openvaf_attr(attr: &str) {
//     matches!(attr, "openvaf_allow" | "openvaf_warn" | "openvaf_deny");
// }
