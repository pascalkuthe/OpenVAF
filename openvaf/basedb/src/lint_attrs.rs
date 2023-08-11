use std::sync::Arc;

use ahash::AHashMap;
pub use diagnostics::AttrDiagnostic;
use syntax::ast::{self, AstToken, AttrIter, LiteralKind};
use syntax::{AstNode, TextRange};
use vfs::FileId;

use crate::lints::{Lint, LintLevel, LintRegistry, LintSrc};
use crate::{AstIdMap, BaseDB, ErasedAstId};

mod diagnostics;

// #[cfg(test)]
// mod tests;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LintAttrTree {
    overwrites: AHashMap<(ErasedAstId, Lint), LintLevel>,
    pub diagnostics: Vec<AttrDiagnostic>,
}

impl LintAttrTree {
    pub fn lint_attr_tree_query(db: &dyn BaseDB, root_file: FileId) -> Arc<LintAttrTree> {
        let map = db.ast_id_map(root_file);
        let cst = db.parse(root_file).tree();
        let cst = cst.syntax();
        let registry = db.lint_registry();

        let mut res = LintAttrTree { overwrites: AHashMap::new(), diagnostics: Vec::new() };

        for (id, entry) in map.entries() {
            // quick reject to avoid looking at the ast when not necessary
            let has_attr = entry
                .attrs
                .iter()
                .any(|attr| matches!(&**attr, "openvaf_allow" | "openvaf_warn" | "openvaf_deny"));
            if has_attr {
                let cst = entry.syntax.to_node(cst);
                if ast::Var::can_cast(cst.kind()) || ast::Param::can_cast(cst.kind()) {
                    continue;
                }
                let overwrites =
                    resolve_overwrites(&registry, ast::attrs(&cst), &mut res.diagnostics, id);
                res.overwrites.extend(overwrites.map(|(lint, lvl)| ((id, lint), lvl)));
            }
        }

        Arc::new(res)
    }

    pub fn lint_lvl(&self, map: &AstIdMap, mut id: ErasedAstId, lint: Lint) -> Option<LintLevel> {
        loop {
            if let Some(lvl) = self.overwrites.get(&(id, lint)) {
                return Some(*lvl);
            }
            id = map.get_parent(id)?;
        }
    }
}

pub fn resolve_overwrites(
    registry: &LintRegistry,
    attrs: AttrIter,
    err: &mut Vec<AttrDiagnostic>,
    src: ErasedAstId,
) -> impl Iterator<Item = (Lint, LintLevel)> {
    fn insert_lint(
        lit: ast::Literal,
        err: &mut Vec<AttrDiagnostic>,
        registry: &LintRegistry,
        overwrites: &mut AHashMap<Lint, (LintLevel, TextRange)>,
        lvl: LintLevel,
        src: ErasedAstId,
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
                        err.push(AttrDiagnostic::UnknownLint { range, lint: lint_name, src });
                    }
                    return;
                };
                if let Some((_, old)) = overwrites.insert(lint, (lvl, range)) {
                    err.push(AttrDiagnostic::LintOverwrite {
                        old,
                        new: range,
                        name: lint_name,
                        src,
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
                insert_lint(lit, err, registry, &mut overwrites, lvl, src)
            }

            Some(ast::Expr::ArrayExpr(e)) => {
                for expr in e.exprs() {
                    if let ast::Expr::Literal(lit) = expr {
                        insert_lint(lit, err, registry, &mut overwrites, lvl, src)
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
    overwrites.into_iter().map(|(lint, (lvl, _))| (lint, lvl))
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LintAttrs {
    overwrites: AHashMap<Lint, LintLevel>,
    parent: ErasedAstId,
}

impl LintAttrs {
    pub fn empty(parent: ErasedAstId) -> LintAttrs {
        LintAttrs { parent, overwrites: AHashMap::new() }
    }

    pub fn resolve(
        registry: &LintRegistry,
        attrs: AttrIter,
        err: &mut Vec<AttrDiagnostic>,
        parent: ErasedAstId,
    ) -> LintAttrs {
        let overwrites = resolve_overwrites(registry, attrs, err, parent);
        LintAttrs { parent, overwrites: overwrites.collect() }
    }

    pub fn lint_src(&self, lint: Lint) -> LintSrc {
        LintSrc { overwrite: self.level(lint), ast: Some(self.parent) }
    }

    pub fn level(&self, lint: Lint) -> Option<LintLevel> {
        self.overwrites.get(&lint).copied()
    }
}
