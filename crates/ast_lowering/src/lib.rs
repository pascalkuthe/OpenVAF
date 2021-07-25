/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

//! This module is responsible for lowering an [`Ast`](crate::ast::Ast) to an [`Hir`](hir::Hir)
//!
//! This pass mainly applies the following three transformations to the AST
//!
//!
//! ## [Name resolution](name_resolution)
//!
//!   Names (such as variable references) are resolved to declarations.
//!   It is enforced that the resolved declarations are the right type of Hir node
//!   (no type checking that happens during `hir_lowering`
//!   Ids of the resolved declarations are then stored in the HIR inplace of identifiers in the AST
//!
//!
//! ## [Branch resolution](BranchResolver)
//!
//!   Unnamed branches ( accessed using for example using `<nature>(<net1>,<net2>)` )  are created as needed
//!   and tracked so that the same unnamed branch isn't created multiple times.
//!   Furthermore it is enforced that disciplines of the nets defining a branch are comparable
//!   and that branches are only accessed using the flow/potential Nature of those disciplines
//!
//! ## Context based information
//!
//!   Some expressions and statements are not allowed in some places (for example in an analog/digital context).
//!   During the fold these [states](VerilogContext) are tracked and errors are generated when an illegal expressions/statements is used
//!
//!
//!

#[doc(inline)]
pub use branches::resolver::BranchResolver;
#[doc(inline)]
use branches::Branches;
#[doc(inline)]
use expression::ExpressionFolder;
#[doc(inline)]
use global::Global;
#[doc(inline)]
use statements::Statements;

use crate::error::Error::{ExpectedIdentifier, ReservedSymbol};
use crate::error::{Error, MockSymbolDeclaration};

use crate::allowed_operations::VerilogAState;
use crate::name_resolution::Resolver;
use ast as ast;
use ast::{Ast, HierarchicalId};
use data_structures::bit_set::BitSet;
use data_structures::index_vec::index_vec;
use data_structures::index_vec::IndexVec;
use diagnostics::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use hir::{AllowedOperations, Hir, Node, SyntaxContextData};
use ir::ids::{AttributeId, SyntaxCtx};
use ir::{Attributes, Spanned};
use session::sourcemap::span::DUMMY_SP;
use session::sourcemap::Span;
use session::symbols::{kw, Ident, Symbol};
use std::mem::take;
use tracing::trace_span;

#[macro_use]
pub mod name_resolution;

#[doc(hidden)]
mod branches;
#[doc(hidden)]
mod expression;
#[doc(hidden)]
mod global;
#[doc(hidden)]
mod statements;

mod allowed_operations;
pub mod error;
pub mod lints;

//TODO input/output enforcement

/// A struct that contains data and functionality all ast to hir folds share
/// It is used for abstracting over functionality/data for the `resolve!`/`resolve_hierarchical!` macros and [`BranchResolver`](crate::branches::resolver::BranchResolver)
pub struct Fold<'ast, F: Fn(Symbol) -> AllowedOperations> {
    pub resolver: Resolver<'ast>,
    pub errors: MultiDiagnostic<Error>,
    pub hir: Hir,
    pub ast: &'ast Ast,
    pub sctx: SyntaxCtx,
    folded_attribute: BitSet<AttributeId>,
    allowed_attribute_references: F,
}

impl<'lt, F: Fn(Symbol) -> AllowedOperations> Fold<'lt, F> {
    pub fn check_ident(&mut self, ident: Ident, decl_kind: MockSymbolDeclaration) {
        if ident.is_reserved() {
            self.error(ReservedSymbol { decl_kind, ident })
        }
    }

    pub fn new(ast: &'lt mut Ast, allowed_attribute_references: F) -> Self {
        let mut syntax_ctx = IndexVec::with_capacity(
            ast.parameters.len()
                + ast.variables.len()
                + ast.nets.len()
                + ast.disciplines.len()
                + ast.statements.len()
                + ast.branches.len()
                + ast.blocks.len()
                + ast.modules.len()
                + ast.functions.len()
                + 20,
        );

        syntax_ctx.push(SyntaxContextData {
            span: DUMMY_SP,
            attributes: Attributes::EMPTY,
            parent: None,
        });

        let gnd_node = Node {
            ident: Ident::new(kw::ground, DUMMY_SP),
            discipline: None,
            sctx: SyntaxCtx::ROOT,
        };

        let hir = Hir {
            attributes: take(&mut ast.attributes),
            variables: index_vec![hir::Variable::PLACEHOLDER; ast.variables.len()],
            parameters: index_vec![hir::Parameter::PLACEHOLDER; ast.parameters.len()],
            functions: index_vec![hir::UserFunction::PLACEHOLDER; ast.functions.len()],
            nodes: index_vec![gnd_node],

            ports: IndexVec::new(),
            branches: IndexVec::with_capacity(ast.branches.len()),
            modules: IndexVec::with_capacity(ast.modules.len()),
            disciplines: IndexVec::new(),
            natures: IndexVec::new(),
            expressions: IndexVec::with_capacity(ast.expressions.len()),
            statements: IndexVec::with_capacity(ast.statements.len()),
            syntax_ctx,
        };

        let folded_attribute = BitSet::new_empty(hir.attributes.len());
        let mut res = Self {
            hir,
            ast: &*ast,
            errors: MultiDiagnostic(Vec::with_capacity(32)),
            resolver: Resolver::new(&*ast),
            sctx: SyntaxCtx::ROOT,
            folded_attribute,
            allowed_attribute_references,
        };
        res.resolver.enter_scope(&ast.top_symbols);
        res
    }

    pub fn enter_sctxt(&mut self, span: Span, attributes: Attributes) {
        self.fold_attributes(attributes);

        self.sctx = self.hir.syntax_ctx.push(SyntaxContextData {
            span,
            attributes,
            parent: Some(self.sctx),
        })
    }

    pub fn exit_sctxt(&mut self) -> SyntaxCtx {
        let old_ctx = self.sctx;
        self.sctx = self.hir[old_ctx].parent.unwrap();
        old_ctx
    }

    pub fn error(&mut self, error: Error) {
        self.errors.add(error)
    }

    pub(crate) fn reinterpret_expression_as_hierarchical_identifier(
        &mut self,
        function: &'static str,
        expression: &'lt Spanned<ast::Expression>,
    ) -> Option<&'lt HierarchicalId> {
        if let ast::Expression::Primary(ast::Primary::Reference(ref name)) = expression.contents {
            Some(name)
        } else {
            self.error(ExpectedIdentifier(function, expression.span));
            None
        }
    }

    pub(crate) fn reinterpret_expression_as_identifier(
        &mut self,
        function: &'static str,
        expression: &'lt Spanned<ast::Expression>,
    ) -> Option<Ident> {
        let hident =
            self.reinterpret_expression_as_hierarchical_identifier(function, expression)?;

        match hident.names.as_slice() {
            [ident] => Some(*ident),
            _ => {
                self.error(ExpectedIdentifier(function, expression.span));
                None
            }
        }
    }

    pub fn fold_attributes(&mut self, attributes: Attributes) {
        for attribute in attributes {
            if !self.folded_attribute.insert(attribute) {
                //attribute has already been folded
                continue;
            }
            let span = trace_span!(
                "attribute",
                id = attribute.index(),
                name = display(self.hir[attribute].ident)
            );
            let _enter = span.enter();

            self.hir[attribute].value = self.hir[attribute].value.and_then(|value| {
                let allowed_operations =
                    (self.allowed_attribute_references)(self.hir[attribute].ident.name);

                ExpressionFolder {
                    state: VerilogAState::new_attribute(allowed_operations),
                    branch_resolver: None,
                    base: self,
                }
                .fold(value)
            })
        }
    }
}

/// Lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability
pub fn lower_ast_userfacing<F: Fn(Symbol) -> AllowedOperations>(
    ast: Ast,
    allowed_attributes: F,
) -> UserResult<Hir> {
    lower_ast_userfacing_with_printer(ast, allowed_attributes)
}

/// Lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability
pub fn lower_ast_userfacing_with_printer<
    P: DiagnosticSlicePrinter,
    F: Fn(Symbol) -> AllowedOperations,
>(
    ast: Ast,
    allowed_attributes: F,
) -> UserResult<Hir, P> {
    lower_ast(ast, allowed_attributes).map_err(diagnostics::MultiDiagnostic::user_facing)
}

/// A Helper method to avoid code duplication until try blocks are stable
pub fn lower_ast<F: Fn(Symbol) -> AllowedOperations>(
    mut ast: Ast,
    allowed_attributes: F,
) -> Result<Hir, MultiDiagnostic<Error>> {
    let span = trace_span!("ast_lowering");
    let _enter = span.enter();

    Global::new(&mut ast, allowed_attributes).fold()?.fold()?.fold()
}
