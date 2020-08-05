/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

//! This module is responsible for lowering an [`Ast`](crate::ast::Ast) to an [`Hir`](openvaf_hir::Hir)
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

use bitflags::bitflags;

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

use crate::error::Error;
use crate::error::Error::ExpectedIdentifier;
use crate::expression::{AllowedReferences, ConstantExpressionFolder};
use crate::name_resolution::Resolver;
use bitflags::_core::mem::take;
use openvaf_ast as ast;
use openvaf_ast::{Ast, HierarchicalId};
use openvaf_data_structures::BitSet;
use openvaf_diagnostics::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use openvaf_hir::Hir;
use openvaf_ir::ids::{AttributeId, ExpressionId};
use openvaf_ir::{Attributes, Spanned};
use openvaf_session::symbols::Ident;

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

pub mod error;
pub mod lints;

//TODO input/output enforcement

/// A struct that contains data and functionality all ast to hir folds share
/// It is used for abstracting over functionality/data for the `resolve!`/`resolve_hierarchical!` openvaf_macros and [`BranchResolver`](crate::branches::resolver::BranchResolver)
pub struct Fold<'lt> {
    pub resolver: Resolver<'lt>,
    pub errors: MultiDiagnostic<Error>,
    pub hir: Hir,
    pub ast: &'lt Ast,
    folded_attribute: BitSet<AttributeId>,
}

impl<'lt> Fold<'lt> {
    pub fn new(ast: &'lt mut Ast) -> Self {
        let folded_attribute = BitSet::new_empty(ast.attributes.len_idx());
        let mut res = Self {
            hir: Hir::init(ast),
            ast: &*ast,
            errors: MultiDiagnostic(Vec::with_capacity(32)),
            resolver: Resolver::new(&*ast),
            folded_attribute,
        };
        res.resolver.enter_scope(&ast.top_symbols);
        res
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
}

impl<'lt> Fold<'lt> {
    pub fn fold_attributes(&mut self, attributes: Attributes) {
        for attribute in attributes {
            if self.folded_attribute.put(attribute) {
                //attribute has already been folded
                continue;
            }

            let mut values = take(&mut self.hir[attribute].value);
            for value in &mut values {
                if let Some(expr) =
                    ConstantExpressionFolder(AllowedReferences::All).fold(*value, self)
                {
                    *value = expr
                } else {
                    *value = ExpressionId::MAX_INDEX.into();
                }
            }

            self.hir[attribute].value = values
        }
    }
}

bitflags! {
    /// The Verilog AMS standard uses multiple different grammar rules to enfoce that constants/analog exprerssions only contain items valid in their context
    /// OpenVAF uses flags stored inside this struct instead during the AST to MIR folding process in this module
    pub struct VerilogContext: u8{
        const CONSTANT = 0b0000_0001;
        const CONDITIONAL = 0b0000_0010;
        const FUNCTION = 0b0000_1000;
        // attributes receive special treatment because all references are allowed which would normally not be the allowed
        const ATTRIBUTE = 0b0001_0000;

    }
}

/// Lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability
pub fn lower_ast_userfacing(ast: Ast) -> UserResult<Hir> {
    lower_ast_userfacing_with_printer(ast)
}

/// Lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability
pub fn lower_ast_userfacing_with_printer<P: DiagnosticSlicePrinter>(
    ast: Ast,
) -> UserResult<Hir, P> {
    lower_ast(ast).map_err(openvaf_diagnostics::MultiDiagnostic::user_facing)
}

/// A Helper method to avoid code duplication until try blocks are stable
pub fn lower_ast(mut ast: Ast) -> Result<Hir, MultiDiagnostic<Error>> {
    Ok(Global::new(&mut ast).fold()?.fold()?.fold()?)
}
