//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use lalrpop_util::lalrpop_mod;

use declarations::SymbolTableBuilder;

use crate::ast::{Ast, Parameter};
use crate::diagnostic::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use crate::ir::ast::{Expression, ParameterType, Variable, VariableType};
use crate::ir::Node;
use crate::ir::Spanned;
use crate::parser::error::{ArgumentCountMissmatch, Error};
use crate::sourcemap::Span;
use crate::symbol::Ident;

use crate::ir::ids::ExpressionId;
pub use crate::parser::tokenstream::{Token, TokenStream};
use crate::SourceMap;
use std::sync::Arc;

mod declarations;
mod error;
mod lints;
mod tokenstream;

#[cfg(test)]
mod test;

lalrpop_mod!(
    #[allow(clippy::all)]
    pub grammar, "/parser/grammar.rs"
);

fn extract_single_arg(
    l: Span,
    args: Vec<ExpressionId>,
    r: Span,
    call: &'static str,
    errors: &mut MultiDiagnostic<Error>,
    ast: &mut Ast,
) -> ExpressionId {
    if let [arg] = args.as_slice() {
        *arg
    } else {
        let span = l.extend(r);

        let missmatch = ArgumentCountMissmatch {
            found: args.len() as u8,
            at_most: 1,
            at_least: 1,
        };
        errors.add(Error::ArgumentCountMissmatch(missmatch, call, span));
        ast.expressions.push(Spanned::new(Expression::Error, span))
    }
}

fn extract_double_arg(
    l: Span,
    args: Vec<ExpressionId>,
    r: Span,
    call: &'static str,
    errors: &mut MultiDiagnostic<Error>,
    ast: &mut Ast,
) -> (ExpressionId, ExpressionId) {
    if let [arg1, arg2] = args.as_slice() {
        (*arg1, *arg2)
    } else {
        let span = l.extend(r);

        let missmatch = ArgumentCountMissmatch {
            found: args.len() as u8,
            at_most: 1,
            at_least: 1,
        };
        errors.add(Error::ArgumentCountMissmatch(missmatch, call, span));
        let err_expr = ast.expressions.push(Spanned::new(Expression::Error, span));
        (err_expr, err_expr)
    }
}

fn extract_single_or_double_arg(
    l: Span,
    args: Vec<ExpressionId>,
    r: Span,
    call: &'static str,
    errors: &mut MultiDiagnostic<Error>,
    ast: &mut Ast,
) -> (ExpressionId, Option<ExpressionId>) {
    match args.as_slice() {
        [arg] => (*arg, None),
        [arg, name] => (*arg, Some(*name)),
        args => {
            let span = l.extend(r);

            let missmatch = ArgumentCountMissmatch {
                found: args.len() as u8,
                at_most: 2,
                at_least: 1,
            };
            errors.add(Error::ArgumentCountMissmatch(missmatch, call, span));
            let err = ast.expressions.push(Spanned::new(Expression::Error, span));
            (err, None)
        }
    }
}

impl Ast {
    pub fn parse_from_token_stream(
        ts: TokenStream,
    ) -> std::result::Result<Self, MultiDiagnostic<Error>> {
        let spanned_iter = ts.into_iter().map(|(token, span)| Ok((span, token, span)));
        let mut ast = Ast::new();
        let mut scopes = SymbolTableBuilder::new();
        let mut errors = MultiDiagnostic(Vec::new());
        let res = grammar::TopParser::new().parse(&mut scopes, &mut ast, &mut errors, spanned_iter);

        if let Err(error) = res {
            errors.add(error)
        }

        if errors.is_empty() {
            ast.top_symbols = scopes.finish();
            Ok(ast)
        } else {
            Err(errors)
        }
    }

    pub fn parse_from_token_stream_user_facing(
        ts: TokenStream,
        sm: &Arc<SourceMap>,
        expansion_disclaimer: &'static str,
    ) -> UserResult<Self> {
        let res = Self::parse_from_token_stream(ts);
        res.map_err(|error| error.user_facing(&sm, expansion_disclaimer))
    }

    pub fn parse_from_token_stream_user_facing_with_printer<P: DiagnosticSlicePrinter>(
        ts: TokenStream,
        sm: &Arc<SourceMap>,
        expansion_disclaimer: &'static str,
    ) -> UserResult<Self, P> {
        let res = Self::parse_from_token_stream(ts);
        res.map_err(|error| error.user_facing(&sm, expansion_disclaimer))
    }

    fn insert_params<R>(
        &mut self,
        errors: &mut MultiDiagnostic<Error>,
        symbols: &mut SymbolTableBuilder,
        node: Node<Vec<(Ident, ExpressionId, Vec<R>)>>,
        mut param_type: impl FnMut(Vec<R>) -> ParameterType,
    ) {
        for (ident, default, ranges) in node.contents {
            let param = Parameter {
                ident,
                default,
                param_type: param_type(ranges),
            };

            let node = Node {
                contents: param,
                span: node.span,
                attributes: node.attributes,
            };
            let id = self.parameters.push(node);

            symbols.insert(self, errors, id);
        }
    }

    #[allow(clippy::type_complexity)]
    fn insert_vars(
        &mut self,
        errors: &mut MultiDiagnostic<Error>,
        symbols: &mut SymbolTableBuilder,
        node: Node<(VariableType, Vec<(Ident, Option<ExpressionId>)>)>,
    ) {
        for (ident, default) in node.contents.1 {
            let var = Variable {
                ident,
                default,
                var_type: node.contents.0,
            };

            let node = Node {
                contents: var,
                span: node.span,
                attributes: node.attributes,
            };

            let id = self.variables.push(node);
            symbols.insert(self, errors, id);
        }
    }
}
