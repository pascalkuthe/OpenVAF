//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use lalrpop_util::lalrpop_mod;

use declarations::SymbolTableBuilder;

use crate::error::{ArgumentCountMissmatch, Error};
use openvaf_ast::{Ast, Expression, Parameter, ParameterConstraints, Type, Variable};
use openvaf_diagnostics::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use openvaf_ir::{Node, Spanned};
use openvaf_session::sourcemap::Span;
use openvaf_session::symbols::Ident;

pub use crate::tokenstream::{Token, TokenStream};
use openvaf_ir::ids::ExpressionId;

mod declarations;
mod error;
mod lints;
mod tokenstream;

lalrpop_mod!(
    #[allow(clippy::all)]
    pub grammar
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

pub fn parse(ts: TokenStream) -> std::result::Result<Ast, MultiDiagnostic<Error>> {
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

pub fn parse_user_facing(ts: TokenStream) -> UserResult<Ast> {
    let res = parse(ts);
    res.map_err(|error| error.user_facing())
}

pub fn parse_facing_with_printer<P: DiagnosticSlicePrinter>(ts: TokenStream) -> UserResult<Ast, P> {
    let res = parse(ts);
    res.map_err(|error| error.user_facing())
}

fn insert_params<R>(
    ast: &mut Ast,
    errors: &mut MultiDiagnostic<Error>,
    symbols: &mut SymbolTableBuilder,
    node: Node<Vec<(Ident, ExpressionId, Vec<R>)>>,
    mut constraint: impl FnMut(Vec<R>) -> ParameterConstraints,
    ty: Type,
) {
    for (ident, default, ranges) in node.contents {
        let param = Parameter {
            ident,
            default,
            param_constraints: constraint(ranges),
            ty,
        };

        let node = Node {
            contents: param,
            span: node.span,
            attributes: node.attributes,
        };
        let id = ast.parameters.push(node);

        symbols.insert(ast, errors, id);
    }
}

#[allow(clippy::type_complexity)]
fn insert_vars(
    ast: &mut Ast,
    errors: &mut MultiDiagnostic<Error>,
    symbols: &mut SymbolTableBuilder,
    node: Node<(Type, Vec<(Ident, Option<ExpressionId>)>)>,
) {
    for (ident, default) in node.contents.1 {
        let var = Variable {
            ident,
            default,
            ty: node.contents.0,
        };

        let node = Node {
            contents: var,
            span: node.span,
            attributes: node.attributes,
        };

        let id = ast.variables.push(node);
        symbols.insert(ast, errors, id);
    }
}
