/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use lalrpop_util::lalrpop_mod;

use declarations::SymbolTableBuilder;

use crate::error::Error;
use openvaf_ast::{Ast, Parameter, ParameterConstraints, Type, Variable};
use openvaf_diagnostics::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use openvaf_ir::AttrSpanned;
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
    node: AttrSpanned<Vec<(Ident, ExpressionId, Vec<R>)>>,
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

        let node = AttrSpanned {
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
    node: AttrSpanned<(Type, Vec<(Ident, Option<ExpressionId>)>)>,
) {
    for (ident, default) in node.contents.1 {
        let var = Variable {
            ident,
            default,
            ty: node.contents.0,
        };

        let node = AttrSpanned {
            contents: var,
            span: node.span,
            attributes: node.attributes,
        };

        let id = ast.variables.push(node);
        symbols.insert(ast, errors, id);
    }
}
