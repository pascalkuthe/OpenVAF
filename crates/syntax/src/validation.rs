use crate::T;
use rowan::TextRange;
use tokens::SyntaxKind;

use crate::{
    ast::{self, ArgListOwner, BlockItem, FunctionItem, PathSegmentKind},
    match_ast, AstNode, AstPtr, SyntaxError, SyntaxNode,
};

pub(crate) fn validate(root: &SyntaxNode, errors: &mut Vec<SyntaxError>) {
    for node in root.descendants() {
        match_ast! {
            match node {
                ast::Path(path) => validate_path(path,errors),
                ast::BlockStmt(block) => validate_block(block, errors),
                ast::Function(fun) => validate_function(fun, errors),
                ast::BranchDecl(decl) => validate_branch_decl(decl, errors),
                ast::DisciplineDecl(decl) => validate_discipline_decl(decl,errors),
        ast::Literal(decl) => validate_literal(decl, errors),
                _ => ()
            }
        }
    }
}

fn is_valid_inf_position(s: SyntaxNode) -> bool {
    if s.kind() == SyntaxKind::RANGE {
        return true;
    }
    if s.parent().map_or(false, |parent| parent.kind() == SyntaxKind::RANGE) {
        if let Some(expr) = ast::PrefixExpr::cast(s) {
            if matches!(expr.op_kind(), Some(ast::UnaryOp::Neg) | None) {
                return true;
            }
        }
    }
    false
}

fn validate_literal(literal: ast::Literal, errors: &mut Vec<SyntaxError>) {
    if literal.kind() == ast::LiteralKind::Inf
        && !literal.syntax.parent().map_or(true, is_valid_inf_position)
    {
        errors.push(SyntaxError::IllegalInfToken { range: literal.syntax().text_range() });
    }
}

fn validate_path(path: ast::Path, errors: &mut Vec<SyntaxError>) {
    if path.segment_kind() == Some(PathSegmentKind::Root) && path.parent_path().is_none() {
        errors.push(SyntaxError::IllegalRootSegment {
            path_segment: path.segment_token().unwrap().text_range(),
            prefix: None,
        })
    }

    for qual in path.qualifiers() {
        if qual.qualifier().is_some() && path.segment_kind() == Some(PathSegmentKind::Root) {
            errors.push(SyntaxError::IllegalRootSegment {
                path_segment: path.segment_token().unwrap().text_range(),
                prefix: Some(path.top_path().syntax().text_range()),
            })
        }
    }
}

fn validate_block(block: ast::BlockStmt, errors: &mut Vec<SyntaxError>) {
    if block.block_scope().is_some() {
        let mut items = block.items();

        let first_stmt = loop {
            match items.next() {
                Some(BlockItem::Stmt(stmt)) => break stmt,
                Some(BlockItem::ParamDecl(_) | BlockItem::VarDecl(_)) => (),
                None => return,
            }
        };

        let misplaced_items: Vec<_> = items
            .filter_map(|item| {
                matches!(item, ast::BlockItem::VarDecl(_) | ast::BlockItem::ParamDecl(_))
                    .then(|| AstPtr::new(&item))
            })
            .collect();

        if !misplaced_items.is_empty() {
            errors.push(SyntaxError::BlockItemsAfterStmt {
                items: misplaced_items,
                first_stmt: first_stmt.syntax().text_range(),
            })
        }
    } else {
        let items: Vec<_> = block
            .items()
            .filter_map(|item| {
                matches!(item, ast::BlockItem::VarDecl(_) | ast::BlockItem::ParamDecl(_))
                    .then(|| AstPtr::new(&item))
            })
            .collect();

        if !items.is_empty() {
            if let Some(begin_token) = block.begin_token() {
                errors.push(SyntaxError::BlockItemsWithoutScope {
                    items,
                    begin_token: begin_token.text_range(),
                })
            }
        }
    }
}

fn validate_function(fun: ast::Function, errors: &mut Vec<SyntaxError>) {
    let mut items = fun.function_items();

    let body = loop {
        match items.next() {
            Some(FunctionItem::Stmt(stmt)) => break stmt,
            None => {
                errors.push(SyntaxError::FunWithoutBody { fun: fun.syntax().text_range() });
                return;
            }
            _ => (),
        }
    };

    let illegal_items: Vec<_> = items
        .clone()
        .filter(|item| !matches!(item, FunctionItem::Stmt(_)))
        .map(|item| AstPtr::new(&item))
        .collect();

    if !illegal_items.is_empty() {
        errors.push(SyntaxError::FunItemsAfterBody {
            items: illegal_items,
            body: body.syntax().text_range(),
        })
    }

    let additional_bodys: Vec<_> = items
        .filter_map(|item| {
            if let FunctionItem::Stmt(stmt) = item {
                Some(stmt.syntax().text_range())
            } else {
                None
            }
        })
        .collect();

    if !additional_bodys.is_empty() {
        errors.push(SyntaxError::MultipleFunBodys { additional_bodys, body: AstPtr::new(&body) })
    }
}

fn validate_branch_decl(decl: ast::BranchDecl, errors: &mut Vec<SyntaxError>) {
    if let Some(arg_list) = decl.arg_list() {
        match arg_list.args().count() {
            1 => {
                let arg = arg_list.args().next().unwrap();
                match arg {
                    ast::Expr::PortFlow(_) => (),
                    ast::Expr::PathExpr(path)
                        if path.path().map_or(true, |path| path.qualifier().is_none()) => {}
                    _ => errors.push(SyntaxError::IllegalBranchNodeExpr {
                        single: true,
                        illegal_nodes: vec![arg.syntax().text_range()],
                    }),
                }
            }
            2 => {
                let arg1 = arg_list.args().next().unwrap();
                let arg2 = arg_list.args().nth(1).unwrap();

                let mut illegal_nodes = Vec::new();

                if arg1.as_path().is_none() {
                    illegal_nodes.push(arg1.syntax().text_range())
                }

                if arg2.as_path().is_none() {
                    illegal_nodes.push(arg2.syntax().text_range())
                }

                if !illegal_nodes.is_empty() {
                    errors.push(SyntaxError::IllegalBranchNodeExpr { single: false, illegal_nodes })
                }
            }
            cnt => errors.push(SyntaxError::IllegalBranchNodeCnt {
                cnt,
                arg_list: arg_list.syntax().text_range(),
            }),
        }
    }
}

fn validate_discipline_decl(discipline: ast::DisciplineDecl, errors: &mut Vec<SyntaxError>) {
    for attr in discipline.discipline_attrs() {
        if let Some(name) = attr.name() {
            match &*name.syntax().text().to_string() {
                "domain" | "potential" | "flow" => {
                    if let Some(tok) = attr.eq_token() {
                        errors.push(SyntaxError::SurplusToken {
                            found: T![=],
                            span: tok.text_range(),
                        })
                    }
                }
                _ if attr.eq_token().is_none() => {
                    if let Some(val) = attr.val() {
                        errors.push(SyntaxError::MissingToken {
                            expected: T![=],
                            span: val.syntax().text_range(),
                            expected_at: TextRange::at(name.syntax().text_range().end(), 0.into()),
                        })
                    }
                }
                _ => (),
            }
        }
    }
}
