use rowan::TextRange;
use tokens::SyntaxKind;
use tokens::SyntaxKind::NET_TYPE;

use crate::ast::{
    self, support, ArgListOwner, AttrsOwner, BlockItem, Expr, FunctionItem, LiteralKind,
    ModulePortKind, ModulePorts, Name, PathSegmentKind,
};
use crate::name::{kw, kw_comp};
use crate::{match_ast, AstNode, AstPtr, SyntaxError, SyntaxNode, SyntaxNodePtr, T};

pub(crate) fn validate(root: &SyntaxNode, errors: &mut Vec<SyntaxError>) {
    for node in root.descendants() {
        match_ast! {
            match node {
                ast::Path(path) => validate_path(path,errors),
                ast::BlockStmt(block) => validate_block(block, errors),
                ast::Function(fun) => validate_function(fun, errors),
                ast::BranchDecl(decl) => validate_branch_decl(decl, errors),
                ast::DisciplineDecl(decl) => validate_discipline_decl(decl,errors),
                ast::NatureDecl(decl) => validate_nature_decl(decl,errors),
                ast::NatureAttr(attr) => validate_nature_attr(attr,errors),
                ast::Literal(decl) => validate_literal(decl, errors),
                ast::Name(name) => validate_name(name,errors),
                ast::ModuleDecl(module) => validate_module(module,errors),
                _ => validate_net_type_token(node,errors)
            }
        }
    }
}

fn validate_net_type_token(node: SyntaxNode, errors: &mut Vec<SyntaxError>) {
    if matches!(node.kind(), SyntaxKind::NET_DECL | SyntaxKind::PORT_DECL) {
        if let Some(token) = support::token(&node, NET_TYPE) {
            if token.text() != kw::raw::ground {
                errors.push(SyntaxError::IllegalNetType {
                    found: token.text().to_owned(),
                    range: token.text_range(),
                })
            }
        }
    }
}

fn validate_module(module: ast::ModuleDecl, errors: &mut Vec<SyntaxError>) {
    let ports = if let Some(ports) = module.module_ports() { ports } else { return };
    let has_decl = validate_module_ports(&ports, errors);
    if !has_decl {
        return;
    }
    let body_ports: Vec<_> = module.body_ports().map(|port| port.syntax().text_range()).collect();
    if !body_ports.is_empty() {
        errors.push(SyntaxError::IllegalBodyPorts { head: ports.syntax().text_range(), body_ports })
    }
}

fn validate_module_ports(ports: &ModulePorts, errors: &mut Vec<SyntaxError>) -> bool {
    let mut names: Vec<Vec<ast::Name>> = Vec::new();
    let mut has_decl = false;
    for port in ports.ports() {
        if let ModulePortKind::Name(name) = port.kind() {
            if let Some(dst) = names.iter_mut().find(|locs| locs[0].text() == name.text()) {
                dst.push(name.clone())
            } else {
                names.push(vec![name.clone()])
            }
        } else {
            has_decl = true
        }
    }

    if !names.is_empty() && has_decl {
        errors.push(SyntaxError::MixedModuleHead { module_ports: AstPtr::new(ports) });
        // Don't lint body ports when the head is ambigous
        return false;
    }

    for locs in names {
        if locs.len() == 1 {
            continue;
        }
        let name = locs[0].text().to_owned();
        errors.push(SyntaxError::DuplicatePort {
            pos: locs.into_iter().map(|it| it.syntax().text_range()).collect(),
            name,
        })
    }

    has_decl
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

fn validate_nature_attr(attr: ast::NatureAttr, errors: &mut Vec<SyntaxError>) {
    if attr.name().map_or(false, |name| name.text() == "units") {
        if let Some(Expr::Literal(literal)) = attr.val() {
            if !matches!(literal.kind(), LiteralKind::String(_)) {
                errors.push(SyntaxError::UnitsExpectedStringLiteral {
                    range: literal.syntax().text_range(),
                })
            }
        }
    }
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

fn validate_name(name: Name, errors: &mut Vec<SyntaxError>) {
    if let Some(ident) = name.ident_token() {
        let parent = name.syntax().parent();
        let p = parent.as_ref();

        let compat = match ident.text() {
            kw::raw::units if p.map_or(false, |p| p.kind() == SyntaxKind::ATTR) => return,
            kw::raw::units
            | kw::raw::idt_nature
            | kw::raw::ddt_nature
            | kw::raw::abstol
            | kw::raw::access
                if p.map_or(false, |p| p.kind() == SyntaxKind::NATURE_ATTR) =>
            {
                return
            }
            kw::raw::domain | kw::raw::potential | kw::raw::flow
                if p.map_or(false, |p| p.kind() == SyntaxKind::DISCIPLINE_ATTR) =>
            {
                return
            }
            ident if kw::is_reserved(ident) => false,
            ident if kw_comp::is_reserved(ident) => true,
            _ => return,
        };

        errors.push(SyntaxError::ReservedIdentifier {
            src: SyntaxNodePtr::new(name.syntax()),
            compat,
            name: ident.text().to_owned(),
        })
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

fn validate_nature_decl(nature: ast::NatureDecl, errors: &mut Vec<SyntaxError>) {
    if let Some(parent) = nature.parent() {
        check_nature_path(&parent, errors)
    }
    for attr in nature.attrs() {
        if let (Some(name), Some(val)) = (attr.name(), attr.val()) {
            let name_text = name.syntax().text();
            if name_text == "ddt_nature" || name_text == "idt_nature" {
                check_nature_ref_attr(&val, errors)
            } else if name_text == "access" && val.as_raw_ident().is_none() {
                errors.push(SyntaxError::IllegalAttriubte {
                    attr: "access",
                    expected: "an identifier",
                    range: val.syntax().text_range(),
                })
            }
        }
    }
}

fn check_nature_path(path: &ast::Path, errors: &mut Vec<SyntaxError>) {
    if let Some(segment) = path.segment_token() {
        match path.qualifiers().count() {
            0 => (),
            1 if matches!(segment.text(), "ddt_nature" | "idt_nature") => (),
            _ => errors.push(SyntaxError::IllegalNatureIdent { range: path.syntax().text_range() }),
        }
    }
}

fn check_nature_ref_attr(val: &Expr, errors: &mut Vec<SyntaxError>) {
    if let Expr::PathExpr(path) = val {
        if let Some(path) = path.path() {
            check_nature_path(&path, errors)
        }
    } else if val.syntax().children_with_tokens().all(|t| t.kind() != SyntaxKind::ERROR) {
        errors.push(SyntaxError::IllegalNatureIdent { range: val.syntax().text_range() })
    }
}

fn validate_discipline_decl(discipline: ast::DisciplineDecl, errors: &mut Vec<SyntaxError>) {
    for attr in discipline.discipline_attrs() {
        if let Some(name) = attr.name() {
            let is_overwrite = match name.qualifier() {
                Some(qual)
                    if (qual.syntax().text() == "potential" || qual.syntax.text() == "flow")
                        && qual.qualifier().is_none() =>
                {
                    true
                }
                None => false,
                _ => {
                    errors.push(SyntaxError::IllegalDisciplineAttrIdent {
                        range: name.syntax().text_range(),
                    });
                    continue;
                }
            };

            let name_text = name.syntax().text().to_string();
            match &*name_text {
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

            if let Some(val) = attr.val() {
                match &*name_text {
                    "potential" | "flow" => check_nature_ref_attr(&val, errors),
                    "idt_nature" | "ddt_nature" if is_overwrite => {
                        check_nature_ref_attr(&val, errors)
                    }

                    "domain" => {
                        let src = val.syntax().text();
                        if src != "continous" && src != "discrete" {
                            errors.push(SyntaxError::IllegalAttriubte {
                                attr: "domain",
                                expected: "continous or discrete",
                                range: val.syntax().text_range(),
                            })
                        }
                    }

                    _ => (),
                }
            }
        }
    }
}
