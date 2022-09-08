//! Various extension methods to ast Nodes, which are hard to code-generate.
//! Extensions for various expressions live in a sibling `expr_extensions` module.

use std::iter::successors;

use stdx::impl_debug;

use super::{
    AnalogBehaviour, ArgListOwner, Assign, AstChildTokens, AstChildren, Constraint, EventStmt,
    Expr, ForStmt, Function, ModulePortKind, Path, PortFlow, Range, Stmt, StrLit,
};
use crate::ast::{self, support, AstNode};
use crate::SyntaxKind::{IDENT, ROOT_KW};
use crate::{SyntaxToken, T};

// impl ast::PathSegment {
//     pub fn parent_path(&self) -> Option<Path> {
//         self.syntax.parent().and_then(|x| Path::cast(x))
//     }
// }

impl ast::Path {
    pub fn parent_path(&self) -> Option<ast::Path> {
        self.syntax().parent().and_then(ast::Path::cast)
    }

    #[must_use]
    pub fn first_qualifier(&self) -> ast::Path {
        successors(Some(self.clone()), ast::Path::qualifier).last().unwrap()
    }

    pub fn first_segment(&self) -> Option<ast::PathSegment> {
        self.first_qualifier().segment()
    }

    //     pub fn last_segment(&self) -> Option<ast::PathSegment> {
    //         self.top_path().segment()
    //     }

    pub fn segments(&self) -> impl Iterator<Item = ast::PathSegment> + Clone {
        successors(self.first_segment(), |p| {
            p.parent_path().and_then(|path| path.parent_path()).and_then(|p| p.segment())
        })
    }

    pub fn qualifiers(&self) -> impl Iterator<Item = ast::Path> + Clone {
        successors(self.qualifier(), |p| p.qualifier())
    }

    #[must_use]
    pub fn top_path(&self) -> ast::Path {
        successors(Some(self.clone()), ast::Path::parent_path).last().unwrap()
    }

    pub fn segment_token(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .find(|e| matches!(e.kind(), IDENT | ROOT_KW))
            .and_then(|e| e.into_token())
    }

    pub fn segment_kind(&self) -> Option<PathSegmentKind> {
        self.syntax().children_with_tokens().find_map(|e| match e.kind() {
            IDENT => Some(PathSegmentKind::Name),
            ROOT_KW => Some(PathSegmentKind::Root),
            _ => None,
        })
    }

    pub fn segment(&self) -> Option<PathSegment> {
        self.syntax().children_with_tokens().find_map(|e| {
            let kind = match e.kind() {
                IDENT => PathSegmentKind::Name,
                ROOT_KW => PathSegmentKind::Root,
                _ => return None,
            };
            Some(PathSegment { kind, syntax: e.into_token().unwrap() })
        })
    }

    pub fn as_raw_ident(&self) -> Option<SyntaxToken> {
        let segment = self.segment()?;
        let is_valid =
            self.qualifier().is_none() && matches!(segment.kind, ast::PathSegmentKind::Name);
        is_valid.then(|| segment.syntax)
    }
}
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct PathSegment {
    pub syntax: SyntaxToken,
    pub kind: PathSegmentKind,
}

impl PathSegment {
    pub fn parent_path(&self) -> Option<ast::Path> {
        self.syntax.parent().and_then(ast::Path::cast)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum PathSegmentKind {
    Root,
    Name,
}

impl ast::Expr {
    pub fn as_raw_ident(&self) -> Option<SyntaxToken> {
        self.as_path()?.as_raw_ident()
    }

    pub fn as_path(&self) -> Option<Path> {
        if let ast::Expr::PathExpr(path_expr) = self {
            path_expr.path()
        } else {
            None
        }
    }
}

impl ast::Range {
    // if the range bound is missing we just assume inclusive here

    pub fn start_inclusive(&self) -> bool {
        self.l_brack_token().is_some()
    }

    pub fn end_inclusive(&self) -> bool {
        self.r_brack_token().is_some()
    }

    pub fn start(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).next()
    }

    pub fn end(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }
}

impl ast::IfStmt {
    pub fn then_branch(&self) -> Option<ast::Stmt> {
        support::children(self.syntax()).next()
    }

    pub fn else_branch(&self) -> Option<ast::Stmt> {
        support::children(self.syntax()).nth(1)
    }
}

impl ast::ModuleDecl {
    pub fn analog_behaviour(&self) -> impl Iterator<Item = Stmt> {
        support::children::<AnalogBehaviour>(self.syntax()).filter_map(|it| it.stmt())
    }

    pub fn body_ports(&self) -> AstChildren<ast::BodyPortDecl> {
        support::children(self.syntax())
    }
}

impl ast::ModulePort {
    pub fn kind(&self) -> ModulePortKind {
        support::child(&self.syntax).unwrap()
    }
}

impl ast::ModulePorts {
    pub fn declarations(&self) -> AstChildren<ast::PortDecl> {
        support::children(self.syntax())
    }

    pub fn names(&self) -> AstChildren<ast::Name> {
        support::children(self.syntax())
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum AssignOp {
    Contribute,
    Assign,
}

impl_debug! {
    match AssignOp{
        AssignOp::Contribute => "<+";
        AssignOp::Assign => "=";
    }
}

impl Assign {
    pub fn op(&self) -> Option<AssignOp> {
        if support::token(self.syntax(), T![=]).is_some() {
            Some(AssignOp::Assign)
        } else if support::token(self.syntax(), T![<+]).is_some() {
            Some(AssignOp::Contribute)
        } else {
            None
        }
    }

    pub fn lval(&self) -> Option<ast::Expr> {
        support::child(self.syntax())
    }

    pub fn rval(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }
}

impl ForStmt {
    pub fn init(&self) -> Option<Stmt> {
        support::child(self.syntax())
    }

    pub fn incr(&self) -> Option<Stmt> {
        support::children(self.syntax()).nth(1)
    }

    pub fn for_body(&self) -> Option<Stmt> {
        support::children(self.syntax()).nth(2)
    }
}

impl EventStmt {
    pub fn sim_phases(&self) -> AstChildTokens<StrLit> {
        support::child_token(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BranchKind {
    PortFlow(PortFlow),
    NodeGnd(Path),
    Nodes(Path, Path),
}

impl ast::BranchDecl {
    pub fn branch_kind(&self) -> Option<BranchKind> {
        let nodes = self.arg_list()?;
        let node1 = nodes.args().next()?;
        let node2 = nodes.args().nth(1);

        let kind = match node2 {
            Some(node2) => BranchKind::Nodes(node1.as_path()?, node2.as_path()?),
            None => {
                if let Some(node) = node1.as_path() {
                    BranchKind::NodeGnd(node)
                } else if let ast::Expr::PortFlow(port_flow) = node1 {
                    BranchKind::PortFlow(port_flow)
                } else {
                    return None;
                }
            }
        };

        Some(kind)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ConstraintKind {
    Exclude,
    From,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ConstraintValue {
    Range(Range),
    Val(Expr),
}

impl Constraint {
    pub fn kind(&self) -> Option<ConstraintKind> {
        if self.from_token().is_some() {
            Some(ConstraintKind::From)
        } else if self.exclude_token().is_some() {
            Some(ConstraintKind::Exclude)
        } else {
            None
        }
    }

    pub fn val(&self) -> Option<ConstraintValue> {
        if let Some(range) = self.range() {
            Some(ConstraintValue::Range(range))
        } else {
            Some(ConstraintValue::Val(self.expr()?))
        }
    }
}

impl Function {
    pub fn body(&self) -> AstChildren<Stmt> {
        support::children(self.syntax())
    }
}
