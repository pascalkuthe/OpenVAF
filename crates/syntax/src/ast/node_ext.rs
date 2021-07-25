//! Various extension methods to ast Nodes, which are hard to code-generate.
//! Extensions for various expressions live in a sibling `expr_extensions` module.

use crate::ast::support;
use crate::ast::{self, AstNode};
use crate::SyntaxToken;
use parser::SyntaxKind::{IDENT, ROOT_KW};
use std::iter::successors;

use super::AstChildren;

// impl ast::PathSegment {
//     pub fn parent_path(&self) -> Option<Path> {
//         self.syntax.parent().and_then(|x| Path::cast(x))
//     }
// }

impl ast::Path {
    pub fn parent_path(&self) -> Option<ast::Path> {
        self.syntax().parent().and_then(ast::Path::cast)
    }

    pub fn first_qualifier(&self) -> ast::Path {
        successors(Some(self.clone()), ast::Path::qualifier).last().unwrap()
    }

    //     pub fn first_segment(&self) -> Option<ast::PathSegment> {
    //         self.first_qualifier().segment()
    //     }

    //     pub fn last_segment(&self) -> Option<ast::PathSegment> {
    //         self.top_path().segment()
    //     }

    //     pub fn segments(&self) -> impl Iterator<Item = ast::PathSegment> + Clone {
    //         successors(self.first_segment(), |p| {
    //             p.parent_path().and_then(|path| path.parent_path()).and_then(|p| p.segment())
    //         })
    //     }

    pub fn qualifiers(&self) -> impl Iterator<Item = ast::Path> + Clone {
        successors(self.qualifier(), |p| p.qualifier())
    }

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
    pub fn analog_behaviour(&self) -> AstChildren<ast::AnalogBehaviour> {
        support::children(&self.syntax)
    }
}

#[derive(PartialEq, Eq,Debug,Clone)]
pub struct PathSegment {
    pub syntax: SyntaxToken,
    pub kind: PathSegmentKind,
}

#[derive(PartialEq, Eq,Debug,Clone, Copy)]
pub enum PathSegmentKind {
    Root,
    Name,
}
