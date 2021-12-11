mod expr_ext;
mod generated;
mod node_ext;
mod traits;

use std::marker::PhantomData;

pub use self::expr_ext::{ArrayExprKind, BinaryOp, LiteralKind, UnaryOp};
pub use self::generated::nodes::*;
pub use self::generated::tokens::*;
pub use self::node_ext::{
    AssignOp, BranchKind, ConstraintKind, ConstraintValue, PathSegment, PathSegmentKind,
};
pub use self::traits::*;
use crate::syntax_node::{SyntaxElementChildren, SyntaxNode, SyntaxNodeChildren, SyntaxToken};
use crate::SyntaxKind;

/// The main trait to go from untyped `SyntaxNode`  to a typed ast. The
/// conversion itself has zero runtime cost: ast and syntax nodes have exactly
/// the same representation: a pointer to the tree root and a pointer to the
/// node itself.
pub trait AstNode {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
    fn clone_for_update(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_for_update()).unwrap()
    }
    fn clone_subtree(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_subtree()).unwrap()
    }
}

/// Like `AstNode`, but wraps tokens rather than interior nodes.
pub trait AstToken {
    fn can_cast(token: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstChildren { inner: parent.children(), ph: PhantomData }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

#[derive(Debug, Clone)]
pub struct AstChildTokens<N> {
    inner: SyntaxElementChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildTokens<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstChildTokens { inner: parent.children_with_tokens(), ph: PhantomData }
    }
}

impl<N: AstToken> Iterator for AstChildTokens<N> {
    type Item = N;
    fn next(&mut self) -> Option<N> {
        self.inner.find_map(|child| N::cast(child.into_token()?))
    }
}

pub(crate) mod support {
    use super::{
        AstChildTokens, AstChildren, AstNode, AstToken, SyntaxKind, SyntaxNode, SyntaxToken,
    };

    pub(crate) fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
        parent.children().find_map(N::cast)
    }

    pub(crate) fn children<N: AstNode>(parent: &SyntaxNode) -> AstChildren<N> {
        AstChildren::new(parent)
    }

    pub(crate) fn child_token<N: AstToken>(parent: &SyntaxNode) -> AstChildTokens<N> {
        AstChildTokens::new(parent)
    }

    pub(crate) fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        parent.children_with_tokens().filter_map(|it| it.into_token()).find(|it| it.kind() == kind)
    }
}
