/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

//! In OpenVAF syntax trees are transient objects.
//!
//! That means that we create trees when we need them, and tear them down to
//! save memory. In this architecture, hanging on to a particular syntax node
//! for a long time is ill-advisable, as that keeps the whole tree resident.
//!
//! Instead, we provide a [`SyntaxNodePtr`] type, which stores information about
//! *location* of a particular syntax node in a tree. Its a small type which can
//! be cheaply stored, and which can be resolved to a real [`SyntaxNode`] when
//! necessary.

use std::hash::{Hash, Hasher};
use std::iter::successors;
use std::marker::PhantomData;

use crate::{AstNode, SyntaxKind, SyntaxNode, SyntaxToken, TextRange};

/// A pointer to a syntax node inside a file. It can be used to remember a
/// specific node across reparses of the same file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntaxNodePtr {
    // Don't expose this field further. At some point, we might want to replace
    // range with node id.
    pub(crate) range: TextRange,
    kind: SyntaxKind,
}

impl SyntaxNodePtr {
    #[inline]
    pub fn new(node: &SyntaxNode) -> SyntaxNodePtr {
        SyntaxNodePtr { range: node.text_range(), kind: node.kind() }
    }

    #[inline]
    pub fn new_token(node: &SyntaxToken) -> SyntaxNodePtr {
        SyntaxNodePtr { range: node.text_range(), kind: node.kind() }
    }

    #[inline]
    pub fn range(&self) -> TextRange {
        self.range
    }

    /// "Dereference" the pointer to get the node it points to.
    ///
    /// Panics if node is not found, so make sure that `root` syntax tree is
    /// equivalent (is build from the same text) to the tree which was
    /// originally used to get this [`SyntaxNodePtr`].
    ///
    /// The complexity is linear in the depth of the tree and logarithmic in
    /// tree width. As most trees are shallow, thinking about this as
    /// `O(log(N))` in the size of the tree is not too wrong!
    pub fn to_node(&self, root: &SyntaxNode) -> SyntaxNode {
        assert!(root.parent().is_none());
        successors(Some(root.clone()), |node| {
            node.child_or_token_at_range(self.range).and_then(|it| it.into_node())
        })
        .find(|it| it.text_range() == self.range && it.kind() == self.kind)
        .unwrap_or_else(|| panic!("can't resolve local ptr to SyntaxNode: {:?}", self))
    }

    pub fn cast<N: AstNode>(self) -> Option<AstPtr<N>> {
        if !N::can_cast(self.kind) {
            return None;
        }
        Some(AstPtr { raw: self, _ty: PhantomData })
    }

    #[inline]
    pub fn syntax_kind(&self) -> SyntaxKind {
        self.kind
    }
}

/// Like `SyntaxNodePtr`, but remembers the type of node
#[derive(Debug)]
pub struct AstPtr<N: AstNode> {
    raw: SyntaxNodePtr,
    _ty: PhantomData<fn() -> N>,
}

impl<N: AstNode> Clone for AstPtr<N> {
    fn clone(&self) -> AstPtr<N> {
        AstPtr { raw: self.raw, _ty: PhantomData }
    }
}

impl<N: AstNode> Eq for AstPtr<N> {}

impl<N: AstNode> PartialEq for AstPtr<N> {
    fn eq(&self, other: &AstPtr<N>) -> bool {
        self.raw == other.raw
    }
}

impl<N: AstNode> Hash for AstPtr<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw.hash(state)
    }
}

impl<N: AstNode> AstPtr<N> {
    #[inline]
    pub fn new(node: &N) -> AstPtr<N> {
        AstPtr { raw: SyntaxNodePtr::new(node.syntax()), _ty: PhantomData }
    }

    #[inline]
    pub fn to_node(&self, root: &SyntaxNode) -> N {
        let syntax_node = self.raw.to_node(root);
        N::cast(syntax_node).unwrap()
    }

    #[inline]
    pub fn syntax_node_ptr(&self) -> SyntaxNodePtr {
        self.raw
    }

    #[inline]
    pub fn cast<U: AstNode>(self) -> Option<AstPtr<U>> {
        if !U::can_cast(self.raw.kind) {
            return None;
        }
        Some(AstPtr { raw: self.raw, _ty: PhantomData })
    }

    #[inline]
    pub fn range(&self) -> TextRange {
        self.raw.range
    }

    #[inline]
    pub fn syntax_kind(&self) -> SyntaxKind {
        self.raw.kind
    }
}

impl<N: AstNode> From<AstPtr<N>> for SyntaxNodePtr {
    fn from(ptr: AstPtr<N>) -> SyntaxNodePtr {
        ptr.raw
    }
}

// #[test]
// fn test_local_syntax_ptr() {
//     use crate::{ast, AstNode, SourceFile};

//     let file = SourceFile::parse("struct Foo { f: u32, }").ok().unwrap();
//     let field = file.syntax().descendants().find_map(ast::RecordField::cast).unwrap();
//     let ptr = SyntaxNodePtr::new(field.syntax());
//     let field_syntax = ptr.to_node(file.syntax());
//     assert_eq!(field.syntax(), &field_syntax);
// }
