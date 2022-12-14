//! This module defines Concrete Syntax Tree (CST), used by OpenVAF.
//!
//! The CST includes comments and whitespace, provides a single node type,
//! `SyntaxNode`, and a basic traversal API (parent, children, siblings).
//!
//! The *real* implementation is in the (language-agnostic) `rowan` crate, this
//! module just wraps its API.

pub(crate) use rowan::GreenNode;
use rowan::Language;

use crate::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VerilogALanguage {}
impl Language for VerilogALanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        SyntaxKind::from(raw.0)
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
    }
}

pub type SyntaxNode = rowan::SyntaxNode<VerilogALanguage>;
pub type SyntaxToken = rowan::SyntaxToken<VerilogALanguage>;
// pub type SyntaxElement = rowan::SyntaxElement<VerilogALanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<VerilogALanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<VerilogALanguage>;

// Syntax node children iterated in reverse order
// Fairly trivial but sadly lacking from rowan
#[derive(Clone, Debug)]
pub struct RevSyntaxNodeChildren {
    next: Option<SyntaxNode>,
}

impl RevSyntaxNodeChildren {
    pub fn new(parent: &SyntaxNode) -> RevSyntaxNodeChildren {
        RevSyntaxNodeChildren { next: parent.last_child() }
    }
}

impl Iterator for RevSyntaxNodeChildren {
    type Item = SyntaxNode;
    fn next(&mut self) -> Option<SyntaxNode> {
        self.next.take().map(|next| {
            self.next = next.prev_sibling();
            next
        })
    }
}
