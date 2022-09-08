use std::borrow::Cow;
use std::iter::FlatMap;

use rowan::{GreenNodeData, GreenTokenData, NodeOrToken};

use crate::ast::{support, RevAstChildren};
use crate::{ast, AstNode, SyntaxNode, TokenText};

pub trait ArgListOwner: AstNode {
    fn arg_list(&self) -> Option<ast::ArgList> {
        support::child(self.syntax())
    }
}

pub type AttrIter = FlatMap<
    RevAstChildren<ast::AttrList>,
    RevAstChildren<ast::Attr>,
    fn(ast::AttrList) -> RevAstChildren<ast::Attr>,
>;

pub fn attrs(syntax: &SyntaxNode) -> AttrIter {
    support::rev_children::<ast::AttrList>(syntax)
        .flat_map(|list| support::rev_children::<ast::Attr>(list.syntax()))
}

pub trait AttrsOwner: AstNode {
    fn attrs(&self) -> AttrIter {
        attrs(self.syntax())
    }
    fn has_attr(&self, name: &str) -> bool {
        self.attrs().any(|attr| attr.name().map_or(false, |n| n.text() == name))
    }
    fn get_attr(&self, name: &str) -> Option<ast::Attr> {
        self.attrs().find(|attr| attr.name().map_or(false, |n| n.text() == name))
    }
}

impl ast::Name {
    pub fn text(&self) -> TokenText<'_> {
        text_of_first_token(self.syntax())
    }
}

impl ast::NameRef {
    pub fn text(&self) -> TokenText<'_> {
        text_of_first_token(self.syntax())
    }
}

fn text_of_first_token(node: &SyntaxNode) -> TokenText<'_> {
    fn first_token(green_ref: &GreenNodeData) -> &GreenTokenData {
        green_ref.children().next().and_then(NodeOrToken::into_token).unwrap()
    }

    match node.green() {
        Cow::Borrowed(green_ref) => TokenText::borrowed(first_token(green_ref).text()),
        Cow::Owned(green) => TokenText::owned(first_token(&green).to_owned()),
    }
}
