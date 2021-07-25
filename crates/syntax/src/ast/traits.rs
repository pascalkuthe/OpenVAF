/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use rowan::{GreenNodeData, GreenTokenData, NodeOrToken};

use crate::ast::{support, AstChildren};
use crate::{ast, AstNode, SyntaxNode, TokenText};
use std::borrow::Cow;
use std::iter::FlatMap;

pub trait ArgListOwner: AstNode {
    fn arg_list(&self) -> Option<ast::ArgList> {
        support::child(self.syntax())
    }
}

pub type AttrIter = FlatMap<
    AstChildren<ast::AttrList>,
    AstChildren<ast::Attr>,
    fn(ast::AttrList) -> AstChildren<ast::Attr>,
>;

pub trait AttrsOwner: AstNode {
    fn attrs(&self) -> AttrIter {
        support::children::<ast::AttrList>(self.syntax())
            .flat_map(|list| support::children::<ast::Attr>(list.syntax()))
    }
    fn has_attr(&self, name: &str) -> bool {
        self.attrs().any(|attr| attr.name().map_or(false, |n| n.text() == name))
    }
    fn get_attr(&self, name: &str) -> Option<ast::Attr> {
        self.attrs().find(|attr| attr.name().map_or(false, |n| n.text() == name)).clone()
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
