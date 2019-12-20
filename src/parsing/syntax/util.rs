//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use pest::iterators::Pair;

use crate::parsing::syntax::Rule;

pub(crate) type ParseTreeNode<'lifetime> = Pair<'lifetime, Rule>;
// Macros to generate similar functions

//TODO report issue or wait for rust additions to avoid this hack
macro_rules! mk_fold_fn {
    ($name:ident($sel:ident,$parse_tree_node:ident, $lt:lifetime ,$description:ident) $body:block) => {
    //Paste allows to concat identifiers in macros but has a bug
    //A weird error is produced when the actual function is put into a paste::item!
    //apparently all macro parameters are parsed seperatly
    // so its not possible to use the method args in the body
    //This is a workout with the only caviat that this needs to be declared in a seperate impl block in a submodule
    //Otherwise this might create name conflicts that are not transparent
        paste::item!{
        pub (super) fn [<fold_$name>](&mut $sel, $parse_tree_node: ParseTreeNode<$lt>
            , parent_ast_node: NodeId, attributes: Vec<NodeId>) -> SyntaxResult {
                $sel.$name($parse_tree_node,parent_ast_node,attributes)
            }
        }

        #[inline]
        fn $name(&mut $sel, $parse_tree_node: ParseTreeNode<$lt>
        , parent_ast_node: NodeId, attributes: Vec<NodeId>) -> SyntaxResult {
            trace!{"Processing {} from \n {:?}",stringify!($name),$parse_tree_node}
            let mut $description = $parse_tree_node.clone().into_inner();
            let nodeids = $body;
            for node in &nodeids?{
                parent_ast_node.append(*node,&mut $sel.ast.arena);
                $sel.append_attributes(*node,&attributes);
            }
            Ok(())
        }

    };
}

/*
macro_rules! mk_simple_fold_fn {
    ($name:ident($sel:ident,$parse_tree_node:ident,$description:ident) $body:block) => {
    //Paste allows to concat identifiers in macros but has a bug
    //A weird error is produced when the actual function is put into a paste::item!
    //apparently all macro parameters are parsed seperatly
    // so its not possible to use the method args in the body
    //This is a workout with the only caviat that this needs to be declared in a seperate impl block in a submodule
    //Otherwise this might create name conflicts that are not transparent
        paste::item!{
        pub (super) fn [<fold_$name>](&mut $sel, $parse_tree_node: ParseTreeNode
            , parent_ast_node: NodeId, attributes: Vec<NodeId>) -> SyntaxResult {
                $sel.$name($parse_tree_node,$parent_ast_node,$attributes)
            }
        }

        #[inline]
        fn $name(&mut $sel, $parse_tree_node: ParseTreeNode
        , $parent_ast_node: NodeId, $attributes: Vec<NodeId>) -> SyntaxResult {
            trace!{"Processing {} from \n {:?}",stringify!($name),$parse_tree_node}
            let mut $description = $parse_tree_node.clone().into_inner();
            let nodeids = $body;
            for node in &nodeids?{
                let ast_node = $sel.ast.new_node(*node);
                parent_ast_node.append(ast_node,&mut $sel.ast);
                $sel.append_attributes(ast_node,&attributes);
            }
            Ok(())
        }

    };
}*/

macro_rules! rule_eq {
    (let $res:ident = $pair:expr; $eq:pat $(| $additional_eq:pat),* , $neq:pat $(| $additional_neq:pat),*) => {
        let $res = match $pair.as_rule(){
            $eq $(| $additional_eq),* => true,
            $neq $(| $additional_neq),* => false,
            _ => unexpected_rule!($pair),
        };
    };
}
