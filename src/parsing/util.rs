//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use pest::iterators::Pair;
use pest::RuleType;

pub(super) fn identifier_string<R: RuleType>(matched_pair: Pair<R>) -> String {
    if matched_pair.as_str().starts_with('\\') {
        let raw = matched_pair.as_str();
        raw[1..raw.len() - 1].to_string()
    } else {
        matched_pair.as_str().to_string()
    }
}

macro_rules! unexpected_rule {
    ( $ unexpected_pair: expr) => {
        panic!(
            "Unexpected Rule {:?} from string {}",
            $unexpected_pair.as_rule(),
            $unexpected_pair.as_str()
        )
    };
}

//Macros that make working with Pest Pairs Iterators more convenient by extending the if let/for syntax to allow imposing conditions using where

macro_rules! for_rules {
    //A for loop that only consumes from pairs when the next node is one of the specified nodes. if not the loop exits
    ($i:ident in $pairs:ident where $rule:pat $(| $optional_rule:pat),* => $body:block) => {
        while let Some($i) = $pairs.peek(){
            if let $rule $(| $optional_rule),* = $i.as_rule(){
                $pairs.next();
                $body;
            }else{
                break;
            }
        }
    };
    //A for loop that only consumes from pairs when the next node is one of the specified nodes. if not the loop exits
    ($i:ident in $pairs:ident where !$rule:pat $(| $optional_rule:pat),* => $body:block) => {
        while let Some($i) = $pairs.peek(){
            if let $rule $(| $optional_rule),* = $i.as_rule(){
                break;
            }else{
                $pairs.next();
                $body;
            }
        }
    };
}
//An if let statement that only executes if the next item in pairs is of the specified rule
macro_rules! if_rule {
(let Some($i:ident) = $pairs:ident.next() where $rule:pat $(| $optional_rule:pat),* => $body:block) => {
    if let Some($i) = $pairs.peek(){
        if let $rule $(| $optional_rule),* = $i.as_rule() {
                 $pairs.next();
                 $body
            }
    }}
}

//Short hands that I like but don't offer too much

macro_rules! as_string {
    ($pair:expr) => {
        $pair.as_str().to_string()
    };
}
