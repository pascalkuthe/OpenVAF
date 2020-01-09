/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use intrusive_collections::__core::borrow::BorrowMut;
use std::borrow::Borrow;

use crate::parser::error::List;
use crate::parser::error::Result;
use crate::parser::lexer::Token;
use crate::source::Span;

/* TODO for parser use
pub fn parse_list<'source,T,C,TokenHandler, NewEntry,NextToken>(terminator:Token, separator:Token, error_type: List,
handel_token:TokenHandler,new_entry:NewEntry,next_token:NextToken,context:&mut C) ->Result<Vec<T>>
    where TokenHandler: Fn(Token,&C,T)->Result
        ,NewEntry:Fn()->T
        ,NextToken:Fn(&C)->Result<Token>{
    let mut res = Vec::new();//Todo sralloc Vec
    res.push(new_entry());
    loop{
        let last_colon;
        match next_token(context.borrow_mut())? {
            token if token == terminator=> break,
            token if token == separator => {
                res.push(new_entry());
            }
            token => handel_token(token,context.borrow_mut(),res.last())
        }
    }
    Ok(res)
}*/
