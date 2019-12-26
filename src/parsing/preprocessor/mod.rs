/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use log::*;
use crate::parsing::lexer::{Lexer, Token};
use crate::parsing::Span;
use hesr_alloc::{Allocator, NodeId, Immutable, SliceId, ImSrHeVec};
use test::TestResult::TrOk;
use std::collections::HashMap;


pub type MacroReference<'lt> = SliceId<PreprocessedToken<'lt>>;

pub struct PreprocessedSource<'lt> {
    data: Immutable,
    tokens: SliceId<PreprocessedToken<'lt>>
}

impl<'lt> PreprocessedSource<'lt>{
    pub fn new(alloc:Allocator,tokens:SliceId<PreprocessedToken<'lt>>)->Self{
        Self{
            data:Immutable(alloc),
            tokens
        }
    }
    pub fn get_macro_reference_tokens(&self,id:MacroReference)->&[PreprocessedToken]{
        self.data.get_slice(id)
    }
}
enum PreprocessedToken<'lt>{
    LexerToken(Token,Span<'lt>),
    InsertedMacro(NodeId<PreprocessedToken<'lt>>)
}
enum MacroBodyToken<'lt>{
    MacroReference(&'lt str),
    ArgumentReference(usize),
    LexerToken(Token,Span<'lt>),
}
struct Preprocessor<'source,'lt>{
    lexer:Lexer<'source>,
    macro_alloc:&'lt Allocator,
    tokens:ImSrHeVec<'lt,PreprocessedToken<'source>>,
    macros: HashMap<&'source str, Vec<MacroBodyToken<'source>>>,
    calling_macros: Vec<&'source str>,
  //  calling_macro_arguments: HashMap<>,
}

impl<'source,'lt> Preprocessor<'source,'lt> {
    pub fn run(&mut self){
        while self.lexer.token() != Token::EOF{
            match self.lexer.token() {
                Token::Unexpected => /*TODO append error*/(),
                Token::UnexpectedEOF => return/*TODO error*/,
                Token::Include=>unimplemented!(),
                Token::MacroReference => ,
                Token::MacroDef => ,
                _ => self.tokens.push(self.current_lexer_token());
            }
            self.lexer.advance();
        }
    }
    fn current_lexer_token(&self) ->PreprocessedToken<'source>{
        PreprocessedToken::LexerToken(self.lexer.token(),self.lexer.span())
    }
    fn resolve_reference(&mut self)->Result<MacroReference<'source>,()>{
        /*if let Some(definition) = self.macros.get(self.lexer.slice()){
            let span = self.lexer.
            Ok(())
        }else{
            Err(());
        }*/
    }
}
