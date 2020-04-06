/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use std::fmt::{Debug, Formatter};

use ansi_term::Color::*;
use log::*;
use crate::compact_arena::{TinyArena, NanoArena};
use intrusive_collections::__core::mem::MaybeUninit;
use std::sync::Mutex;

pub type Index = u32;
pub type IndexOffset = i64;
pub type Length = u16;
pub type Range = std::ops::Range<Index>;
pub type LineNumber = u16;

#[derive(Copy, Clone, Debug)]
enum SpanData {
    ///This Span is too large to fit its length in an u16
    LargeSpan,
    Length(Length),
}
lazy_static! {
        static ref INTERNER: Mutex<SpanInterner> = Mutex::new(SpanInterner::new());
    }
struct SpanInterner{
    data: Vec<(u32,u32)>,
}
impl SpanInterner{
    fn new() -> Self{
        Self{
            data: Vec::with_capacity(64),
        }
    }
    fn add(&mut self,start:u32,end:u32)->usize{
        let idx = self.data.len();
        self.data.push ((start,end));
        idx
    }
    fn get(&self,idx:usize)-> (u32,u32){
        unsafe { *self.data.get_unchecked(idx) }
    }
}

#[derive(Copy, Clone)]
pub struct Span {
    start_or_idx: u32,
    data: SpanData,
}
impl Span {
    pub fn new(start: Index, end: Index) -> Self {
        Self::new_with_length(start, end - start)
    }
    pub fn new_with_length(start: Index, len: Index) -> Self {
        //we accept index here instead of Length because we want to allow long ranges using special handling (eventually)
        if len <= std::u16::MAX as u32 {
            Self {
                start_or_idx: start,
                data: SpanData::Length(len as Length),
            }
        } else {
            Self {
                start_or_idx: INTERNER.lock().unwrap().add(start,start+len) as u32,
                data: SpanData::LargeSpan
            }
        }
    }
    pub const fn new_short_span(start: Index, len: u16) -> Self {
        Self {
            start_or_idx: start,
            data: SpanData::Length(len as Length),
        }
    }
    pub fn get_start(self) -> Index {
        match self.data {
            SpanData::LargeSpan =>
                INTERNER.lock().unwrap().get(self.start_or_idx as usize).0,

            SpanData::Length(_) => self.start_or_idx,
        }
    }
    pub fn get_end(self) -> Index {
        match self.data {
            SpanData::LargeSpan =>
                INTERNER.lock().unwrap().get(self.start_or_idx as usize).1,

            SpanData::Length(length) => self.start_or_idx + (length as u32),
        }
    }
    pub fn get_len(self) -> Index {
        match self.data {
            SpanData::LargeSpan => {
                let (start,end) = INTERNER.lock().unwrap().get(self.start_or_idx as usize);
                end - start
            }
            SpanData::Length(length) => length as Index,
        }
    }
    pub fn offset(mut self, offset: Index) -> Self {
        match self.data {
            SpanData::LargeSpan => {
                let mut inter = INTERNER.lock().unwrap();
                let (start,end) = inter.get(self.start_or_idx as usize);
                self.start_or_idx = inter.add(start+offset,end+offset) as u32;
            }
            SpanData::Length(_) => self.start_or_idx += offset,
        }
        self
    }
    pub fn signed_offset(self, offset: IndexOffset) -> Self {
        if offset < 0 {
            self.negative_offset(-offset as Index)
        } else {
            self.offset(offset as Index)
        }
    }
    pub fn negative_offset(mut self, offset: Index) -> Self {
        match self.data {
            SpanData::LargeSpan => {
                let mut inter = INTERNER.lock().unwrap();
                let (start,end) = inter.get(self.start_or_idx as usize);
                self.start_or_idx = inter.add(start-offset,end-offset) as u32;
            }
            SpanData::Length(_) => self.start_or_idx -= offset,
        }
        self
    }
    pub fn extend(self, to: Self) -> Self {
        Self::new(self.get_start(), to.get_end())
    }
}
impl From<Range> for Span {
    fn from(range: Range) -> Self {
        Self::new(range.start, range.end)
    }
}
impl Into<Range> for Span {
    fn into(self) -> Range {
        Range {
            start: self.get_start(),
            end: self.get_end(),
        }
    }
}
impl Into<std::ops::Range<usize>> for Span {
    fn into(self) -> std::ops::Range<usize> {
        std::ops::Range {
            start: self.get_start() as usize,
            end: self.get_end() as usize,
        }
    }
}
impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(format_args!(" [{},{}]", self.get_start(), self.get_end()))
    }
}
