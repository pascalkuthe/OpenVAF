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
use std::sync::Mutex;
use core::num::NonZeroU16;

pub type Index = u32;
pub type IndexOffset = i64;
pub type Length = u16;
pub type Range = std::ops::Range<Index>;
pub type LineNumber = u16;

#[derive(Copy, Clone, Debug)]
enum SpanData {
    ///This Span is too large to fit its length or zero
    LargeSpan,
    Length(NonZeroU16),
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
    #[inline]
    pub fn new(start: Index, end: Index) -> Self {
        Self::new_with_length(start, end - start)
    }

    #[inline]
    pub fn new_with_length(start: Index, len: Index) -> Self {
        match len{
            len @ 1..=std::u16::MAX =>  Self {
                start_or_idx: start,
                data: SpanData::Length(len as Length),
            },
            0 if start <= 1<<31 -1 => Self {
                start_or_idx: (start << 1) |1,
                data: SpanData::LargeSpan
            },
            len => Self {
                start_or_idx: (INTERNER.lock().unwrap().add(star,start+len) as u32) << 1,
                data: SpanData::LargeSpan
            },

        }
    }

    #[inline]
    pub const fn new_short_span(start: Index, len: u16) -> Self {
        Self {
            start_or_idx: start,
            data: SpanData::Length(len as Length),
        }
    }

    #[inline]
    pub fn get_start(self) -> Index {
        match self.data {
            SpanData::Length(_) => self.start_or_idx,
            SpanData::LargeSpan if self.start_or_idx & 1 == 1=>
                self.start_or_idx >> 1,
            SpanData::LargeSpan =>{
                INTERNER.lock().unwrap().get((self.start_or_idx>>1) as usize).0
            }
        }
    }

    #[inline]
    pub fn get_end(self) -> Index {
        match self.data {
            SpanData::Length(length) => self.start_or_idx + (length as u32),
            SpanData::LargeSpan if self.start_or_idx & 1 == 1=>
                self.start_or_idx>>1,
            SpanData::LargeSpan =>{
                INTERNER.lock().unwrap().get((self.start_or_idx>>1) as usize).1
            }
        }
    }


    pub fn get_len(self) -> Index {
        match self.data {
            SpanData::Length(length) => length as Index,
            SpanData::LargeSpan if self.start_or_idx & 1 == 1=>
                0,
            SpanData::LargeSpan => {
                let (start,end) = INTERNER.lock().unwrap().get((self.start_or_idx >> 1) as usize);
                end - start
            }
        }
    }
    pub fn offset(mut self, offset: Index) -> Self {
        match self.data {
            SpanData::Length(_) => self.start_or_idx += offset,
            SpanData::LargeSpan if self.start_or_idx & 1 == 1=>{
                let new_start= self.start_or_idx>>1 + offset;
                if new_start + offset <= 1<<31 -1{
                    self.start_or_idx = new_start
                }else {
                    self.start_or_idx = (INTERNER.lock().unwrap().add(new_start,new_start) as u32) << 1
                }

            }
            SpanData::LargeSpan=> {
                let mut inter = INTERNER.lock().unwrap();
                let (start,end) = inter.get((self.start_or_idx >> 1) as usize);
                self.start_or_idx = inter.add(start+offset,end+offset) as u32;
            }
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
            SpanData::LargeSpan if self.start_or_idx & 1 == 1=>{
                let new_start= self.start_or_idx>>1 - offset;
                if new_start + offset <= 1<<31 -1{
                    self.start_or_idx = new_start
                }else {
                    self.start_or_idx = (INTERNER.lock().unwrap().add(new_start,new_start) as u32) << 1
                }

            }
            SpanData::LargeSpan=> {
                let mut inter = INTERNER.lock().unwrap();
                let (start,end) = inter.get((self.start_or_idx >> 1) as usize);
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
