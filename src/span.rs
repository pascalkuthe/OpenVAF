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
#[derive(Copy, Clone)]
pub struct Span {
    start_or_large_span_index: u32,
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
                start_or_large_span_index: start,
                data: SpanData::Length(len as Length),
            }
        } else {
            unimplemented!("Spans longer than {} aren't supported yet", std::u16::MAX)
        }
    }
    pub fn get_start(self) -> Index {
        match self.data {
            SpanData::LargeSpan => {
                unimplemented!("Spans longer than {} aren't supported yet", std::u16::MAX)
            }
            SpanData::Length(_) => self.start_or_large_span_index,
        }
    }
    pub fn get_end(self) -> Index {
        match self.data {
            SpanData::LargeSpan => {
                unimplemented!("Spans longer than {} aren't supported yet", std::u16::MAX)
            }
            SpanData::Length(length) => self.start_or_large_span_index + (length as u32),
        }
    }
    pub fn get_len(self) -> Index {
        match self.data {
            SpanData::LargeSpan => {
                unimplemented!("Spans longer than {} aren't supported yet", std::u16::MAX)
            }
            SpanData::Length(length) => length as Index,
        }
    }
    pub fn offset(mut self, offset: Index) -> Self {
        match self.data {
            SpanData::LargeSpan => {
                unimplemented!("Spans longer than {} aren't supported yet", std::u16::MAX)
            }
            SpanData::Length(_) => self.start_or_large_span_index += offset,
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
                unimplemented!("Spans longer than {} aren't supported yet", std::u16::MAX)
            }
            SpanData::Length(_) => self.start_or_large_span_index -= offset,
        }
        self
    }
    pub fn extend(mut self, to: Self) -> Self {
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
