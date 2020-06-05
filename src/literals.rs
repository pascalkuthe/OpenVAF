use bitflags::_core::fmt::Formatter;
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use std::fmt;
use std::fmt::Display;
use std::ops::{Index, IndexMut, Range};

lazy_static! {
    static ref BUFFER: RwLock<StringLiteralBuffer> =
        RwLock::new(StringLiteralBuffer(String::with_capacity(8192)));
}

#[derive(Clone, Default, Debug)]
pub struct StringLiteralBuffer(pub String);

impl StringLiteralBuffer {
    pub fn get_str(&self, range: StringLiteral) -> &str {
        &self.0[range.as_usize_range()]
    }

    pub fn get_str_mut(&mut self, range: StringLiteral) -> &mut str {
        &mut self.0[range.as_usize_range()]
    }

    pub fn add_str_literal(&mut self, string: &str) -> Result<StringLiteral, ()> {
        if string.len() > u16::MAX as usize {
            Err(())
        } else {
            let start = self.0.len();
            self.0.push_str(string);
            Ok((start..self.0.len()).into())
        }
    }
}

impl Index<StringLiteral> for StringLiteralBuffer {
    type Output = str;

    fn index(&self, range: StringLiteral) -> &Self::Output {
        self.get_str(range)
    }
}

impl IndexMut<StringLiteral> for StringLiteralBuffer {
    fn index_mut(&mut self, range: StringLiteral) -> &mut Self::Output {
        self.get_str_mut(range)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct StringLiteral {
    pub start: u16,
    pub end: u16,
}

impl StringLiteral {
    pub fn new(src: &str) -> Result<Self, ()> {
        BUFFER.write().add_str_literal(src)
    }

    pub fn as_usize_range(self) -> Range<usize> {
        (self.start as usize)..(self.end as usize)
    }
    pub fn as_raw_range(self) -> Range<u16> {
        self.start..self.end
    }
    pub fn as_str<'lt>(self) -> MappedRwLockReadGuard<'lt, str> {
        RwLockReadGuard::map(BUFFER.read(), |buff| &buff[self])
    }

    pub const fn empty() -> Self {
        Self { start: 0, end: 0 }
    }
}

impl From<Range<u16>> for StringLiteral {
    fn from(range: Range<u16>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl From<Range<usize>> for StringLiteral {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start as u16,
            end: range.end as u16,
        }
    }
}

impl Into<Range<u16>> for StringLiteral {
    fn into(self) -> Range<u16> {
        self.start..self.end
    }
}

impl Into<Range<usize>> for StringLiteral {
    fn into(self) -> Range<usize> {
        self.as_usize_range()
    }
}

impl PartialEq for StringLiteral {
    fn eq(&self, other: &Self) -> bool {
        let buffer = BUFFER.read();
        buffer[*self] == buffer[*other]
    }
}

impl Eq for StringLiteral {}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&BUFFER.read()[*self])
    }
}
