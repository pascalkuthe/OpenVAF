//! A bit-set of `SyntaxKind`s.

use std::convert::TryInto;
use std::u128;

use crate::SyntaxKind;

/// A bit-set of `SyntaxKind`s
#[derive(Clone, Copy)]
pub(crate) struct TokenSet(u128);

impl TokenSet {
    pub(crate) const EMPTY: TokenSet = TokenSet(0);

    // pub fn from_raw(raw: u128)->Self{
    //     Self(raw)
    // }

    // pub fn to_raw(self)->u128{
    //     self.0
    // }

    pub(crate) const fn new(kinds: &[SyntaxKind]) -> TokenSet {
        let mut res = 0u128;
        let mut i = 0;
        while i < kinds.len() {
            res |= mask(kinds[i]);
            i += 1
        }
        TokenSet(res)
    }

    pub(crate) const fn unique(kind: SyntaxKind) -> TokenSet {
        Self::new(&[kind])
    }

    pub(crate) const fn union(self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    pub(crate) const fn contains(&self, kind: SyntaxKind) -> bool {
        self.0 & mask(kind) != 0
    }

    pub(crate) const fn iter(&self) -> TokenSetIter {
        TokenSetIter(self.0)
    }
}

const fn mask(kind: SyntaxKind) -> u128 {
    1u128 << (kind as usize)
}

pub(crate) struct TokenSetIter(u128);

impl Iterator for TokenSetIter {
    type Item = SyntaxKind;
    fn next(&mut self) -> Option<SyntaxKind> {
        if self.0 != 0 {
            let bit_pos: u16 = self.0.trailing_zeros().try_into().unwrap();
            let bit = 1 << bit_pos;
            self.0 ^= bit;
            Some(SyntaxKind::from(bit_pos))
        } else {
            None
        }
    }
}
