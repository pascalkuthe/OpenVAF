//! A bit-set of `SyntaxKind`s.

use crate::lexer::RawToken;

/// A bit-set of `SyntaxKind`s
#[derive(Clone, Copy)]
pub(crate) struct TokenSet(u128);

impl TokenSet {
    pub(crate) const fn new(kinds: &[RawToken]) -> TokenSet {
        let mut res = 0u128;
        let mut i = 0;
        while i < kinds.len() {
            res |= mask(kinds[i]);
            i += 1
        }
        TokenSet(res)
    }

    pub(crate) const fn union(self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    pub(crate) const fn contains(&self, kind: RawToken) -> bool {
        (self.0 & mask(kind)) != 0
    }
}

const fn mask(kind: RawToken) -> u128 {
    1u128 << (kind as usize)
}
