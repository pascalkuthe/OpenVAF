/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::iter;
pub use index_vec::*;
use std::ptr;

#[inline]
pub fn ensure_contains_elem<I: Idx, T>(
    vec: &mut IndexVec<I, T>,
    elem: I,
    fill_value: impl FnMut() -> T,
) {
    let min_new_len = elem.index() + 1;
    if vec.len() < min_new_len {
        vec.raw.resize_with(min_new_len, fill_value);
    }
}

pub trait IndexVecExtensions<I: Idx, T> {
    fn ensure_contains_elem(&mut self, elem: I, fill_value: impl FnMut() -> T);
}

pub trait IndexSliceExntesions<I: Idx, T> {
    /// Returns mutable references to two distinct elements, a and b. Panics if a == b.
    fn pick2_mut(&mut self, a: I, b: I) -> (&mut T, &mut T);

    /// Returns mutable references to three distinct elements or panics otherwise.
    fn pick3_mut(&mut self, a: I, b: I, c: I) -> (&mut T, &mut T, &mut T);

    fn pick_n_mut<const N: usize>(&mut self, indecies: [I; N]) -> [&mut T; N];
}

impl<I: Idx, T> IndexSliceExntesions<I, T> for IndexSlice<I, [T]> {
    #[inline]
    fn pick2_mut(&mut self, a: I, b: I) -> (&mut T, &mut T) {
        let (ai, bi) = (a.index(), b.index());
        assert_ne!(ai, bi);

        let len = self.len();
        assert!(ai < len && bi < len);
        let ptr = self.raw.as_mut_ptr();
        unsafe { (&mut *ptr.add(ai), &mut *ptr.add(bi)) }
    }

    #[inline]
    fn pick3_mut(&mut self, a: I, b: I, c: I) -> (&mut T, &mut T, &mut T) {
        let (ai, bi, ci) = (a.index(), b.index(), c.index());
        assert!(ai != bi && bi != ci && ci != ai);
        let len = self.raw.len();
        assert!(ai < len && bi < len && ci < len);
        let ptr = self.raw.as_mut_ptr();
        unsafe { (&mut *ptr.add(ai), &mut *ptr.add(bi), &mut *ptr.add(ci)) }
    }

    #[inline]
    fn pick_n_mut<const N: usize>(&mut self, indecies: [I; N]) -> [&mut T; N] {
        //safety check
        for idx1 in &indecies {
            assert!(*idx1 < self.len_idx());
            for idx2 in &indecies[..idx1.index()] {
                assert_ne!(idx1, idx2);
            }
            for idx2 in &indecies[(idx1.index() + 1..)] {
                assert_ne!(idx1, idx2);
            }
        }

        let mut res = [self.as_mut_ptr(); N];
        for (res, idx) in iter::zip(&mut res, &indecies) {
            // This is save we check that the offset are unique and inbounds before
            *res = unsafe { (*res).add(idx.index()) }
        }

        unsafe { ptr::read(&res as *const [*mut T; N] as *const [&mut T; N]) }
    }
}

impl<I: Idx, T> IndexVecExtensions<I, T> for IndexVec<I, T> {
    fn ensure_contains_elem(&mut self, elem: I, fill_value: impl FnMut() -> T) {
        let min_new_len = elem.index() + 1;
        if self.len() < min_new_len {
            self.raw.resize_with(min_new_len, fill_value);
        }
    }
}
