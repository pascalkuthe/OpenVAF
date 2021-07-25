/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use std::mem::MaybeUninit;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
enum AtMostTwo {
    Zero = 0,
    One = 1,
    Two = 2,
}

impl AtMostTwo {
    fn decriment(self) -> Option<Self> {
        match self {
            Self::Zero => None,
            Self::One => Some(Self::Zero),
            Self::Two => Some(Self::One),
        }
    }
}

#[derive(Debug)]
pub struct AtMostTwoIter<T> {
    data: [MaybeUninit<T>; 2],
    len: AtMostTwo,
}

impl<T: Clone> Clone for AtMostTwoIter<T> {
    fn clone(&self) -> Self {
        unsafe {
            match self.len {
                AtMostTwo::Zero => Self::new_empty(),
                AtMostTwo::One => Self::new_single((&*self.data[0].as_ptr()).clone()),
                AtMostTwo::Two => Self::new_double(
                    (&*self.data[0].as_ptr()).clone(),
                    (&*self.data[1].as_ptr()).clone(),
                ),
            }
        }
    }
}

impl<T: Copy + Clone> Copy for AtMostTwoIter<T> {}

impl<T> AtMostTwoIter<T> {
    #[inline]
    #[must_use]
    pub const fn new_empty() -> Self {
        Self { data: [MaybeUninit::uninit(), MaybeUninit::uninit()], len: AtMostTwo::Zero }
    }

    #[inline]
    #[must_use]
    pub const fn new_single(val: T) -> Self {
        Self { data: [MaybeUninit::new(val), MaybeUninit::uninit()], len: AtMostTwo::One }
    }

    #[inline]
    #[must_use]
    pub const fn new_double(val1: T, val2: T) -> Self {
        Self { data: [MaybeUninit::new(val1), MaybeUninit::new(val2)], len: AtMostTwo::Two }
    }
}

impl<T> Iterator for AtMostTwoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let len = self.len.decriment()?;
        self.len = len;
        Some(unsafe { std::ptr::read(self.data[len as usize].as_ptr()) })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len as usize, Some(self.len as usize))
    }
}

impl<T: Clone + Copy> ExactSizeIterator for AtMostTwoIter<T> {
    fn len(&self) -> usize {
        self.len as usize
    }
}
