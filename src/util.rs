/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::compact_arena::SafeRange;

macro_rules! unreachable_unchecked{
    ($reason:expr) => {
        if cfg!(debug_assertions){
            $crate::std::unreachable!($reason);
        }else{
            unsafe{$crate::std::hint::unreachable_unchecked()};//if you call this macro the unsafety is your problem.. You should have good reason for doing so and the fact that this happens in production builds only makes this less bad tough
        }
    };
}
macro_rules! static_assert_size {
    ($ty:ty, $size:expr) => {
        const _: [(); $size] = [(); ::std::mem::size_of::<$ty>()];
    };
}
macro_rules! static_assert_size_eq {
    ($ty:ty, $ty2:ty) => {
        const _: [(); ::std::mem::size_of::<$ty>] = [(); ::std::mem::size_of::<$ty2>()];
    };
}
macro_rules! static_assert_align_eq {
    ($ty:ty, $ty2:ty) => {
        const _: [(); ::std::mem::align_of::<$ty>] = [(); ::std::mem::align_of::<$ty2>()];
    };
}
pub trait Push<T> {
    type Key;
    fn push(&mut self, value: T) -> Self::Key;
}
pub trait SafeRangeCreation<Key: Copy + Clone> {
    fn range_to_end(&self, from: Key) -> SafeRange<Key>;
    fn empty_range_from_end(&self) -> SafeRange<Key>;
    fn extend_range_to_end(&self, range: SafeRange<Key>) -> SafeRange<Key>;
    fn full_range(&self) -> SafeRange<Key>;
}
pub trait Step {
    unsafe fn step(&mut self);
}
