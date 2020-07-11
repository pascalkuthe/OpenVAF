/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use core::fmt::Debug;
use core::fmt::{Display, Formatter};
macro_rules! unreachable_unchecked{
    ($reason:expr) => {
        if cfg!(debug_assertions){
            ::std::unreachable!($reason);
        }else{
            //if you call this macro the unsafety is your problem.. You should have good reason for doing so
            unsafe{::std::hint::unreachable_unchecked()};
        }
    };
}

pub fn format_list_with_seperator<C>(list: C, seperator: &'static str) -> ListFormatter<C> {
    ListFormatter(list, seperator, " or ")
}

pub fn format_list<C>(list: C) -> ListFormatter<C> {
    ListFormatter(list, "", " or ")
}

#[derive(Clone)]
pub struct ListFormatter<C>(pub C, pub &'static str, pub &'static str);

impl<C: Debug> Debug for ListFormatter<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl<'lt, T: Display> Display for ListFormatter<&'lt [T]> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.0 {
            [] => f.write_str(" "),
            [x] => {
                f.write_str(self.1)?;
                x.fmt(f)?;
                f.write_str(self.1)
            }
            [ref body @ .., second_last, last] => {
                for x in body {
                    f.write_str(self.1)?;
                    x.fmt(f)?;
                    f.write_str(self.1)?;
                    f.write_str(", ")?;
                }
                f.write_str(self.1)?;
                second_last.fmt(f)?;
                f.write_str(self.1)?;
                f.write_str(self.2)?;
                f.write_str(self.1)?;
                last.fmt(f)?;
                f.write_str(self.1)
            }
        }
    }
}

impl<T: Display> Display for ListFormatter<Vec<T>> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        ListFormatter(self.0.as_slice(), self.1, self.2).fmt(f)
    }
}

impl<'lt, T: Display + Clone> Display for ListFormatter<beef::Cow<'lt, [T]>> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&ListFormatter(self.0.as_ref(), self.1, self.2), f)
    }
}

impl<'lt, T: Display + Clone> Display for ListFormatter<beef::lean::Cow<'lt, [T]>> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&ListFormatter(self.0.as_ref(), self.1, self.2), f)
    }
}
