/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use bitflags::_core::fmt::{Display, Formatter};
macro_rules! unreachable_unchecked{
    ($reason:expr) => {
        if cfg!(debug_assertions){
            $crate::std::unreachable!($reason);
        }else{
            //if you call this macro the unsafety is your problem.. You should have good reason for doing so
            unsafe{$crate::std::hint::unreachable_unchecked()};
        }
    };
}

pub(crate) trait RefMut<T> {
    fn ref_mut(&mut self) -> Option<&mut T>;
}

impl<'t, T> RefMut<T> for Option<&'t mut T> {
    #[inline]
    fn ref_mut(&mut self) -> Option<&mut T> {
        self.as_mut().map(|x| &mut **x)
    }
}

pub struct VecFormatter<'lt, T: Display>(pub &'lt Vec<T>, pub &'lt str);

impl<'lt, T: Display> Display for VecFormatter<'lt, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.0.as_slice() {
            [] => f.write_str(" "),
            [x] => {
                f.write_str(self.1);
                x.fmt(f)?;
                f.write_str(self.1);
                Ok(())
            }
            [ref body @ .., second_last, last] => {
                for x in body {
                    f.write_str(self.1);
                    x.fmt(f)?;
                    f.write_str(self.1);
                    f.write_str(", ");
                }
                f.write_str(self.1);
                second_last.fmt(f)?;
                f.write_str(self.1);
                f.write_str(" or ");
                f.write_str(self.1);
                last.fmt(f)?;
                f.write_str(self.1);
                Ok(())
            }
        }
    }
}
