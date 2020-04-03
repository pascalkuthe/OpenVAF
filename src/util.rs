/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

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
