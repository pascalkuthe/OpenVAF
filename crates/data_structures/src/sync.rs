/*
    * ******************************************************************************************
    * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
    * It is subject to the license terms in the LICENSE file found in the top-level directory
    *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
    *  No part of frontend, including this file, may be copied, modified, propagated, or
    *  distributed except according to the terms contained in the LICENSE file.
    * *****************************************************************************************

    Adapted from https://github.com/rust-lang/rust src/librustc_data_structures/work_queue.rs under MIT-License

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
    ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
    TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
    PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
    SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
    IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
*/

//! This module defines types which are thread safe if cfg!(feature = threading) is true.
//! `Lrc` is an alias of `Arc` if the `threading` feature is enable, `Rc` otherwise.
//!
//! `Lock` is a mutex.
//! It internally uses `parking_lot::Mutex` if the `threading` feature is enable,
//! `RefCell` otherwise.
//!
//! `RwLock` is a read-write lock.
//! It internally uses `parking_lot::RwLock` if the `threading` feature is enable,
//! `RefCell` otherwise.
//!
//! `
//!
//! Currently this module serves little purpose as frontend does currently not use multithreading
//! The only use is multithreaded calls to frontend and providing an abstraction that allows multithreading in the future

use cfg_if::cfg_if;

#[derive(Debug)]
pub struct Lock<T>(InnerLock<T>);

impl<T> Lock<T> {
    #[inline(always)]
    pub fn into_inner(self) -> T {
        self.0.into_inner()
    }

    #[inline(always)]
    pub fn get_mut(&mut self) -> &mut T {
        self.0.get_mut()
    }

    #[inline(always)]
    pub fn with_lock<F: FnOnce(&mut T) -> R, R>(&self, f: F) -> R {
        f(&mut *self.lock())
    }

    #[inline(always)]
    pub fn borrow(&self) -> LockGuard<'_, T> {
        self.lock()
    }

    #[inline(always)]
    pub fn borrow_mut(&self) -> LockGuard<'_, T> {
        self.lock()
    }
}

impl<T: Default> Default for Lock<T> {
    #[inline]
    fn default() -> Self {
        Lock::new(T::default())
    }
}

// FIXME: Probably a bad idea
impl<T: Clone> Clone for Lock<T> {
    #[inline]
    fn clone(&self) -> Self {
        Lock::new(self.borrow().clone())
    }
}

#[derive(Debug)]
pub struct RwLock<T>(InnerRwLock<T>);

impl<T> RwLock<T> {
    #[inline(always)]
    pub fn into_inner(self) -> T {
        self.0.into_inner()
    }

    #[inline(always)]
    pub fn get_mut(&mut self) -> &mut T {
        self.0.get_mut()
    }

    #[inline(always)]
    pub fn with_read_lock<F: FnOnce(&T) -> R, R>(&self, f: F) -> R {
        f(&*self.read())
    }

    #[inline(always)]
    pub fn with_write_lock<F: FnOnce(&mut T) -> R, R>(&self, f: F) -> R {
        f(&mut *self.write())
    }

    #[inline(always)]
    pub fn borrow(&self) -> ReadGuard<'_, T> {
        self.read()
    }

    #[inline(always)]
    pub fn borrow_mut(&self) -> WriteGuard<'_, T> {
        self.write()
    }
}

cfg_if! {

    if #[cfg(feature = "threading")]{

        use parking_lot::Mutex as InnerLock;
        use parking_lot::RwLock as InnerRwLock;
        pub use std::sync::Arc as Lrc;
        pub use parking_lot::RwLockReadGuard as ReadGuard;
        pub use parking_lot::MappedRwLockReadGuard as MappedReadGuard;
        pub use parking_lot::RwLockWriteGuard as WriteGuard;
        pub use parking_lot::MappedRwLockWriteGuard as MappedWriteGuard;

        pub use parking_lot::MutexGuard as LockGuard;
        pub use parking_lot::MappedMutexGuard as MappedLockGuard;

        pub use once_cell::sync::OnceCell;

        impl<T> Lock<T>{

            #[inline(always)]
            pub const fn new(inner: T) -> Self {
                Self(parking_lot::const_mutex(inner))
            }

            #[inline(always)]
            pub fn try_lock(&self) -> Option<LockGuard<'_, T>> {
                self.0.try_lock()
            }

            #[inline(always)]
            pub fn lock(&self) -> LockGuard<'_, T> {
                self.0.lock()
            }
        }


        impl<T> RwLock<T> {

            #[inline(always)]
            pub const fn new(inner: T) -> Self {
                Self(parking_lot::const_rwlock(inner))
            }

            #[inline(always)]
            pub fn read(&self) -> ReadGuard<'_, T> {
                self.0.read()
            }

            #[inline(always)]
            pub fn try_write(&self) -> Result<WriteGuard<'_, T>, ()> {
                self.0.try_write().ok_or(())
            }

            #[inline(always)]
            pub fn write(&self) -> WriteGuard<'_, T> {
                self.0.write()
            }

        }

    }else{
        use core::cell::RefCell;
        use std::cell::RefCell as InnerLock;
        use std::cell::RefCell as InnerRwLock;
        pub use std::rc::Rc as Lrc;
        pub use std::rc::Weak as Weak;
        pub use std::cell::Ref as ReadGuard;
        pub use std::cell::Ref as MappedReadGuard;
        pub use std::cell::RefMut as WriteGuard;
        pub use std::cell::RefMut as MappedWriteGuard;
        pub use std::cell::RefMut as LockGuard;
        pub use std::cell::RefMut as MappedLockGuard;
        pub use once_cell::unsync::OnceCell;

        impl<T> Lock<T>{

            #[inline(always)]
            pub const fn new(inner: T) -> Self {
                Self(RefCell::new(inner))
            }

            #[inline(always)]
            pub fn try_lock(&self) -> Option<LockGuard<'_, T>> {
                self.0.try_borrow_mut().ok()
            }

            #[inline(always)]
            pub fn lock(&self) -> LockGuard<'_, T> {
                self.0.borrow_mut()
            }
        }

        impl<T> RwLock<T> {

            #[inline(always)]
            pub const fn new(inner: T) -> Self {
                Self(RefCell::new(inner))
            }

            #[inline(always)]
            pub fn read(&self) -> ReadGuard<'_, T> {
                self.0.borrow()
            }

            #[inline(always)]
            pub fn try_write(&self) -> Result<WriteGuard<'_, T>, ()> {
                self.0.try_borrow_mut().map_err(|_| ())
            }

            #[inline(always)]
            pub fn write(&self) -> WriteGuard<'_, T> {
                self.0.borrow_mut()
            }

        }

    }
}
