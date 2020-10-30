/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

//! This crates uses black magic (macros and unsafe code) to create session data
//! Session data can be registered with the [`session_data`] macro.
//! This session data is only valid inside `openvaf_session` and will cause a panic otherwise
//! It is technically UNDEFINED BEHAVIOUR if the session is dropped while data is being read (aka moving the session into a with method) in practice nobody would get that Idea I hope

pub use sourcemap::SourceMap;
use std::cell::Cell;

use crate::__macro_exports::SESSION_DATA;
use cfg_if::cfg_if;
use std::mem::take;
use std::ptr::NonNull;
use tracing::Span;
use tracing::{debug, trace_span};

#[doc(hidden)]
pub mod __macro_exports {
    use crate::Session;
    use cfg_if::cfg_if;
    pub use linkme::distributed_slice;
    pub use paste;
    use std::cell::Cell;
    use std::thread::LocalKey;

    pub type SessionInit = unsafe fn(&mut Session);
    pub type SessionCleanup = unsafe fn(usize);

    #[distributed_slice]
    pub static SESSION_DATA: [(SessionInit, SessionCleanup)] = [..];

    cfg_if! {
        if #[cfg(feature = "global_session")]{
            #[doc(hidden)]
            pub unsafe fn with<X, T>(ptr: usize, f: impl FnOnce(&T) -> X) -> X {
                assert_ne!(
                    ptr, 0,
                    "Session Data was not initalized (either not registered or access outside session)"
                );
                let ptr = ptr as *const T;
                let val = &*ptr;
                f(val)
            }

            #[doc(hidden)]
            pub unsafe fn set<T>(location: &mut usize, val: Box<T>, session: &mut Session) {
                assert_eq!(*location,0,"Session Data was not deinitalized properly! This should be impossible if openvaf_session was used!");
                let ptr = Box::into_raw(val);
                *location = ptr as usize;
                session.0.push(ptr as usize);
            }
        }else{
            #[doc(hidden)]
            pub unsafe fn with<X, T>(
                thread_local: &'static LocalKey<Cell<usize>>,
                f: impl FnOnce(&T) -> X,
            ) -> X {
                thread_local.with(|ptr| {
                    let ptr = ptr.get();
                    assert_ne!(
                        ptr, 0,
                        "Session Data was not initalized (either not registered or access outside session)"
                    );
                    let ptr = ptr as *const T;
                    let val = &*ptr;
                    f(val)
                })
            }

            #[doc(hidden)]
            pub unsafe fn set<T>(
                thread_local: &'static LocalKey<Cell<usize>>,
                val: Box<T>,
                session: &mut Session,
            ) {
                let ptr = Box::into_raw(val);
                session.0.push(ptr as usize);
                thread_local.with(|cell|{
                    assert_eq!(cell.get(),0,"Session Data was not deinitalized properly! This should be impossible if openvaf_session was used!");
                    cell.set(ptr as usize)

                })
            }
        }
    }
}

cfg_if! {
        if #[cfg(feature = "global_session")]{
            /// Session data is a thread local that can only be accessed inside a [session](crate::openvaf_session)
            /// This macro generates a function `with_$name(f: impl FnOnce(&$ty))` that can be used to access the thread local.
            /// Note that this function will panic when accessed outside a thread
            #[macro_export]
            macro_rules! session_data {
                ($(#[$attrs:meta])* $vis:vis static $name:ident: $ty:ty = $init: expr) => {

                    $crate::__macro_exports::paste::item! {

                        #[doc(hidden)]
                        #[allow(non_upper_case_globals)]
                        #[$crate::__macro_exports::distributed_slice($crate::__macro_exports::SESSION_DATA)]
                        static [<__ $name _session>]: ($crate::__macro_exports::SessionInit, $crate::__macro_exports::SessionCleanup) = ([<__openvaf_session_init_ $name>],[<__openvaf_session_cleanup_ $name>]);

                        #[doc(hidden)] #[allow(non_upper_case_globals)] static mut [<__ $name _data>]: usize = 0;


                        #[doc(hidden)]
                        unsafe fn [<__openvaf_session_init_ $name>](session: &mut $crate::Session){
                            let data: Box<$ty> = Box::new($init);
                            $crate::__macro_exports::set(&mut [<__ $name _data>], data, session)
                        }

                        #[doc(hidden)]
                        unsafe fn [<__openvaf_session_cleanup_ $name>](ptr: usize){
                            // Drop the data
                            // This is save since we use an atomic to check that only one session may run at the same time
                            Box::from_raw(ptr as *mut $ty);
                            [<__ $name _data>] = 0;
                        }


                        /// # Safety
                        /// It is undefined behaviour to move the sessions instance into the closure
                        $(#[$attrs:meta])* $vis fn [<with_ $name>]<X>(f: impl FnOnce(&$ty)->X)->X{
                            unsafe{
                                $crate::__macro_exports::with([<__ $name _data>], f)
                            }
                        }

                    }
                }
            }


            static GLOBAL_SESSION_LOCK: ::std::sync::atomic::AtomicBool =
                ::std::sync::atomic::AtomicBool::new(false);

            static GLOBAL_SOURCEMAP_LOCK: ::std::sync::atomic::AtomicBool =
                ::std::sync::atomic::AtomicBool::new(false);

            static mut SOURCEMAP_STORE: Option<NonNull<SourceMap>> = None;

            #[doc(hidden)]
            pub unsafe fn init_sourcemap(sm: Box<SourceMap>) {
                debug!("Sourcemap init");
                #[cfg(feature = "global_session")]
                if GLOBAL_SOURCEMAP_LOCK.swap(true, ::std::sync::atomic::Ordering::Relaxed) {
                    unreachable!("Sourcemap was already set in this session!")
                }
                let ptr = NonNull::new_unchecked(Box::into_raw(sm));
                assert!(SOURCEMAP_STORE.is_none(), "Sourcemap was already set in this session!");
                SOURCEMAP_STORE = Some(ptr);
            }

            /// # Safety
            /// It is undefined behaviour to move the session instance into the closure
            pub fn with_sourcemap<T>(f: impl FnOnce(&SourceMap) -> T) -> T {
                unsafe {
                    if let Some(ptr) = SOURCEMAP_STORE {
                        f(ptr.as_ref())
                    } else {
                        unreachable!("Sourcemap was used before initialization")
                    }
                }
            }

            pub(crate) fn cleanup_sourcemap(){
                unsafe {
                    if let Some(ptr) = SOURCEMAP_STORE.take() {
                        // This is save since we just checked this proper pointer
                        Box::from_raw(ptr.as_ptr());
                    }
                }
            }

        }else{
            /// Session data is a thread local that can only be accessed inside a [session](crate::openvaf_session)
            /// This macro generates a function `with_$name(f: impl FnOnce(&$ty))` that can be used to access the thread local.
            /// Note that this function will panic when accessed outside a thread
            #[macro_export]
            macro_rules! session_data {
                ($(#[$attrs:meta])* $vis:vis static $name:ident: $ty:ty = $init: expr) => {

                    $crate::__macro_exports::paste::item! {

                        #[doc(hidden)]
                        #[allow(non_upper_case_globals)]
                        #[$crate::__macro_exports::distributed_slice($crate::__macro_exports::SESSION_DATA)]
                        static  [<__ $name _session>]: ($crate::__macro_exports::SessionInit, $crate::__macro_exports::SessionCleanup) = ([<__openvaf_session_init_ $name>],[<__openvaf_session_cleanup_ $name>]);

                        thread_local!(#[doc(hidden)] #[allow(non_upper_case_globals)] static [<__ $name _data>]: ::std::cell::Cell<usize> = ::std::cell::Cell::new(0));


                        #[doc(hidden)]
                        unsafe fn [<__openvaf_session_init_ $name>](session: &mut $crate::Session){
                            let data: Box<$ty> = Box::new($init);
                            $crate::__macro_exports::set(&[<__ $name _data>], data, session)
                        }

                        #[doc(hidden)]
                        unsafe fn [<__openvaf_session_cleanup_ $name>](ptr: usize){
                            // Drop the data
                            Box::from_raw(ptr as *mut $ty);
                            [<__ $name _data>].with(|cell| cell.set(0));
                        }


                        /// # Safety
                        /// It is undefined behaviour to move the session instance into the closure
                        $(#[$attrs:meta])* $vis fn [<with_ $name>]<X>(f: impl FnOnce(&$ty)->X)->X{
                            unsafe{
                                $crate::__macro_exports::with(&[<__ $name _data>], f)
                            }
                        }

                    }
                }
            }

            thread_local!(static SOURCEMAP_STORE: Cell<Option<NonNull<SourceMap>>> = Cell::new(None));

            #[doc(hidden)]
            pub unsafe fn init_sourcemap(sm: Box<SourceMap>) {
                debug!("Sourcemap init");
                SOURCEMAP_STORE.with(|cell| {
                    let ptr = NonNull::new_unchecked(Box::into_raw(sm));
                    let old = cell.replace(Some(ptr));
                    assert!(old.is_none(), "Sourcemap was already set in this session!");
                })
            }

            /// # Safety
            /// It is undefined behaviour to move the session instance into the closure
            pub fn with_sourcemap<T>(f: impl FnOnce(&SourceMap) -> T) -> T {
                let ptr = SOURCEMAP_STORE.with(|cell| cell.get());
                if let Some(ptr) = ptr {
                    f(unsafe { ptr.as_ref() })
                } else {
                    unreachable!("Sourcemap was used before initialization")
                }
            }

            pub (crate) fn cleanup_sourcemap(){
                SOURCEMAP_STORE.with(|cell| {
                    let ptr = cell.replace(None);
                    if let Some(ptr) = ptr {
                        // This is save since we just checked this proper pointer
                        unsafe { Box::from_raw(ptr.as_ptr()) };
                    }
                });
            }
        }
}

pub mod sourcemap;
pub mod symbols;

// Soundness: PhantomData for DropCheck is not required
pub struct Session(pub(crate) Vec<usize>, pub(crate) Span);

impl Session {
    pub fn new() -> Self {
        let span = trace_span!("OpenVaf session");
        span.with_subscriber(|(id, dispatch)| dispatch.enter(id));

        #[cfg(feature = "global_session")]
        if GLOBAL_SESSION_LOCK.swap(true, ::std::sync::atomic::Ordering::Relaxed) {
            unreachable!("Only one OpenVAF session may be active at the same time when the \"globa_session\" feature gate is active")
        }

        let mut res = Self(Vec::with_capacity(SESSION_DATA.len()), span);
        for (init, _) in SESSION_DATA.iter() {
            unsafe { init(&mut res) }
        }
        res
    }

    pub fn run<T>(&self, f: impl FnOnce() -> T) -> T {
        f()
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        let cleanup_span = trace_span!("Session cleanup");
        let enter = cleanup_span.enter();
        for ((_, cleanup), ptr) in SESSION_DATA.iter().zip(take(&mut self.0)) {
            unsafe { cleanup(ptr) }
        }

        cleanup_sourcemap();

        #[cfg(feature = "global_session")]
        GLOBAL_SESSION_LOCK.store(false, ::std::sync::atomic::Ordering::Relaxed);

        #[cfg(feature = "global_session")]
        GLOBAL_SOURCEMAP_LOCK.store(false, ::std::sync::atomic::Ordering::Relaxed);

        drop(enter);
        self.1.with_subscriber(|(id, dispatch)| dispatch.exit(id));
    }
}
