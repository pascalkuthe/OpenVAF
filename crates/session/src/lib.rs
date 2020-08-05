//! This crates uses black magic (macros and unsafe code) to create session data
//! Session data can be registered with the [`session_data`] macro.
//! This session data is only valid inside `openvaf_session` and will cause a panic otherwise

pub use sourcemap::SourceMap;
use std::cell::Cell;

use crate::__macro_exports::{Session, SESSION_DATA};
use std::ptr::NonNull;

#[doc(hidden)]
pub mod __macro_exports {
    pub use linkme::distributed_slice;
    pub use paste;
    use std::cell::Cell;
    use std::thread::LocalKey;

    pub type SessionInit = unsafe fn(&mut Session);
    pub type SessionCleanup = unsafe fn(usize);

    #[distributed_slice]
    pub static SESSION_DATA: [(SessionInit, SessionCleanup)] = [..];

    pub type Session = Vec<usize>;

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
        session.push(ptr as usize);
        thread_local.with(|cell|{
            assert_eq!(cell.get(),0,"Session Data was not deinitalized properly! This should be impossible if openvaf_session was used!");
            cell.set(ptr as usize)

        })
    }
}

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
            unsafe fn [<__openvaf_session_init_ $name>](session: &mut $crate::__macro_exports::Session){
                let data: Box<$ty> = Box::new($init);
                $crate::__macro_exports::set(&[<__ $name _data>], data, session)
            }

            #[doc(hidden)]
            unsafe fn [<__openvaf_session_cleanup_ $name>](ptr: usize){
                // Drop the data
                Box::from_raw(ptr as *mut $ty);
                [<__ $name _data>].with(|cell| cell.set(0));
            }


            $(#[$attrs:meta])* $vis fn [<with_ $name>]<X>(f: impl FnOnce(&$ty)->X)->X{
                unsafe{
                    $crate::__macro_exports::with(&[<__ $name _data>], f)
                }
            }

        }
    }
}

pub mod sourcemap;
pub mod symbols;

thread_local!(static SOURCEMAP_STORE: Cell<Option<NonNull<SourceMap>>> = Cell::new(None));

#[doc(hidden)]
pub unsafe fn init_sourcemap(sm: Box<SourceMap>) {
    SOURCEMAP_STORE.with(|cell| {
        let ptr = NonNull::new_unchecked(Box::into_raw(sm));
        let old = cell.replace(Some(ptr));
        assert!(old.is_none(), "Sourcemap was already set in this session!");
    })
}

pub fn with_sourcemap<T>(f: impl FnOnce(&SourceMap) -> T) -> T {
    let ptr = SOURCEMAP_STORE.with(|cell| cell.get());
    if let Some(ptr) = ptr {
        f(unsafe { ptr.as_ref() })
    } else {
        unreachable!("Sourcemap was used before initialization")
    }
}

/// # Safety
/// This function may not be called recrusively (this may cause a panic or leak memory)
pub fn openvaf_session<T>(session: impl FnOnce() -> T) -> T {
    let mut data = Session::with_capacity(SESSION_DATA.len());
    for (init, _) in SESSION_DATA.iter() {
        unsafe { init(&mut data) }
    }

    let res = session();

    for ((_, cleanup), ptr) in SESSION_DATA.iter().zip(data) {
        unsafe { cleanup(ptr) }
    }

    SOURCEMAP_STORE.with(|cell| {
        let ptr = cell.replace(None);
        if let Some(ptr) = ptr {
            // This is save since we just checked this proper pointer
            unsafe { Box::from_raw(ptr.as_ptr()) };
        }
    });

    res
}
