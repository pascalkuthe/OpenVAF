use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_void};

extern "C" {
    fn lld_link(flavor: Flavor, argc: i32, argv: *const *const c_char) -> LldInvokeResult;
}

#[repr(C)]
pub struct LldInvokeResult {
    retcode: i32,
    messages: *const c_char,
}

impl Drop for LldInvokeResult {
    fn drop(&mut self) {
        if !self.messages.is_null() {
            unsafe { libc::free(self.messages as *const c_void as *mut c_void) }
        }
    }
}

pub struct Result {
    pub retcode: i32,
    pub messages: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub enum Flavor {
    Ld = 0,
    Ld64 = 1,
    Link = 2,
}

pub fn link(flavor: Flavor, args: &[String]) -> Result {
    let arg_allocs: Vec<_> = args.iter().map(|arg| CString::new(arg.as_bytes()).unwrap()).collect();
    let mut args_ = vec![b"lld\0".as_ptr() as *const c_char];
    args_.extend(arg_allocs.iter().map(|arg| arg.as_ptr()));
    let res = unsafe { lld_link(flavor, args_.len() as i32, args_.as_ptr()) };
    let messages = if res.messages.is_null() {
        String::new()
    } else {
        unsafe { CStr::from_ptr(res.messages).to_string_lossy().to_string() }
    };
    if res.retcode < 0 {
        panic!("lld crashed! Messages before crash:\n{messages}");
    }

    Result { retcode: res.retcode, messages }
}
