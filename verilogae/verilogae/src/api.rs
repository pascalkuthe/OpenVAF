use std::ffi::{c_void, CStr};
use std::os::raw::c_char;
use std::panic::catch_unwind;
use std::{ptr, slice};

#[cfg(not(windows))]
use libloading::os::unix::Library;
#[cfg(windows)]
use libloading::os::windows::Library;

use crate::{export_vfs, load};

#[repr(C)]
#[derive(Default)]
pub struct Opts {
    pub model: Slice<u8>,
    pub cache_dir: Slice<u8>,
    pub include_dirs: Slice<Slice<u8>>,
    pub macro_flags: Slice<Slice<u8>>,
    pub allow_lints: Slice<Slice<u8>>,
    pub warn_lints: Slice<Slice<u8>>,
    pub deny_lints: Slice<Slice<u8>>,
    pub opt_lvl: OptLevel,
    pub target_cpu: Slice<u8>,
    pub target: Slice<u8>,
    pub cg_flags: Slice<Slice<u8>>,
    pub vfs: Vfs,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum OptLevel {
    None = 0,
    Less = 1,
    Default = 2,
    #[default]
    Aggressive = 3,
}

macro_rules! expose_ptrs {
    ($($mut: ident $name: ident: $ty:ty = $sym: literal;)*) => {
        $(
        #[doc = concat!("This function returns a pointer to the `", $sym, "` global")]
        /// of a VerilogAE model loaded with `load`.
        ///
        /// # Safety
        ///
        /// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
        #[no_mangle]
        pub unsafe extern "C" fn $name(lib: *const c_void) -> *$mut $ty {
            catch_unwind(||{
                let lib = Library::from_raw(lib as _);
                let res = access_ptr::<$ty>(&lib, $sym.as_bytes()) as _;
                // forget library so it doesn't get closed
                std::mem::forget(lib);
                res
            }).unwrap_or_else(|_|ptr::null::<$ty>() as _)
        }
    )*
    };
}

expose_ptrs! {
    const verilogae_functions: *const c_char = "functions";
    const verilogae_function_symbols: *const c_char = "functions.sym";
    const verilogae_opvars: *const c_char = "opvars";
    const verilogae_real_params: *const c_char = "params.real";
    const verilogae_real_param_units: *const c_char = "params.unit.real";
    const verilogae_real_param_descriptions: *const c_char = "params.desc.real";
    const verilogae_real_param_groups: *const c_char = "params.group.real";
    const verilogae_int_params: *const c_char = "params.integer";
    const verilogae_int_param_units: *const c_char = "params.unit.integer";
    const verilogae_int_param_descriptions: *const c_char = "params.desc.integer";
    const verilogae_int_param_groups: *const c_char = "params.group.integer";
    const verilogae_str_params: *const c_char = "params.string";
    const verilogae_str_param_units: *const c_char = "params.unit.string";
    const verilogae_str_param_descriptions: *const c_char = "params.desc.string";
    const verilogae_str_param_groups: *const c_char = "params.group.string";
    const verilogae_nodes: *const c_char = "nodes";
}

macro_rules! expose_consts{
    ($($name: ident: $ty:ty = $sym: literal;)*) => {
        $(
        #[doc = concat!("This function returns the value stored in the `", $sym, "` global")]
        /// of a VerilogAE model loaded with `load`.
        ///
        /// # Safety
        ///
        /// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
        #[no_mangle]
        pub unsafe extern "C" fn $name(lib: *const c_void) -> $ty {
            catch_unwind(||{
                let lib = Library::from_raw(lib as _);
                let res = access_val(&lib, $sym.as_bytes());
                // forget library so it doesn't get closed
                std::mem::forget(lib);
                res
            }).ok().unwrap_or_else(<$ty>::default)
        }
    )*
    };
}

pub type ParamFlags = u8;
pub type ModelcardInit = Option<
    extern "C" fn(
        *mut f64,
        *mut i32,
        *mut *const c_char,
        *mut f64,
        *mut i32,
        *mut f64,
        *mut i32,
        *mut ParamFlags,
    ),
>;

expose_consts! {
    verilogae_function_cnt: usize = "functions.cnt";
    verilogae_opvars_cnt: usize = "opvars.cnt";
    verilogae_real_param_cnt: usize = "params.real.cnt";
    verilogae_int_param_cnt: usize = "params.integer.cnt";
    verilogae_str_param_cnt: usize = "params.string.cnt";
    verilogae_node_cnt: usize = "nodes.cnt";
}

macro_rules! expose_named_ptrs {
    ($($mut: ident $name: ident: $ty:ty = $sym: literal;)*) => {
        $(
        #[doc = concat!("This function returns a pointer to the `", $sym, "` global")]
        /// of a VerilogAE model loaded with `load`.
        ///
        /// # Safety
        ///
        /// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
        #[doc = concat!("`sym_name` must batch the schema fun.{NUM}", $sym)]
        #[no_mangle]
        pub unsafe extern "C" fn $name(lib: *const c_void, fun: *const c_char) -> *$mut $ty {
            catch_unwind(||{
                let fun = CStr::from_ptr(fun);
                let mut sym_name = fun.to_bytes().to_vec();
                sym_name.push(b'.');
                sym_name.extend_from_slice($sym.as_bytes());
                sym_name.push(b'\0');
                let lib = Library::from_raw(lib as _);
                let res = access_ptr::<$ty>(&lib, &sym_name) as _;
                // forget library so it doesn't get closed
                std::mem::forget(lib);
                res
                }
            )
            .unwrap_or_else(|_| ptr::null::<$ty>() as _)
        }
    )*


    };
}

expose_named_ptrs! {
    const verilogae_real_fun_params: *const c_char = "params.real";
    const verilogae_int_fun_params: *const c_char = "params.integer";
    const verilogae_str_fun_params: *const c_char = "params.string";
    const verilogae_real_fun_depbreak: *const c_char = "depbreak.real";
    const verilogae_int_fun_depbreak: *const c_char = "depbreak.integer";
    const verilogae_fun_voltages: *const c_char = "voltages";
    const verilogae_fun_currents: *const c_char = "currents";
    const verilogae_fun_voltage_defaults: f64 = "voltages.default";
    const verilogae_fun_current_defaults: f64 = "currents.default";
}

macro_rules! expose_named_consts {
    ($($name: ident: $ty:ty = $sym: literal;)*) => {
        $(
        #[doc = concat!("This function returns a pointer to the `", $sym, "` global")]
        /// of a VerilogAE model loaded with `load`.
        ///
        /// # Safety
        ///
        /// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
        #[no_mangle]
        pub unsafe extern "C" fn $name(lib: *const c_void, fun: *const c_char) -> $ty {
            catch_unwind(||{
                let fun = CStr::from_ptr(fun);
                let mut sym_name = fun.to_bytes().to_vec();
                sym_name.push(b'.');
                sym_name.extend_from_slice($sym.as_bytes());
                sym_name.push(b'\0');
                let lib = Library::from_raw(lib as _);
                let res = access_val(&lib, &sym_name);
                // forget library so it doesn't get closed
                std::mem::forget(lib);
                res
                }
            )
            .ok().unwrap_or_else(<$ty>::default)
        }
    )*

    };
}

expose_named_consts! {
    verilogae_real_fun_param_cnt: usize = "params.real.cnt";
    verilogae_int_fun_param_cnt: usize = "params.integer.cnt";
    verilogae_str_fun_param_cnt: usize = "params.string.cnt";
    verilogae_real_fun_depbreak_cnt: usize = "depbreak.real.cnt";
    verilogae_int_fun_depbreak_cnt: usize = "depbreak.integer.cnt";
    verilogae_fun_voltage_cnt: usize = "voltages.cnt";
    verilogae_fun_current_cnt: usize = "currents.cnt";
    verilogae_fun_voltage_default_cnt: usize = "voltages.default.cnt";
    verilogae_fun_current_default_cnt: usize = "currents.default.cnt";
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union Meta<T: Copy> {
    pub stride: u64,
    pub scalar: T,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct FatPtr<T: Copy> {
    pub ptr: *mut T,
    pub meta: Meta<T>,
}

impl<T: Copy> FatPtr<T> {
    #[inline(always)]
    pub fn set_scalar(&mut self, val: T) {
        self.ptr = std::ptr::null_mut();
        self.meta.scalar = val
    }

    #[inline(always)]
    pub fn set_ptr(&mut self, ptr: *mut T, stride: isize) {
        self.ptr = ptr;
        self.meta.stride = stride as u64;
    }
}

pub type VaeFun = Option<
    extern "C" fn(
        usize,
        *mut FatPtr<f64>,
        *mut FatPtr<f64>,
        *mut FatPtr<f64>,
        *mut FatPtr<i32>,
        *mut *const c_char,
        *mut FatPtr<f64>,
        *mut FatPtr<i32>,
        *mut FatPtr<f64>,
        *mut c_void,
    ),
>;

/// Obtains a pointer to the modelcard initialization function of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
#[no_mangle]
pub unsafe extern "C" fn verilogae_init_modelcard(lib: *const c_void) -> ModelcardInit {
    catch_unwind(|| {
        let lib = Library::from_raw(lib as _);
        let res = match lib.get(b"init_modelcard\0") {
            Ok(val) => Some(*val),
            Err(err) => {
                eprintln!("error: failed to access init_modelcard\n\n{}", err);
                None
            }
        };
        // forget library so it doesn't get closed
        std::mem::forget(lib);
        res
    })
    .ok()
    .flatten()
}

/// Obtains a pointer to a model functions of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
#[no_mangle]
pub unsafe extern "C" fn verilogae_fun_ptr(lib: *const c_void, fun: *const c_char) -> VaeFun {
    catch_unwind(|| {
        let fun = CStr::from_ptr(fun);
        let lib = Library::from_raw(lib as _);
        let res = match lib.get(fun.to_bytes()) {
            Ok(val) => Some(*val),
            Err(err) => {
                eprintln!("error: failed to access {}\n\n{}", fun.to_string_lossy(), err);
                None
            }
        };
        // forget library so it doesn't get closed
        std::mem::forget(lib);
        res
    })
    .ok()
    .flatten()
}

/// # Safety
/// handle must be a valid model compiled with VerilogAE
#[no_mangle]
pub unsafe extern "C" fn verilogae_module_name(lib: *const c_void) -> *const c_char {
    catch_unwind(|| {
        let lib = Library::from_raw(lib as _);
        let res = match lib.get::<*const *const c_char>("module_name\0".as_bytes()) {
            Ok(val) => **val,
            Err(err) => {
                eprintln!("error: failed to access module_name\n\n{}", err);
                std::ptr::null()
            }
        };
        // forget library so it doesn't get closed
        std::mem::forget(lib);
        res
    })
    .unwrap_or(ptr::null())
}

/// # Safety
///
/// All required parameters must be initialized appropriately
#[no_mangle]
pub unsafe extern "C" fn verilogae_call_fun_parallel(
    fun: VaeFun,
    cnt: usize,
    voltages: *mut FatPtr<f64>,
    currents: *mut FatPtr<f64>,
    real_params: *mut FatPtr<f64>,
    int_params: *mut FatPtr<i32>,
    str_params: *mut *const c_char,
    real_dep_break: *mut FatPtr<f64>,
    int_dep_break: *mut FatPtr<i32>,
    temp: *mut FatPtr<f64>,
    out: *mut c_void,
) -> i32 {
    let fun = match fun {
        Some(fun) => fun,
        None => return -1,
    };

    // mark as as sync and send
    // this is technically not save In general but the compiled code is expected to perform only
    // valid operations

    #[derive(Copy, Clone)]
    struct PayLoad {
        voltages: *mut FatPtr<f64>,
        currents: *mut FatPtr<f64>,
        real_params: *mut FatPtr<f64>,
        int_params: *mut FatPtr<i32>,
        str_params: *mut *const c_char,
        real_dep_break: *mut FatPtr<f64>,
        int_dep_break: *mut FatPtr<i32>,
        temp: *mut FatPtr<f64>,
        out: *mut c_void,
    }

    unsafe impl Sync for PayLoad {}
    unsafe impl Send for PayLoad {}

    let payload = PayLoad {
        voltages,
        currents,
        real_params,
        int_params,
        real_dep_break,
        int_dep_break,
        str_params,
        temp,
        out,
    };

    rayon_core::scope(|s| {
        for i in 0..cnt {
            s.spawn(move |_| {
                let payload = payload;
                fun(
                    i,
                    payload.voltages,
                    payload.currents,
                    payload.real_params,
                    payload.int_params,
                    payload.str_params,
                    payload.real_dep_break,
                    payload.int_dep_break,
                    payload.temp,
                    payload.out,
                )
            })
        }
    });

    0
}

unsafe fn access_ptr<T>(lib: &Library, sym_name: &[u8]) -> *const T {
    match access_global(lib, sym_name) {
        Ok(val) => val,
        Err(err) => {
            eprintln!("error: failed to access {}\n\n{}", String::from_utf8_lossy(sym_name), err);
            ptr::null()
        }
    }
}

unsafe fn access_val<T: Copy + Default>(lib: &Library, sym_name: &[u8]) -> T {
    match access_global(lib, sym_name) {
        Ok(val) => *val,
        Err(err) => {
            eprintln!("error: failed to access {}\n\n{}", String::from_utf8_lossy(sym_name), err);
            T::default()
        }
    }
}

unsafe fn access_global<'a, T>(
    lib: &'a Library,
    sym_name: &[u8],
) -> Result<&'a T, libloading::Error> {
    lib.get(sym_name).map(|val| {
        let val: &'a T = *val;
        val
    })
}

#[no_mangle]
pub extern "C" fn verilogae_new_opts() -> *mut Opts {
    Box::into_raw(Box::default())
}

/// # Safety
/// `opts` must be a valid pointer created with `new_opts`
#[no_mangle]
pub unsafe extern "C" fn verilogae_free_opts(opts: *mut Opts) {
    drop(Box::from_raw(opts))
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Slice<T> {
    pub ptr: *mut T,
    pub len: usize,
}

impl<T> From<Box<[T]>> for Slice<T> {
    fn from(raw: Box<[T]>) -> Self {
        let len = raw.len();
        let ptr = Box::into_raw(raw);
        Self { ptr: ptr as *mut T, len }
    }
}

impl<T> Default for Slice<T> {
    fn default() -> Self {
        Self { ptr: ptr::null_mut(), len: 0 }
    }
}

impl<T> Slice<T> {
    pub fn from_raw_parts(ptr: *const T, len: usize) -> Self {
        Self { ptr: ptr as *mut T, len }
    }
    /// # Safety
    /// Pointer must be valid for reads
    /// Pointer must be allocated by rust
    pub unsafe fn into_box(self) -> Box<[T]> {
        let raw = std::ptr::slice_from_raw_parts_mut(self.ptr, self.len);
        Box::from_raw(raw)
    }

    /// # Safety
    /// Pointer must be valid for reads or null
    /// Pointer must be allocated by rust
    pub unsafe fn into_box_opt(self) -> Option<Box<[T]>> {
        if self.ptr.is_null() {
            None
        } else {
            Some(self.into_box())
        }
    }

    /// # Safety
    /// Pointer must be valid for reads
    pub unsafe fn read(&self) -> &[T] {
        slice::from_raw_parts(self.ptr, self.len)
    }
}

impl<T: 'static> From<&'_ [T]> for Slice<T> {
    fn from(src: &'_ [T]) -> Self {
        Self { ptr: src.as_ptr() as *mut T, len: src.len() }
    }
}

impl From<Box<str>> for Slice<u8> {
    fn from(raw: Box<str>) -> Self {
        let len = raw.len();
        let ptr = Box::into_raw(raw) as *mut [u8] as *mut u8;
        Self { ptr, len }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct VfsEntry {
    pub name: Slice<u8>,
    pub data: Slice<u8>,
}

pub type Vfs = Slice<VfsEntry>;

/// # Safety
/// * path must be valid for reads
/// * opts must be valid for reads or null
/// * opts must only contain valid data
#[no_mangle]
pub unsafe extern "C" fn verilogae_export_vfs(path: Slice<u8>, opts: *mut Opts) -> Vfs {
    let path = path.to_path();
    let opts_;

    let opts = if opts.is_null() {
        opts_ = Opts::default();
        &opts_
    } else {
        &*opts
    };

    let res = std::panic::catch_unwind(|| export_vfs(&path, opts));

    if let Ok(res) = res {
        match res {
            Ok(vfs_export) => return vfs_export.into(),
            Err(err) => eprintln!("{:?}", err),
        }
    }

    Slice { ptr: ptr::null_mut(), len: 0 }
}

/// # Safety
/// * path must be valid for reads
/// * opts must be valid for reads or null
/// * opts must only contain valid data
#[no_mangle]
pub unsafe extern "C" fn verilogae_free_vfs(vfs: Vfs) {
    if vfs.ptr.is_null() {
        return;
    }
    let vfs = vfs.into_box().into_vec();
    for entry in vfs {
        entry.name.into_box();
        entry.data.into_box();
    }
}

/// # Safety
/// * path must be valid for reads
/// * opts must be valid for reads or null
/// * opts must only contain valid data
#[no_mangle]
pub unsafe extern "C" fn verilogae_load(
    path: Slice<u8>,
    full_compile: bool,
    opts: *const Opts,
) -> *const c_void {
    let path = path.to_path();
    let opts_;

    let opts = if opts.is_null() {
        opts_ = Opts::default();
        &opts_
    } else {
        &*opts
    };

    let res = std::panic::catch_unwind(|| load(&path, full_compile, opts));

    if let Ok(res) = res {
        match res {
            Ok(lib) => return lib.into_raw() as *const c_void,
            Err(err) => eprintln!("{:?}", err),
        }
    }
    ptr::null()
}
