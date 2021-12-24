use llvm::{Type, Value};

use crate::CodegenCx;

impl<'a, 'll> CodegenCx<'a, 'll> {
    pub fn intrinsic(&mut self, name: &'static str) -> Option<(&'ll Type, &'ll Value)> {
        if let Some(res) = self.intrinsics.get(name) {
            return Some(*res);
        }

        macro_rules! ifn {
            ($name:expr, fn() -> $ret:expr) => (
                if name == $name {
                    return Some(self.insert_intrinsic($name, Some(&[]), $ret));
                }
            );
            ($name:expr, fn(...) -> $ret:expr) => (
                if name == $name {
                    return Some(self.insert_intrinsic($name, None, $ret));
                }
            );
            ($name:expr, fn($($arg:expr),*) -> $ret:expr) => (
                if name == $name {
                    return Some(self.insert_intrinsic($name, Some(&[$($arg),*]), $ret));
                }
            );
        }

        // let void = self.ty_void();
        let t_bool = self.ty_bool();
        let t_i32 = self.ty_int();
        let t_f64 = self.ty_real();
        let t_str = self.ty_str();

        ifn!("llvm.pow.f64", fn(t_f64, t_f64) -> t_f64);
        ifn!("llvm.sqrt.f64", fn(t_f64) -> t_f64);
        ifn!("llvm.sin.f64", fn(t_f64) -> t_f64);
        ifn!("llvm.cos.f64", fn(t_f64) -> t_f64);
        ifn!("llvm.exp.f64", fn(t_f64) -> t_f64);
        ifn!("llvm.log.f64", fn(t_f64) -> t_f64);
        ifn!("llvm.log10.f64", fn(t_f64) -> t_f64);
        ifn!("llvm.log2.f64", fn(t_f64) -> t_f64);
        ifn!("llvm.floor.f64", fn(t_f64) -> t_f64);

        // not technically intrinsics but part of the C standard library
        // TODO link custom mathmatical functions
        ifn!("tan", fn(t_f64) -> t_f64);
        ifn!("acos", fn(t_f64) -> t_f64);
        ifn!("asin", fn(t_f64) -> t_f64);
        ifn!("atan", fn(t_f64) -> t_f64);
        ifn!("atan2", fn(t_f64, t_f64) -> t_f64);
        ifn!("sqrt", fn(t_f64) -> t_f64);
        ifn!("cosh", fn(t_f64) -> t_f64);
        ifn!("sinh", fn(t_f64) -> t_f64);
        ifn!("tanh", fn(t_f64) -> t_f64);
        ifn!("acosh", fn(t_f64) -> t_f64);
        ifn!("asinh", fn(t_f64) -> t_f64);
        ifn!("atanh", fn(t_f64) -> t_f64);

        if name == "hypot" {
            let name = if self.target.options.is_like_msvc { "_hypot" } else { "hypot" };
            return Some(self.insert_intrinsic(name, Some(&[t_f64]), t_f64));
        }

        ifn!("strcmp", fn(t_str, t_str) -> t_bool);
        ifn!("llvm.llround.i32.f64", fn(t_f64) -> t_i32);

        // ifn!("llvm.lifetime.start.p0i8", fn(t_i64, i8p) -> void);
        // ifn!("llvm.lifetime.end.p0i8", fn(t_i64, i8p) -> void);

        // ifn!("llvm.expect.i1", fn(i1, i1) -> i1);
        // ifn!("llvm.eh.typeid.for", fn(i8p) -> t_i32);
        // ifn!("llvm.localescape", fn(...) -> void);
        // ifn!("llvm.localrecover", fn(i8p, i8p, t_i32) -> i8p);
        // ifn!("llvm.x86.seh.recoverfp", fn(i8p, i8p) -> i8p);

        // ifn!("llvm.assume", fn(i1) -> void);
        // ifn!("llvm.prefetch", fn(i8p, t_i32, t_i32, t_i32) -> void);

        // // This isn't an "LLVM intrinsic", but LLVM's optimization passes
        // // recognize it like one and we assume it exists in `core::slice::cmp`
        // ifn!("memcmp", fn(i8p, i8p, t_isize) -> t_i32);

        // // variadic intrinsics
        // ifn!("llvm.va_start", fn(i8p) -> void);
        // ifn!("llvm.va_end", fn(i8p) -> void);
        // ifn!("llvm.va_copy", fn(i8p, i8p) -> void);

        None
    }

    fn insert_intrinsic(
        &mut self,
        name: &'static str,
        args: Option<&[&'ll llvm::Type]>,
        ret: &'ll llvm::Type,
    ) -> (&'ll llvm::Type, &'ll llvm::Value) {
        let fn_ty = if let Some(args) = args {
            self.ty_func(args, ret)
        } else {
            self.ty_variadic_func(&[], ret)
        };
        let f = self.declare_ext_fn(name, fn_ty);
        self.intrinsics.insert(name, (fn_ty, f));
        (fn_ty, f)
    }
}
