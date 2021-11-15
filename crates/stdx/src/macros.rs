//! Convenience macros.

#[macro_export]
macro_rules! eprintln {
    ($($tt:tt)*) => {{
        if $crate::is_ci() {
            panic!("Forgot to remove debug-print?")
        }
        std::eprintln!($($tt)*)
    }}
}

/// Appends formatted string to a `String`.
#[macro_export]
macro_rules! format_to {
    ($buf:expr) => ();
    ($buf:expr, $lit:literal $($arg:tt)*) => {
        { use ::std::fmt::Write as _; let _ = ::std::write!($buf, $lit $($arg)*); }
    };
}

/// Generates `From<Foo> for E` and `TryFrom<E> for Foo` impls for `Enum E { Foo(Foo), Bar(Bar) }` enums
///
/// # Example
///
/// ```rust
/// impl_from!(Struct, Union, Enum for Adt);
/// ```
#[macro_export]
macro_rules! impl_from {
    ($($variant:ident $(($($sub_variant:ident),*))?),* for $enum:ident) => {
        $(
            impl From<$variant> for $enum {
                fn from(it: $variant) -> $enum {
                    $enum::$variant(it)
                }
            }
            impl TryFrom<$enum> for $variant {
                type Error = ();

                fn try_from(it: $enum) -> Result<$variant,()> {
                    if let $enum::$variant(it) = it{
                        Ok(it)
                    }else{
                        Err(())
                    }
                }
            }
            $($(
                impl From<$sub_variant> for $enum {
                    fn from(it: $sub_variant) -> $enum {
                        $enum::$variant($variant::$sub_variant(it))
                    }
                }
                impl TryFrom<$enum> for $sub_variant {
                    type Error = ();

                    fn try_from(it: $enum) -> Result<$sub_variant,()> {
                        if let $enum::$variant($variant::$sub_variant(it)) = it{
                            Ok(it)
                        }else{
                            Err(())
                        }
                    }
                }
            )*)?
        )*
    }
}

/// Generates `From<Foo> for E` and `TryFrom<E> for Foo` impls for `Enum E { Foo(Foo), Bar(Bar) }` enums
///
/// # Example
///
/// ```rust
/// impl_from!(Struct, Union, Enum for Adt);
/// ```
#[macro_export]
macro_rules! impl_from_typed{
    ($($variant:ident ($ty:ty)),* for $enum:ident) => {
        $(
            impl From<$ty> for $enum {
                fn from(it: $ty) -> $enum {
                    $enum::$variant(it)
                }
            }
            impl TryFrom<$enum> for $ty {
                type Error = ();

                fn try_from(it: $enum) -> Result<$ty,()> {
                    if let $enum::$variant(it) = it{
                        Ok(it)
                    }else{
                        Err(())
                    }
                }
            }
        )*
    }
}

/// Generates `From<u32> for I`, `From<I> for u32` impls for `Enum E { Foo(Foo), Bar(Bar) }` enums
///
/// # Example
///
/// ```rust
/// impl_from!(Struct, Union, Enum for Adt);
/// ```
#[macro_export]
macro_rules! impl_idx_from {
    ($ty:ident($raw: ident)) => {
        impl From<$raw> for $ty {
            #[inline(always)]
            fn from(it: $raw) -> $ty {
                $ty(it)
            }
        }

        impl From<$ty> for $raw {
            #[inline(always)]
            fn from(it: $ty) -> $raw {
                it.0
            }
        }

        impl From<usize> for $ty {
            #[inline(always)]
            fn from(it: usize) -> $ty {
                ::std::debug_assert!(it < $raw::MAX as usize);
                $ty(it as $raw)
            }
        }

        impl From<$ty> for usize {
            #[inline(always)]
            fn from(it: $ty) -> usize {
                it.0 as usize
            }
        }
    };
}

/// Generates `From<u32> for I`, `From<I> for u32` impls for `Enum E { Foo(Foo), Bar(Bar) }` enums
///
/// # Example
///
/// ```rust
/// impl_from!(Struct, Union, Enum for Adt);
/// ```
#[macro_export]
macro_rules! impl_idx_from_readonly {
    ($ty:ident($raw: ident)) => {
        impl From<$ty> for $raw {
            #[inline(always)]
            fn from(it: $ty) -> $raw {
                it.0
            }
        }

        impl From<$ty> for usize {
            #[inline(always)]
            fn from(it: $ty) -> usize {
                it.0 as usize
            }
        }
    };
}

/// Generates `From<u32> for I`, `From<I> for u32` impls for `Enum E { Foo(Foo), Bar(Bar) }` enums
///
/// # Example
///
/// ```rust
/// impl_from!(Struct, Union, Enum for Adt);
/// ```
#[macro_export]
macro_rules! impl_idx_math {
    ($ty:ident($raw: ident)) => {
        impl std::ops::Add<$raw> for $ty {
            type Output = $ty;

            #[inline(always)]
            fn add(self, other: $raw) -> $ty {
                $ty(self.0 + other)
            }
        }

        impl std::ops::Add<usize> for $ty {
            type Output = $ty;

            #[inline(always)]
            fn add(self, other: usize) -> $ty {
                let res = self.0 as usize + other;
                debug_assert!(res <= $raw::MAX as usize);
                $ty(res as $raw)
            }
        }

        impl std::ops::Add for $ty {
            type Output = $ty;

            #[inline(always)]
            fn add(self, other: $ty) -> $ty {
                $ty(self.0 + other.0)
            }
        }

        impl std::ops::Add<$ty> for $raw {
            type Output = $ty;

            #[inline(always)]
            fn add(self, other: $ty) -> $ty {
                $ty(self + other.0)
            }
        }

        impl std::ops::Add<$ty> for usize {
            type Output = $ty;

            #[inline(always)]
            fn add(self, other: $ty) -> $ty {
                let res = self + other.0 as usize;
                debug_assert!(res <= $raw::MAX as usize);
                $ty(res as $raw)
            }
        }

        impl std::ops::AddAssign<$ty> for $ty {
            #[inline(always)]
            fn add_assign(&mut self, other: $ty) {
                self.0 += other.0
            }
        }

        impl std::ops::AddAssign<$raw> for $ty {
            #[inline(always)]
            fn add_assign(&mut self, other: $raw) {
                self.0 += other as $raw;
            }
        }

        impl std::ops::AddAssign<usize> for $ty {
            #[inline(always)]
            fn add_assign(&mut self, other: usize) {
                self.0 += other as $raw;
            }
        }

        impl std::ops::Sub<$raw> for $ty {
            type Output = $ty;

            #[inline(always)]
            fn sub(self, other: $raw) -> $ty {
                $ty(self.0 - other)
            }
        }

        impl std::ops::Sub<usize> for $ty {
            type Output = $ty;

            #[inline(always)]
            fn sub(self, other: usize) -> $ty {
                let res = self.0 as usize - other;
                debug_assert!(res <= $raw::MAX as usize);
                $ty(res as $raw)
            }
        }

        impl std::ops::Sub for $ty {
            type Output = $ty;

            #[inline(always)]
            fn sub(self, other: $ty) -> $ty {
                $ty(self.0 - other.0)
            }
        }

        impl std::ops::Sub<$ty> for $raw {
            type Output = $ty;

            #[inline(always)]
            fn sub(self, other: $ty) -> $ty {
                $ty(self - other.0)
            }
        }

        impl std::ops::Sub<$ty> for usize {
            type Output = $ty;

            #[inline(always)]
            fn sub(self, other: $ty) -> $ty {
                let res = self - other.0 as usize;
                debug_assert!(res <= $raw::MAX as usize);
                $ty(res as $raw)
            }
        }

        impl std::ops::SubAssign<$ty> for $ty {
            #[inline(always)]
            fn sub_assign(&mut self, other: $ty) {
                self.0 -= other.0
            }
        }

        impl std::ops::SubAssign<$raw> for $ty {
            #[inline(always)]
            fn sub_assign(&mut self, other: $raw) {
                self.0 -= other as $raw;
            }
        }

        impl std::ops::SubAssign<usize> for $ty {
            #[inline(always)]
            fn sub_assign(&mut self, other: usize) {
                self.0 -= other as $raw;
            }
        }
    };
}

/// Generates `From<u32> for I`, `From<I> for u32` impls for `Enum E { Foo(Foo), Bar(Bar) }` enums
///
/// # Example
///
/// ```rust
/// impl_from!(Struct, Union, Enum for Adt);
/// ```
#[macro_export]
macro_rules! impl_idx_math_from {
    ($ty:ident($raw: ident)) => {
        $crate::impl_idx_from!($ty($raw));
        $crate::impl_idx_math!($ty($raw));
    };
}

/// Generates an Display implementation
///
/// # Example
///
/// ```rust
/// impl_display! {
///     match Test{
///         Test::Bar(i) => "bar {}", i;
///         Test::Foo => "foo";
///         Test::Mew{x,y} => "mew {}, {}", x, y;
///     }
/// }
/// ```
#[macro_export]
macro_rules! impl_display {
    ( $($args: tt)*) => {
        $crate::impl_fmt!(Display $($args)*);
    };
}

/// Generates an Display implementation
///
/// # Example
///
/// ```rust
/// impl_display! {
///     match Test{
///         Test::Bar(i) => "bar {}", i;
///         Test::Foo => "foo";
///         Test::Mew{x,y} => "mew {}, {}", x, y;
///     }
/// }
/// ```
#[macro_export]
macro_rules! impl_debug {
    ( $($args: tt)*) => {
        $crate::impl_fmt!(Debug $($args)*);
    };
}

/// Generates an Display implementation
///
/// # Example
///
/// ```rust
/// impl_display! {
///     match Test{
///         Test::Bar(i) => "bar {}", i;
///         Test::Foo => "foo";
///         Test::Mew{x,y} => "mew {}, {}", x, y;
///     }
/// }
/// ```
#[macro_export]
macro_rules! impl_debug_display {
    ( $($args: tt)*) => {
        $crate::impl_fmt!(Debug $($args)*);
        $crate::impl_fmt!(Display $($args)*);
    };
}

/// Generates an implementation of the specified fmt Trait
///
/// # Example
///
/// ```rust
/// impl_display! {
///     match Test{
///         Test::Bar(i) => "bar {}", i;
///         Test::Foo => "foo";
///         Test::Mew{x,y} => "mew {}, {}", x, y;
///     }
/// }
/// ```
#[macro_export]
macro_rules! impl_fmt {
    (  $trait:ident  match $ty: ident{ $($variant: pat => $fmt:literal $(, $fmt_arg: expr)*;)*}) => {
        impl std::fmt::$trait for $ty{
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self{
                    $( $variant => write!(f, $fmt $(,$fmt_arg)*)),*
                }
            }
        }
    };


    (  $trait:ident $binding: ident @ $ty: ident => $fmt:literal $(, $fmt_arg: expr)*) => {
        impl std::fmt::$trait for $ty{
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let $binding = self;
                write!(f, $fmt $(,$fmt_arg)*)
            }
        }
    };
}
