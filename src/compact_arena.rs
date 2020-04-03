/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

/*This is an adaption of the compact arena crate https://github.com/llogiq/compact_arena (mk_tiny_arena!) under MIT-License

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

use core::fmt::{Debug, Display, Formatter, Result as FmtResult};
use core::marker::PhantomData;
use core::mem::{self, MaybeUninit};
use core::ops::SubAssign;
use core::ops::{AddAssign, Sub};
use core::ops::{Index, IndexMut};
use core::ptr;
use std::error::Error;
use std::hash::{Hash, Hasher};
use std::ops::Range;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SafeRange<T: Copy> {
    start: T,
    end: T,
}

/*impl<T: Copy> Into<Range<T>> for SafeRange<T> {
    fn into(self) -> Range<T> {
        std::ops::Range {
            start: self.start,
            end: self.end,
        }
    }
}*/

impl<T: Copy> From<Range<T>> for SafeRange<T> {
    fn from(org: Range<T>) -> Self {
        Self {
            start: org.start,
            end: org.end,
        }
    }
}

impl<'tag, T: Copy + Clone> From<SafeRange<Idx<'tag, T>>> for SafeRange<T> {
    fn from(other: SafeRange<Idx<'tag, T>>) -> Self {
        Self {
            start: other.start.index,
            end: other.end.index,
        }
    }
}

impl<T: Copy + Step + PartialOrd> Iterator for SafeRange<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start < self.end {
            //Range is inclusive of the end
            let res = Some(self.start);
            unsafe { self.start.step() };
            res
        } else {
            None
        }
    }
}

impl<T: Copy + Step + PartialOrd> DoubleEndedIterator for SafeRange<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.start < self.end {
            unsafe { self.end.step_back() };
            Some(self.end)
        } else {
            None
        }
    }
}

impl<T: Copy + Clone> SafeRange<T> {
    /// You should never call this! This is here to allow access in macros
    pub(crate) unsafe fn get_end(&self) -> T {
        self.end
    }
    /// You should never call this! This is here to allow access in macros
    pub(crate) unsafe fn get_start(&self) -> T {
        self.start
    }
    pub(crate) fn new(start: T, end: T) -> Self {
        Self { start, end }
    }
    pub fn set_end(&mut self, end: T) {
        self.end = end;
    }
    pub fn set_start(&mut self, start: T) {
        self.end = start;
    }
}

impl<'tag, T: Copy + Clone + Sub<Output = T>> SafeRange<Idx<'tag, T>> {
    pub fn len(self) -> T {
        self.end.distance(self.start)
    }
}

impl<T: Copy + Clone + PartialEq + Eq> SafeRange<T> {
    pub fn is_empty(self) -> bool {
        self.end == self.start
    }
}

/// A trait used for abstracting over unsafe over iteration of ids
pub trait Step {
    unsafe fn step(&mut self);
    unsafe fn step_back(&mut self);
}

/// This is one part of the secret sauce that ensures that indices from
/// different arenas cannot be mixed. You should never need to use this type in
/// your code.
#[derive(Copy, Clone, PartialOrd, PartialEq, Eq)]
pub struct InvariantLifetime<'a>(PhantomData<fn(&'a ()) -> &'a ()>);

/// Create an invariant lifetime. This is one part of the secret sauce that
/// ensures that indices from different arenas cannot be mixed. You should
/// never need to use this type in your code.
pub fn invariant_lifetime<'tag>() -> InvariantLifetime<'tag> {
    InvariantLifetime(PhantomData)
}

/// An index into the arena. You will not directly use this type, but one of
/// the aliases this crate provides (`Idx32`, `Idx16` or `Idx8`).
///
/// The only way to get an index into an arena is to `add` a value to it. With
/// an `Idx` you can index or mutably index into the arena to observe or mutate
/// the value.
#[derive(Copy, Clone, PartialOrd, PartialEq, Eq)]
pub struct Idx<'tag, I: Copy + Clone> {
    index: I,
    tag: InvariantLifetime<'tag>,
}
impl<'tag, T: Copy + Clone + AddAssign> Idx<'tag, T> {
    pub unsafe fn add(&mut self, delta: T) {
        self.index += delta;
    }
}
impl<'tag, T: Copy + Clone + SubAssign> Idx<'tag, T> {
    pub unsafe fn sub(&mut self, delta: T) {
        self.index -= delta;
    }
}
impl<'tag, T: Copy + Clone + Sub<Output = T>> Idx<'tag, T> {
    pub fn distance(self, other: Self) -> T {
        self.index - other.index
    }
    pub fn index(self) -> T {
        self.index
    }
}
impl<'tag, I: Copy + Clone + Sized + Debug> Debug for Idx<'tag, I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Idx: {:?}", self.index))
    }
}
impl<'tag, I: Hash + Copy + Clone> Hash for Idx<'tag, I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
}
impl<'tag, I: Display + Copy + Clone> Display for Idx<'tag, I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.index.fmt(f)
    }
}
/// The index type for a small arena is 32 bits large. You will usually get the
/// index from the arena and use it by indexing, e.g. `arena[index]`.
///
/// # Examples
///
/// ```
///# use {VARF::compact_arena::Idx32, core::mem::size_of};
/// assert_eq!(size_of::<Idx32<'_>>(), size_of::<u32>());
/// ```
pub type Idx32<'tag> = Idx<'tag, u32>;

/// The index type for a tiny arena is 16 bits large. You will usually get the
/// index from the arena and use it by indexing, e.g. `arena[index]`.
///
/// # Examples:
///
/// ```
///# use VARF::compact_arena::Idx16;
///# use std::mem::size_of;
/// assert_eq!(size_of::<Idx16<'_>>(), size_of::<u16>());
/// ```
pub type Idx16<'tag> = Idx<'tag, u16>;

/// The index type for a nano arena is 8 bits large. You will usually get the
/// index from the arena and use it by indexing, e.g. `arena[index]`.
///
/// # Examples:
///
/// ```
///# use {VARF::compact_arena::Idx8, core::mem::size_of};
/// assert_eq!(size_of::<Idx8<'_>>(), size_of::<u8>());
/// ```
pub type Idx8<'tag> = Idx<'tag, u8>;

/// An error type that gets returned on trying to add an element to an already
/// full arena. It contains the element so you can reuse it
pub struct CapacityExceeded<T>(T);

impl<T> CapacityExceeded<T> {
    /// Consumes self and returns the contained value.
    pub fn into_value(self) -> T {
        self.0
    }
}

impl<T> Debug for CapacityExceeded<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "Capacity Exceeded")
    }
}

impl<T> Display for CapacityExceeded<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "Capacity Exceeded")
    }
}
impl<T> Error for CapacityExceeded<T> {
    fn description(&self) -> &str {
        "Capacity exceeded"
    }
}

/// A "Small" arena based on a resizable slice (i.e. a `Vec`) that can be
/// indexed with 32-bit `Idx32`s. This can help reduce memory overhead when
/// using many pointer-heavy objects on 64-bit systems.
///
/// You can obtain an instance of this type by calling `mk_arena`.
pub struct SmallArena<'tag, T> {
    #[allow(dead_code)]
    tag: InvariantLifetime<'tag>,
    // TODO: Use a custom structure, forbid resizing over 2G items
    data: Vec<T>,
}

/// Run code using an arena. The indirection through the macro is required
/// to safely bind the indices to the arena. The macro takes an identifier that
/// will be bound to the `&mut Arena<_, _>` and an expression that will be
/// executed within a block where the arena is instantiated. The arena will be
/// dropped afterwards.
///
/// # Examples
///
/// ```
///# use compact_arena::mk_arena;
/// mk_arena!(arena);
/// let half = arena.add(21);
/// assert_eq!(42, arena[half] + arena[half]);
/// ```
///
/// You can also specify an initial capacity after the arena identifier:
///
/// ```
///# #[allow(dead_code)]
///# use compact_arena::mk_arena;
/// mk_arena!(arena, 65536);
///# arena.add(2usize);
/// ..
///# ;
/// ```
///
/// The capacity will be extended automatically, so `new_arena!(0)` creates a
/// valid arena with initially zero capacity that will be extended on the first
/// `add`.
#[cfg(feature = "alloc")]
#[macro_export]
macro_rules! mk_arena {
    ($name:ident) => {
        $crate::mk_arena!($name, 128 * 1024)
    };
    ($name:ident, $cap:expr) => {
        let tag = $crate::invariant_lifetime();
        let _guard;
        let mut $name = unsafe {
            // this is not per-se unsafe but we need it to be public and
            // calling it with a non-unique `tag` would allow arena mixups,
            // which may introduce UB in `Index`/`IndexMut`
            $crate::compact_arena::SmallArena::new(tag, $cap)
        };
        // this doesn't make it to MIR, but ensures that borrowck will not
        // unify the lifetimes of two macro calls by binding the lifetime to
        // drop scope
        if false {
            struct Guard<'tag>(&'tag $crate::InvariantLifetime<'tag>);
            impl<'tag> ::core::ops::Drop for Guard<'tag> {
                fn drop(&mut self) {}
            }
            _guard = Guard(&tag);
        }
    };
}

/// Create a tiny arena. The indirection through this macro is required
/// to bind the indices to the arena.
///
/// # Examples
///
/// ```
///# use VARF::mk_tiny_arena;
/// mk_tiny_arena!(arena);
/// let idx = arena.add(1usize);
/// assert_eq!(1, arena[idx]);
/// ```
#[macro_export]
macro_rules! mk_tiny_arena {
    ($name:ident) => {
        let tag = $crate::compact_arena::invariant_lifetime();
        let _guard;
        let mut $name = unsafe {
            // this is not per-se unsafe but we need it to be public and
            // calling it with a non-unique `tag` would allow arena mixups,
            // which may introduce UB in `Index`/`IndexMut`
            $crate::compact_arena::TinyArena::new(tag)
        };
        // this doesn't make it to MIR, but ensures that borrowck will not
        // unify the lifetimes of two macro calls by binding the lifetime to
        // drop scope
        if false {
            struct Guard<'tag>(&'tag $crate::compact_arena::InvariantLifetime<'tag>);
            impl<'tag> ::core::ops::Drop for Guard<'tag> {
                fn drop(&mut self) {}
            }
            _guard = Guard(&tag);
        }
    };
}

/// Create a tiny arena. The indirection through this macro is required
/// to bind the indices to the arena.
///
/// # Examples
///
/// ```
///# use VARF::mk_nano_arena;
/// mk_nano_arena!(arena);
/// let idx = arena.add(1usize);
/// assert_eq!(1, arena[idx]);
/// ```
#[macro_export]
macro_rules! mk_nano_arena {
    ($name:ident) => {
        let tag = $crate::compact_arena::invariant_lifetime();
        let _guard;
        let mut $name = unsafe {
            // this is not per-se unsafe but we need it to be public and
            // calling it with a non-unique `tag` would allow arena mixups,
            // which may introduce UB in `Index`/`IndexMut`
            $crate::compact_arena::NanoArena::new(tag)
        };
        // this doesn't make it to MIR, but ensures that borrowck will not
        // unify the lifetimes of two macro calls by binding the lifetime to
        // drop scope
        if false {
            struct Guard<'tag>(&'tag $crate::compact_arena::InvariantLifetime<'tag>);
            impl<'tag> ::core::ops::Drop for Guard<'tag> {
                fn drop(&mut self) {}
            }
            _guard = Guard(&tag);
        }
    };
}

#[cfg(feature = "alloc")]
impl<'tag, T> SmallArena<'tag, T> {
    /// create a new SmallArena. Don't do this manually. Use the macro instead.
    ///
    /// # Safety
    ///
    /// The whole tagged indexing trick relies on the `'tag` you give to this
    /// constructor. You must never use this value in another arena, lest you
    /// might be able to mix up the indices of the two, which could lead to
    /// out of bounds access and thus **Undefined Behavior**!
    pub unsafe fn new(tag: InvariantLifetime<'tag>, capacity: usize) -> Self {
        SmallArena {
            tag,
            data: Vec::with_capacity(capacity),
        }
    }

    /// Add an item to the arena, get an index or CapacityExceeded back.
    #[inline]
    pub fn try_add(&mut self, item: T) -> Result<Idx32<'tag>, CapacityExceeded<T>> {
        let i = self.data.len();
        if i == (core::u32::MAX as usize) {
            return Err(CapacityExceeded(item));
        }
        self.data.push(item);
        Ok(Idx {
            index: (i as u32),
            tag: self.tag,
        })
    }

    /// Add an item to the arena, get an index back.
    #[inline]
    pub fn add(&mut self, item: T) -> Idx32<'tag> {
        self.try_add(item).unwrap()
    }
}

#[cfg(feature = "alloc")]
impl<'tag, T> Index<Idx32<'tag>> for SmallArena<'tag, T> {
    type Output = T;

    /// Gets an immutable reference to the value at this index.
    #[inline]
    fn index(&self, i: Idx32<'tag>) -> &T {
        debug_assert!((i.index as usize) < self.data.len());
        // we can use unchecked indexing here because branding the indices with
        // the arenas lifetime ensures that the index is always valid & within
        // bounds
        unsafe { &self.data.get_unchecked(i.index as usize) }
    }
}

#[cfg(feature = "alloc")]
impl<'tag, T> IndexMut<Idx32<'tag>> for SmallArena<'tag, T> {
    /// Gets a mutable reference to the value at this index.
    #[inline]
    fn index_mut(&mut self, i: Idx32<'tag>) -> &mut T {
        debug_assert!((i.index as usize) < self.data.len());
        // we can use unchecked indexing here because branding the indices with
        // the arenas lifetime ensures that the index is always valid & within
        // bounds
        unsafe { self.data.get_unchecked_mut(i.index as usize) }
    }
}

const TINY_ARENA_ITEMS: u32 = 65536;
const NANO_ARENA_ITEMS: u16 = 256;

/// A "tiny" arena containing <64K elements.
pub struct TinyArena<'tag, T> {
    tag: InvariantLifetime<'tag>,
    pub(crate) len: u32,
    pub(crate) data: [MaybeUninit<T>; TINY_ARENA_ITEMS as usize],
}

impl<'tag, T> TinyArena<'tag, T> {
    /// create a new TinyArena. Don't do this manually. Use the macro instead.
    ///
    /// # Safety
    ///
    /// The whole tagged indexing trick relies on the `'tag` you give to this
    /// constructor. You must never use this value in another arena, lest you
    /// might be able to mix up the indices of the two, which could lead to
    /// out of bounds access and thus **Undefined Behavior**!

    pub unsafe fn new(tag: InvariantLifetime<'tag>) -> TinyArena<'tag, T> {
        TinyArena {
            tag,
            data: MaybeUninit::uninit().assume_init(),
            len: 0,
        }
    }
    pub fn range_to_end(&self, from: Idx16<'tag>) -> SafeRange<Idx16<'tag>> {
        SafeRange {
            start: from,
            end: Idx16 {
                index: self.len as u16,
                tag: self.tag,
            },
        }
    }
    pub fn empty_range_from_end(&self) -> SafeRange<Idx16<'tag>> {
        let next_id = Idx16 {
            index: self.len as u16,
            tag: self.tag,
        };
        SafeRange {
            start: next_id,
            end: next_id,
        }
    }
    pub fn full_range(&self) -> SafeRange<Idx16<'tag>> {
        SafeRange {
            start: Idx16 {
                index: 0,
                tag: self.tag,
            },
            end: Idx16 {
                index: self.len as u16,
                tag: self.tag,
            },
        }
    }
    pub fn extend_range_to_end(&self, mut range: SafeRange<Idx16<'tag>>) -> SafeRange<Idx16<'tag>> {
        range.end = Idx16 {
            index: self.len as u16,
            tag: self.tag,
        };
        range
    }
    /// # Safety
    /// Behavior is undefined if any of the following conditions are violated:
    ///
    /// * `dst` must be valid for writes.
    ///
    /// * `dst` must be properly aligned
    pub unsafe fn init(ptr: *mut Self) {
        ptr::write(&mut (*ptr).len, 0);
    }
    /// # Safety
    /// Behavior is undefined if any of the following conditions are violated:
    ///
    /// * `dst` must be valid for writes.
    ///
    /// * `dst` must be properly aligned
    pub unsafe fn init_from<O>(dst: *mut Self, src: &TinyArena<O>) {
        ptr::write(&mut (*dst).len, src.len);
    }

    /// Add an item to the arena, get an index or CapacityExceeded back.
    #[inline]
    pub fn try_add(&mut self, item: T) -> Result<Idx16<'tag>, CapacityExceeded<T>> {
        let i = self.len;
        if i >= TINY_ARENA_ITEMS {
            return Err(CapacityExceeded(item));
        }
        self.data[i as usize] = MaybeUninit::new(item);
        self.len += 1;
        Ok(Idx16 {
            index: i as u16,
            tag: self.tag,
        })
    }

    /// Add an item to the arena, get an index or CapacityExceeded back.
    #[inline]
    pub unsafe fn write(&mut self, idx: Idx16<'tag>, val: MaybeUninit<T>) {
        self.data[idx.index as usize] = val;
    }
    /// Add an item to the arena, get an index back
    pub fn add(&mut self, item: T) -> Idx16<'tag> {
        self.try_add(item).unwrap()
    }
}
impl<'tag, T: Unpin> TinyArena<'tag, T> {
    /// # Safety
    /// Behavior is undefined if any of the following conditions are violated:
    ///
    /// * `dst` must be valid for writes.
    ///
    /// * `dst` must be properly aligned
    pub unsafe fn move_to(dst: *mut Self, src: &mut TinyArena<T>) {
        ptr::write(&mut (*dst).len, src.len);
        ptr::copy_nonoverlapping(&src.data[0], &mut (*dst).data[0], src.len as usize);
        src.len = 0;
    }
    /// # Safety
    /// When this is called the contents of the arena will not be dropped. This is only Safe in the following conditions:
    /// - The contents of the arena don't have a drop implementation
    /// - The contents have the arena have been copied to another arena usinf ptr::copy
    pub unsafe fn mark_moved(&mut self) {
        self.len = 0;
    }
}
impl<'tag, T: Copy> TinyArena<'tag, T> {
    /// # Safety
    /// Behavior is undefined if any of the following conditions are violated:
    ///
    /// * `dst` must be valid for writes.
    ///
    /// * `dst` must be properly aligned
    pub unsafe fn copy_to(dst: *mut Self, src: &TinyArena<T>) {
        ptr::write(&mut (*dst).len, src.len);
        ptr::copy_nonoverlapping(&src.data[0], &mut (*dst).data[0], src.len as usize);
    }
}

impl<'tag, T> Drop for TinyArena<'tag, T> {
    // dropping the arena drops all values
    fn drop(&mut self) {
        for i in 0..mem::replace(&mut self.len, 0) as usize {
            unsafe {
                ptr::drop_in_place(self.data[i].as_mut_ptr());
            }
        }
    }
}

/// A "nano" arena containing up to 256 elements.
///
/// You will likely use this via the `mk_nano_arena` macro.
pub struct NanoArena<'tag, T> {
    tag: InvariantLifetime<'tag>,
    pub(crate) len: u16,
    pub(crate) data: [MaybeUninit<T>; NANO_ARENA_ITEMS as usize],
}

impl<'tag, T> NanoArena<'tag, T> {
    /// create a new NanoArena. Don't do this manually. Use the
    /// [`mk_nano_arena`] macro instead.
    ///
    /// # Safety
    ///
    /// The whole tagged indexing trick relies on the `'tag` you give to
    /// this constructor. You must never use this value in another arena,
    /// lest you might be able to mix up the indices of the two, which
    /// could lead to out of bounds access and thus **Undefined Behavior**!
    pub unsafe fn new(tag: InvariantLifetime<'tag>) -> NanoArena<'tag, T> {
        NanoArena {
            tag,
            data: MaybeUninit::uninit().assume_init(),
            len: 0,
        }
    }
    /// # Safety
    /// Behavior is undefined if any of the following conditions are violated:
    ///
    /// * `dst` must be valid for writes.
    ///
    /// * `dst` must be properly aligned
    pub unsafe fn init(dst: *mut Self) {
        ptr::write(&mut (*dst).len, 0);
    }

    pub fn range_to_end(&self, from: Idx8<'tag>) -> SafeRange<Idx8<'tag>> {
        SafeRange {
            start: from,
            end: Idx8 {
                index: self.len as u8,
                tag: self.tag,
            },
        }
    }

    pub fn empty_range_from_end(&self) -> SafeRange<Idx8<'tag>> {
        let next_id = Idx8 {
            index: self.len as u8,
            tag: self.tag,
        };
        SafeRange {
            start: next_id,
            end: next_id,
        }
    }

    pub fn extend_range_to_end(&self, mut range: SafeRange<Idx8<'tag>>) -> SafeRange<Idx8<'tag>> {
        range.end = Idx8 {
            index: self.len as u8,
            tag: self.tag,
        };
        range
    }
    pub fn full_range(&self) -> SafeRange<Idx8<'tag>> {
        SafeRange {
            start: Idx8 {
                index: 0,
                tag: self.tag,
            },
            end: Idx8 {
                index: self.len as u8,
                tag: self.tag,
            },
        }
    }
    /// # Safety
    /// Behavior is undefined if any of the following conditions are violated:
    ///
    /// * `dst` must be valid for writes.
    ///
    /// * `dst` must be properly aligned
    ///
    /// * If drop is called on the first `src.len` elements of `dst` before they are initialized
    pub unsafe fn init_from<E>(dst: *mut Self, src: &NanoArena<E>) {
        ptr::write(&mut (*dst).len, src.len);
    }

    /// Add an item to the arena, get an index or CapacityExceeded back.
    #[inline]
    pub fn try_add(&mut self, item: T) -> Result<Idx8<'tag>, CapacityExceeded<T>> {
        let i = self.len;
        if i >= NANO_ARENA_ITEMS {
            return Err(CapacityExceeded(item));
        }
        self.data[usize::from(i)] = MaybeUninit::new(item);
        self.len += 1;
        Ok(Idx8 {
            index: i as u8,
            tag: self.tag,
        })
    }
    #[inline]
    pub unsafe fn write(&mut self, idx: Idx8<'tag>, val: MaybeUninit<T>) {
        self.data[idx.index as usize] = val;
    }

    /// Add an item to the arena, get an index back
    pub fn add(&mut self, item: T) -> Idx8<'tag> {
        self.try_add(item).unwrap()
    }
}
impl<'tag, T: Unpin> NanoArena<'tag, T> {
    /// # Safety
    /// Behavior is undefined if any of the following conditions are violated:
    ///
    /// * `dst` must be valid for writes.
    ///
    /// * `dst` must be properly aligned
    pub unsafe fn move_to(dst: *mut Self, src: &mut NanoArena<T>) {
        ptr::write(&mut (*dst).len, src.len);
        ptr::copy_nonoverlapping(&src.data[0], &mut (*dst).data[0], src.len as usize);
        src.len = 0;
    }
    /// # Safety
    /// When this is called the contents of the arena will not be dropped. This is only Safe in the following conditions:
    /// - The contents of the arena don't have a drop implementation
    /// - The contents have the arena have been copied to another arena usinf ptr::copy
    pub unsafe fn mark_moved(&mut self) {
        self.len = 0;
    }
}
impl<'tag, T: Copy> NanoArena<'tag, T> {
    /// # Safety
    /// Behavior is undefined if any of the following conditions are violated:
    ///
    /// * `dst` must be valid for writes.
    ///
    /// * `dst` must be properly aligned
    pub unsafe fn copy_to(dest: *mut Self, src: &NanoArena<T>) {
        ptr::write(&mut (*dest).len, src.len);
        ptr::copy_nonoverlapping(&src.data[0], &mut (*dest).data[0], usize::from(src.len));
    }
}
impl<'tag, T> Drop for NanoArena<'tag, T> {
    // dropping the arena drops all values
    fn drop(&mut self) {
        for i in 0..mem::replace(&mut self.len, 0) as usize {
            unsafe {
                ptr::drop_in_place(self.data[i].as_mut_ptr());
            }
        }
    }
}

impl<'tag, T> Index<Idx16<'tag>> for TinyArena<'tag, T> {
    type Output = T;

    fn index(&self, i: Idx16<'tag>) -> &T {
        debug_assert!(u32::from(i.index) < self.len);
        // we can use unchecked indexing here because branding the indices with
        // the arenas lifetime ensures that the index is always valid & within
        // bounds
        unsafe { &*self.data.get_unchecked(usize::from(i.index)).as_ptr() }
    }
}

impl<'tag, T> IndexMut<Idx16<'tag>> for TinyArena<'tag, T> {
    fn index_mut(&mut self, i: Idx16<'tag>) -> &mut T {
        debug_assert!(u32::from(i.index) < self.len);
        // we can use unchecked indexing here because branding the indices with
        // the arenas lifetime ensures that the index is always valid & within
        // bounds
        unsafe {
            &mut *self
                .data
                .get_unchecked_mut(usize::from(i.index))
                .as_mut_ptr()
        }
    }
}

impl<'tag, T> Index<Idx8<'tag>> for NanoArena<'tag, T> {
    type Output = T;

    fn index(&self, i: Idx8<'tag>) -> &T {
        debug_assert!(u16::from(i.index) < self.len);
        // we can use unchecked indexing here because branding the indices with
        // the arenas lifetime ensures that the index is always valid & within
        // bounds
        unsafe { &*self.data.get_unchecked(usize::from(i.index)).as_ptr() }
    }
}

impl<'tag, T> IndexMut<Idx8<'tag>> for NanoArena<'tag, T> {
    fn index_mut(&mut self, i: Idx8<'tag>) -> &mut T {
        debug_assert!(u16::from(i.index) < self.len);
        // we can use unchecked indexing here because branding the indices with
        // the arenas lifetime ensures that the index is always valid & within
        // bounds
        unsafe {
            &mut *self
                .data
                .get_unchecked_mut(usize::from(i.index))
                .as_mut_ptr()
        }
    }
}

impl<'tag, T> Index<SafeRange<Idx8<'tag>>> for NanoArena<'tag, T> {
    type Output = [T];

    fn index(&self, i: SafeRange<Idx8<'tag>>) -> &[T] {
        debug_assert!(u16::from(i.end.index) <= self.len);
        debug_assert!(u16::from(i.start.index) < self.len);
        debug_assert!(i.start.index < i.end.index);
        // we can use unchecked indexing here because branding the indices with
        // the arenas lifetime ensures that the index is always valid & within
        // bounds
        let start = unsafe { &*self.data.get_unchecked(usize::from(i.start.index)).as_ptr() };
        unsafe { std::slice::from_raw_parts(start, usize::from(i.end.index - i.start.index)) }
    }
}

impl<'tag, T> IndexMut<SafeRange<Idx8<'tag>>> for NanoArena<'tag, T> {
    fn index_mut(&mut self, i: SafeRange<Idx8<'tag>>) -> &mut [T] {
        debug_assert!(u16::from(i.end.index) <= self.len);
        debug_assert!(u16::from(i.start.index) < self.len);
        debug_assert!(i.start.index < i.end.index);
        // we can use unchecked indexing here because branding the indices with
        // the arenas lifetime ensures that the index is always valid & within
        // bounds
        let start = unsafe {
            &mut *self
                .data
                .get_unchecked_mut(usize::from(i.start.index))
                .as_mut_ptr()
        };
        unsafe { std::slice::from_raw_parts_mut(start, usize::from(i.end.index - i.start.index)) }
    }
}
impl<'tag, T> Index<SafeRange<Idx16<'tag>>> for TinyArena<'tag, T> {
    type Output = [T];

    fn index(&self, i: SafeRange<Idx16<'tag>>) -> &[T] {
        debug_assert!(u32::from(i.end.index) <= self.len);
        debug_assert!(u32::from(i.start.index) < self.len);
        debug_assert!(i.start.index < i.end.index);
        // we can use unchecked indexing here because branding the indices with
        // the arenas lifetime ensures that the index is always valid & within
        // bounds
        let start = unsafe { &*self.data.get_unchecked(usize::from(i.start.index)).as_ptr() };
        unsafe { std::slice::from_raw_parts(start, usize::from(i.end.index - i.start.index)) }
    }
}

impl<'tag, T> IndexMut<SafeRange<Idx16<'tag>>> for TinyArena<'tag, T> {
    fn index_mut(&mut self, i: SafeRange<Idx16<'tag>>) -> &mut [T] {
        debug_assert!(u32::from(i.end.index) <= self.len);
        debug_assert!(u32::from(i.start.index) < self.len);
        debug_assert!(i.start.index < i.end.index);
        // we can use unchecked indexing here because branding the indices with
        // the arenas lifetime ensures that the index is always valid & within
        // bounds
        let start = unsafe {
            &mut *self
                .data
                .get_unchecked_mut(usize::from(i.start.index))
                .as_mut_ptr()
        };
        unsafe { std::slice::from_raw_parts_mut(start, usize::from(i.end.index - i.start.index)) }
    }
}
