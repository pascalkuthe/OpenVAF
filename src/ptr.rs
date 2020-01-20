/*

 * ******************************************************************************************
 *  Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************

 Adapted from https://github.com/rust-lang/rust src/librustc/symbol.rs under MIT-License

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

//! The AST pointer.
//!
//! Provides `P<T>`, a frozen owned smart pointer.
//!
//! # Motivations and benefits
//!
//! * **Identity**: sharing AST nodes is problematic for the various analysis
//!   passes (e.g., one may be able to bypass the borrow checker with a shared
//!   `ExprKind::AddrOf` node taking a mutable borrow).
//!
//! * **Immutability**: `P<T>` disallows mutating its inner `T`, unlike `Box<T>`
//!   (unless it contains an `Unsafe` interior, but that may be denied later).
//!   This mainly prevents mistakes, but can also enforces a kind of "purity".
//!
//! * **Efficiency**: folding can reuse allocation space for `P<T>` and `Vec<T>`,
//!   the latter even when the input and output types differ (as it would be the
//!   case with arenas or a GADT AST using type parameters to toggle features).
//!
//! * **Maintainability**: `P<T>` provides a fixed interface - `Deref`,
//!   `and_then` and `map` - which can remain fully functional even if the
//!   implementation changes (using a special thread-local heap, for example).
//!   Moreover, a switch to, e.g., `P<'a, T>` would be easy and mostly automated.

use std::fmt::{self, Debug, Display};
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};
use std::{slice, vec};

/// An owned smart pointer.
pub struct FrozenBox<T: ?Sized> {
    ptr: Box<T>,
}

/// Construct a `P<T>` from a `T` value.
#[allow(non_snake_case)]
pub fn P<T: 'static>(value: T) -> FrozenBox<T> {
    FrozenBox {
        ptr: Box::new(valu),
    }
}

impl<T: 'static> FrozenBox<T> {
    /// Move out of the pointer.
    /// Intended for chaining transformations not covered by `map`.
    pub fn and_then<U, F>(self, f: F) -> U
    where
        F: FnOnce(T) -> U,
    {
        f(*self.ptr)
    }

    /// Equivalent to `and_then(|x| x)`.
    pub fn into_inner(self) -> T {
        *self.ptr
    }

    /// Produce a new `P<T>` from `self` without reallocating.
    pub fn map<F>(mut self, f: F) -> FrozenBox<T>
    where
        F: FnOnce(T) -> T,
    {
        let x = f(*self.ptr);
        *self.ptr = x;

        self
    }

    /// Optionally produce a new `P<T>` from `self` without reallocating.
    pub fn filter_map<F>(mut self, f: F) -> Option<FrozenBox<T>>
    where
        F: FnOnce(T) -> Option<T>,
    {
        *self.ptr = f(*self.ptr)?;
        Some(self)
    }
}

impl<T: ?Sized> Deref for FrozenBox<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.ptr
    }
}

impl<T: ?Sized> DerefMut for FrozenBox<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.ptr
    }
}

impl<T: 'static + Clone> Clone for FrozenBox<T> {
    fn clone(&self) -> FrozenBox<T> {
        FrozenBox((**self).clone())
    }
}

impl<T: ?Sized + Debug> Debug for FrozenBox<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.ptr, f)
    }
}

impl<T: Display> Display for FrozenBox<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&**self, f)
    }
}

impl<T> fmt::Pointer for FrozenBox<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&self.ptr, f)
    }
}

impl<T> FrozenBox<[T]> {
    pub const fn new() -> FrozenBox<[T]> {
        // HACK(eddyb) bypass the lack of a `const fn` to create an empty `Box<[T]>`
        // (as trait methods, `default` in this case, can't be `const fn` yet).
        FrozenBox {
            ptr: unsafe {
                use std::ptr::NonNull;
                std::mem::transmute(NonNull::<[T; 0]>::dangling() as NonNull<[T]>)
            },
        }
    }

    #[inline(never)]
    pub fn from_vec(v: Vec<T>) -> FrozenBox<[T]> {
        FrozenBox {
            ptr: v.into_boxed_slice(),
        }
    }

    #[inline(never)]
    pub fn into_vec(self) -> Vec<T> {
        self.ptr.into_vec()
    }
}

impl<T> Default for FrozenBox<[T]> {
    /// Creates an empty `P<[T]>`.
    fn default() -> FrozenBox<[T]> {
        FrozenBox::new()
    }
}

impl<T: Clone> Clone for FrozenBox<[T]> {
    fn clone(&self) -> FrozenBox<[T]> {
        FrozenBox::from_vec(self.to_vec())
    }
}

impl<T> From<Vec<T>> for FrozenBox<[T]> {
    fn from(v: Vec<T>) -> Self {
        FrozenBox::from_vec(v)
    }
}

impl<T> Into<Vec<T>> for FrozenBox<[T]> {
    fn into(self) -> Vec<T> {
        self.into_vec()
    }
}

impl<T> FromIterator<T> for FrozenBox<[T]> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> FrozenBox<[T]> {
        FrozenBox::from_vec(iter.into_iter().collect())
    }
}

impl<T> IntoIterator for FrozenBox<[T]> {
    type Item = T;
    type IntoIter = vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_vec().into_iter()
    }
}

impl<'a, T> IntoIterator for &'a FrozenBox<[T]> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.ptr.into_iter()
    }
}
