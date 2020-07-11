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

use crate::data_structures::BitSet;
use core::fmt::Formatter;
use index_vec::Idx;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::iter::FromIterator;

/// A work queue is a handy data structure for tracking work left to
/// do. (For example, basic blocks left to process.) It is basically a
/// de-duplicating queue; so attempting to insert X if X is already
/// enqueued has no effect. This implementation assumes that the
/// elements are dense indices, so it can allocate the queue to size
/// and also use a bit set to track occupancy.
pub struct WorkQueue<T: Idx + From<usize>> {
    pub deque: VecDeque<T>,
    pub set: BitSet<T>,
}

impl<T: Idx + From<usize>> WorkQueue<T> {
    /// Creates a new work queue with all the elements from (0..len).
    #[inline]
    pub fn with_all(len_idx: T) -> Self {
        WorkQueue {
            deque: (0..len_idx.index()).map(T::from_usize).collect(),
            set: BitSet::new_filled(len_idx),
        }
    }

    /// Creates a new work queue that starts empty, where elements range from (0..len).
    #[inline]
    pub fn with_none(len_idx: T) -> Self {
        WorkQueue {
            deque: VecDeque::with_capacity(len_idx.index()),
            set: BitSet::new_empty(len_idx),
        }
    }

    /// Attempt to enqueue `element` in the work queue. Returns false if it was already present.
    #[inline]
    pub fn insert(&mut self, element: T) -> bool {
        if self.set.put(element) {
            false
        } else {
            self.deque.push_back(element);
            true
        }
    }

    /// Attempt to pop an element from the work queue.
    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        if let Some(element) = self.deque.pop_front() {
            self.set.set(element, false);
            Some(element)
        } else {
            None
        }
    }

    /// Attempt to take an element from the work queue
    /// This function does not remove the item from the internal set
    /// As such any element removed using `take` can never be inserted again.
    /// For must use cases [`pop`] should be used
    ///
    /// This is useful when you want to write a worklist based algorithm
    /// that processes every element exactly once
    #[inline]
    pub fn take(&mut self) -> Option<T> {
        if let Some(element) = self.deque.pop_front() {
            Some(element)
        } else {
            None
        }
    }

    /// Returns `true` if nothing is enqueued.
    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.deque.is_empty()
    }
}

impl<I: Idx + From<usize>> From<BitSet<I>> for WorkQueue<I> {
    fn from(set: BitSet<I>) -> Self {
        Self {
            deque: VecDeque::from_iter(set.ones()),
            set,
        }
    }
}

impl<I: Idx + From<usize> + Debug> Debug for WorkQueue<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("[")?;
        for item in &self.deque {
            Debug::fmt(item, f)?;
            f.write_str(" , ")?;
        }
        f.write_str("]")
    }
}
