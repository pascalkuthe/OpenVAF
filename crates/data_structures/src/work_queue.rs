/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::bit_set::BitSet;
use core::fmt::Formatter;
use index_vec::Idx;
use std::collections::VecDeque;
use std::fmt::Debug;

/// A work queue is a handy data structure for tracking work left to
/// do. (For example, basic blocks left to process.) It is basically a
/// de-duplicating queue; so attempting to insert X if X is already
/// enqueued has no effect. This implementation assumes that the
/// elements are dense indices, so it can allocate the queue to size
/// and also use a bit set to track occupancy.
#[derive(Eq, PartialEq)]
pub struct WorkQueue<T: Idx> {
    pub deque: VecDeque<T>,
    pub set: BitSet<T>,
}

impl<T: Idx> Clone for WorkQueue<T> {
    fn clone_from(&mut self, source: &Self) {
        self.deque.clone_from(&source.deque);
        self.set.clone_from(&source.set);
    }

    fn clone(&self) -> Self {
        Self { deque: self.deque.clone(), set: self.set.clone() }
    }
}

impl<T: Idx + From<usize>> WorkQueue<T> {
    /// Creates a new work queue with all the elements from (0..len).
    #[inline]
    pub fn with_all(size: usize) -> Self {
        WorkQueue { deque: (0..size).map(T::from_usize).collect(), set: BitSet::new_filled(size) }
    }

    /// Creates a new work queue that starts empty, where elements range from (0..len).
    #[inline]
    pub fn with_none(size: usize) -> Self {
        WorkQueue { deque: VecDeque::with_capacity(size), set: BitSet::new_empty(size) }
    }

    /// Attempt to enqueue `element` in the work queue. Returns whether the workque has changed
    #[inline]
    pub fn insert(&mut self, element: T) -> bool {
        if self.set.insert(element) {
            self.deque.push_back(element);
            true
        } else {
            false
        }
    }

    /// Attempt to pop an element from the work queue.
    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        if let Some(element) = self.deque.pop_front() {
            self.set.remove(element);
            Some(element)
        } else {
            None
        }
    }

    /// Attempt to take an element from the work queue
    /// This function does not remove the item from the internal set
    /// As such any element removed using `take` can never be inserted again.
    /// For must use cases [`pop`][pop] should be used
    ///
    /// This is useful when you want to write a worklist based algorithm
    /// that processes every element exactly once
    ///
    /// [pop]: crate::work_queue::WorkQueue
    #[inline]
    pub fn take(&mut self) -> Option<T> {
        self.deque.pop_front()
    }

    /// Returns `true` if nothing is enqueued.
    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.deque.is_empty()
    }
}

impl<T: Idx + From<usize>> Extend<T> for WorkQueue<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        let set = &mut self.set;
        let iter = iter.into_iter().filter(|x| set.insert(*x));
        self.deque.extend(iter)
    }
}

impl<I: Idx + From<usize>> From<BitSet<I>> for WorkQueue<I> {
    fn from(set: BitSet<I>) -> Self {
        Self { deque: set.iter().collect(), set }
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
