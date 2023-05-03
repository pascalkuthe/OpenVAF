//! Small lists of entity references.

use std::marker::PhantomData;
use std::mem;
use stdx::packed_option::ReservedValue;

/// A small list of entity references allocated from a pool.
///
/// An `ListHandle<T>` type provides similar functionality to `Vec<T>`, but with some important
/// differences in the implementation:
///
/// 1. Memory is allocated from a `ListPool<T>` instead of the global heap.
/// 2. The footprint of an entity list is 4 bytes, compared with the 24 bytes for `Vec<T>`.
/// 3. An entity list doesn't implement `Drop`, leaving it to the pool to manage memory.
///
/// The list pool is intended to be used as a LIFO allocator. After building up a larger data
/// structure with many list references, the whole thing can be discarded quickly by clearing the
/// pool.
///
/// # Safety
///
/// Entity lists are not as safe to use as `Vec<T>`, but they never jeopardize Rust's memory safety
/// guarantees. These are the problems to be aware of:
///
/// - If you lose track of an entity list, its memory won't be recycled until the pool is cleared.
///   This can cause the pool to grow very large with leaked lists.
/// - If entity lists are used after their pool is cleared, they may contain garbage data, and
///   modifying them may corrupt other lists in the pool.
/// - If an entity list is used with two different pool instances, both pools are likely to become
///   corrupted.
///
/// Entity lists can be cloned, but that operation should only be used as part of cloning the whole
/// function they belong to. *Cloning an entity list does not allocate new memory for the clone*.
/// It creates an alias of the same memory.
///
/// Entity lists cannot be hashed and compared for equality because it's not possible to compare the
/// contents of the list without the pool reference.
///
/// # Implementation
///
/// The `ListHandle` itself is designed to have the smallest possible footprint. This is important
/// because it is used inside very compact data structures like `InstructionData`. The list
/// contains only a 32-bit index into the pool's memory vector, pointing to the first element of
/// the list.
///
/// The pool is just a single `Vec<T>` containing all of the allocated lists. Each list is
/// represented as three contiguous parts:
///
/// 1. The number of elements in the list.
/// 2. The list elements.
/// 3. Excess capacity elements.
///
/// The total size of the three parts is always a power of two, and the excess capacity is always
/// as small as possible. This means that shrinking a list may cause the excess capacity to shrink
/// if a smaller power-of-two size becomes available.
///
/// Both growing and shrinking a list may cause it to be reallocated in the pool vector.
///
/// The index stored in an `ListHandle` points to part 2, the list elements. The value 0 is
/// reserved for the empty list which isn't allocated in the vector.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ListHandle<T: ReservedValue> {
    index: u32,
    unused: PhantomData<T>,
}

/// Create an empty list.
impl<T: ReservedValue> Default for ListHandle<T> {
    fn default() -> Self {
        Self { index: 0, unused: PhantomData }
    }
}

/// A memory pool for storing lists of `T`.
#[derive(Clone, Debug)]
pub struct ListPool<T: ReservedValue> {
    // The main array containing the lists.
    data: Vec<T>,

    // Heads of the free lists, one for each size class.
    free: Vec<usize>,
}

/// Lists are allocated in sizes that are powers of two, starting from 4.
/// Each power of two is assigned a size class number, so the size is `4 << SizeClass`.
type SizeClass = u8;

/// Get the size of a given size class. The size includes the length field, so the maximum list
/// length is one less than the class size.
#[inline]
fn sclass_size(sclass: SizeClass) -> usize {
    4 << sclass
}

/// Get the size class to use for a given list length.
/// This always leaves room for the length element in addition to the list elements.
#[inline]
fn sclass_for_length(len: usize) -> SizeClass {
    30 - (len as u32 | 3).leading_zeros() as SizeClass
}

/// Is `len` the minimum length in its size class?
#[inline]
fn is_sclass_min_length(len: usize) -> bool {
    len > 3 && len.is_power_of_two()
}

impl<T: ReservedValue + Copy + Into<usize> + From<usize>> ListPool<T> {
    /// Create a new list pool.
    pub fn new() -> Self {
        Self { data: Vec::new(), free: Vec::new() }
    }

    /// Clear the pool, forgetting about all lists that use it.
    ///
    /// This invalidates any existing entity lists that used this pool to allocate memory.
    ///
    /// The pool's memory is not released to the operating system, but kept around for faster
    /// allocation in the future.
    pub fn clear(&mut self) {
        self.data.clear();
        self.free.clear();
    }

    /// Read the length of a list field, if it exists.
    fn len_of(&self, list: ListHandle<T>) -> Option<usize> {
        let idx = list.index as usize;
        // `idx` points at the list elements. The list length is encoded in the element immediately
        // before the list elements.
        //
        // The `wrapping_sub` handles the special case 0, which is the empty list. This way, the
        // cost of the bounds check that we have to pay anyway is co-opted to handle the special
        // case of the empty list.
        self.data.get(idx.wrapping_sub(1)).map(|&len| len.into())
    }

    /// Allocate a storage block with a size given by `sclass`.
    ///
    /// Returns the first index of an available segment of `self.data` containing
    /// `sclass_size(sclass)` elements. The allocated memory is filled with reserved
    /// values.
    fn alloc(&mut self, sclass: SizeClass) -> usize {
        // First try the free list for this size class.
        match self.free.get(sclass as usize).cloned() {
            Some(head) if head > 0 => {
                // The free list pointers are offset by 1, using 0 to terminate the list.
                // A block on the free list has two entries: `[ 0, next ]`.
                // The 0 is where the length field would be stored for a block in use.
                // The free list heads and the next pointer point at the `next` field.
                self.free[sclass as usize] = self.data[head].into();
                head - 1
            }
            _ => {
                // Nothing on the free list. Allocate more memory.
                let offset = self.data.len();
                self.data.resize(offset + sclass_size(sclass), T::reserved_value());
                offset
            }
        }
    }

    /// Free a storage block with a size given by `sclass`.
    ///
    /// This must be a block that was previously allocated by `alloc()` with the same size class.
    fn free(&mut self, block: usize, sclass: SizeClass) {
        let sclass = sclass as usize;

        // Make sure we have a free-list head for `sclass`.
        if self.free.len() <= sclass {
            self.free.resize(sclass + 1, 0);
        }

        // Make sure the length field is cleared.
        self.data[block] = T::from(0);
        // Insert the block on the free list which is a single linked list.
        self.data[block + 1] = T::from(self.free[sclass]);
        self.free[sclass] = block + 1
    }

    /// Returns two mutable slices representing the two requested blocks.
    ///
    /// The two returned slices can be longer than the blocks. Each block is located at the front
    /// of the respective slice.
    fn mut_slices(&mut self, block0: usize, block1: usize) -> (&mut [T], &mut [T]) {
        if block0 < block1 {
            let (s0, s1) = self.data.split_at_mut(block1);
            (&mut s0[block0..], s1)
        } else {
            let (s1, s0) = self.data.split_at_mut(block0);
            (s0, &mut s1[block1..])
        }
    }

    /// Reallocate a block to a different size class.
    ///
    /// Copy `elems_to_copy` elements from the old to the new block.
    fn realloc(
        &mut self,
        block: usize,
        from_sclass: SizeClass,
        to_sclass: SizeClass,
        elems_to_copy: usize,
    ) -> usize {
        debug_assert!(elems_to_copy <= sclass_size(from_sclass));
        debug_assert!(elems_to_copy <= sclass_size(to_sclass));
        let new_block = self.alloc(to_sclass);

        if elems_to_copy > 0 {
            let (old, new) = self.mut_slices(block, new_block);
            new[0..elems_to_copy].copy_from_slice(&old[0..elems_to_copy]);
        }

        self.free(block, from_sclass);
        new_block
    }
}

impl<T: ReservedValue + Copy + Into<usize> + From<usize>> Default for ListPool<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: ReservedValue + Copy + Into<usize> + From<usize>> ListHandle<T> {
    /// Create a new empty list.
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new list with the contents initialized from a slice.
    pub fn from_slice(slice: &[T], pool: &mut ListPool<T>) -> Self {
        let len = slice.len();
        if len == 0 {
            return Self::new();
        }

        let block = pool.alloc(sclass_for_length(len));
        pool.data[block] = T::from(len);
        pool.data[block + 1..=block + len].copy_from_slice(slice);

        Self { index: (block + 1) as u32, unused: PhantomData }
    }

    /// Returns `true` if the list has a length of 0.
    pub fn is_empty(&self) -> bool {
        // 0 is a magic value for the empty list. Any list in the pool array must have a positive
        // length.
        self.index == 0
    }

    /// Get the number of elements in the list.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self, pool: &ListPool<T>) -> usize {
        // Both the empty list and any invalidated old lists will return `None`.
        pool.len_of(self.clone()).unwrap_or(0)
    }

    /// Returns `true` if the list is valid
    pub fn is_valid(self, pool: &ListPool<T>) -> bool {
        // We consider an empty list to be valid
        self.is_empty() || pool.len_of(self).is_some()
    }

    /// Get the list as a slice.
    pub fn as_slice<'a>(&self, pool: &'a ListPool<T>) -> &'a [T] {
        let idx = self.index as usize;
        match pool.len_of(self.clone()) {
            None => &[],
            Some(len) => &pool.data[idx..idx + len],
        }
    }

    /// Get a single element from the list.
    pub fn get(&self, index: usize, pool: &ListPool<T>) -> Option<T> {
        self.as_slice(pool).get(index).cloned()
    }

    /// Get the first element from the list.
    pub fn first(&self, pool: &ListPool<T>) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            Some(pool.data[self.index as usize])
        }
    }

    /// Get the list as a mutable slice.
    pub fn as_mut_slice<'a>(&'a mut self, pool: &'a mut ListPool<T>) -> &'a mut [T] {
        let idx = self.index as usize;
        match pool.len_of(self.clone()) {
            None => &mut [],
            Some(len) => &mut pool.data[idx..idx + len],
        }
    }

    /// Get a mutable reference to a single element from the list.
    pub fn get_mut<'a>(&'a mut self, index: usize, pool: &'a mut ListPool<T>) -> Option<&'a mut T> {
        self.as_mut_slice(pool).get_mut(index)
    }

    /// Create a deep clone of the list, which does not alias the original list.
    pub fn deep_clone(&self, pool: &mut ListPool<T>) -> Self {
        match pool.len_of(self.clone()) {
            None => Self::new(),
            Some(len) => {
                let src = self.index as usize;
                let block = pool.alloc(sclass_for_length(len));
                pool.data[block] = T::from(len);
                pool.data.copy_within(src..src + len, block + 1);

                Self { index: (block + 1) as u32, unused: PhantomData }
            }
        }
    }

    /// Removes all elements from the list.
    ///
    /// The memory used by the list is put back in the pool.
    pub fn clear(&mut self, pool: &mut ListPool<T>) {
        let idx = self.index as usize;
        match pool.len_of(self.clone()) {
            None => debug_assert_eq!(idx, 0, "Invalid pool"),
            Some(len) => pool.free(idx - 1, sclass_for_length(len)),
        }
        // Switch back to the empty list representation which has no storage.
        self.index = 0;
    }

    /// Take all elements from this list and return them as a new list. Leave this list empty.
    ///
    /// This is the equivalent of `Option::take()`.
    pub fn take(&mut self) -> Self {
        mem::take(self)
    }

    /// Appends an element to the back of the list.
    /// Returns the index where the element was inserted.
    pub fn push(&mut self, element: T, pool: &mut ListPool<T>) -> usize {
        let idx = self.index as usize;
        match pool.len_of(self.clone()) {
            None => {
                // This is an empty list. Allocate a block and set length=1.
                debug_assert_eq!(idx, 0, "Invalid pool");
                let block = pool.alloc(sclass_for_length(1));
                pool.data[block] = T::from(1);
                pool.data[block + 1] = element;
                self.index = (block + 1) as u32;
                0
            }
            Some(len) => {
                // Do we need to reallocate?
                let new_len = len + 1;
                let block;
                if is_sclass_min_length(new_len) {
                    // Reallocate, preserving length + all old elements.
                    let sclass = sclass_for_length(len);
                    block = pool.realloc(idx - 1, sclass, sclass + 1, len + 1);
                    self.index = (block + 1) as u32;
                } else {
                    block = idx - 1;
                }
                pool.data[block + new_len] = element;
                pool.data[block] = T::from(new_len);
                len
            }
        }
    }

    /// Grow list by adding `count` reserved-value elements at the end.
    ///
    /// Returns a mutable slice representing the whole list.
    fn grow<'a>(&'a mut self, count: usize, pool: &'a mut ListPool<T>) -> &'a mut [T] {
        let idx = self.index as usize;
        let new_len;
        let block;
        match pool.len_of(self.clone()) {
            None => {
                // This is an empty list. Allocate a block.
                debug_assert_eq!(idx, 0, "Invalid pool");
                if count == 0 {
                    return &mut [];
                }
                new_len = count;
                block = pool.alloc(sclass_for_length(new_len));
                self.index = (block + 1) as u32;
            }
            Some(len) => {
                // Do we need to reallocate?
                let sclass = sclass_for_length(len);
                new_len = len + count;
                let new_sclass = sclass_for_length(new_len);
                if new_sclass != sclass {
                    block = pool.realloc(idx - 1, sclass, new_sclass, len + 1);
                    self.index = (block + 1) as u32;
                } else {
                    block = idx - 1;
                }
            }
        }
        pool.data[block] = T::from(new_len);
        &mut pool.data[block + 1..block + 1 + new_len]
    }

    /// Constructs a list from an iterator.
    pub fn from_iter<I>(elements: I, pool: &mut ListPool<T>) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let mut list = Self::new();
        list.extend(elements, pool);
        list
    }

    /// Appends multiple elements to the back of the list.
    pub fn extend<I>(&mut self, elements: I, pool: &mut ListPool<T>)
    where
        I: IntoIterator<Item = T>,
    {
        let iterator = elements.into_iter();
        let (len, upper) = iterator.size_hint();
        // On most iterators this check is optimized down to `true`.
        if upper == Some(len) {
            let data = self.grow(len, pool);
            let offset = data.len() - len;
            for (src, dst) in iterator.zip(data[offset..].iter_mut()) {
                *dst = src;
            }
        } else {
            for x in iterator {
                self.push(x, pool);
            }
        }
    }

    /// Inserts an element as position `index` in the list, shifting all elements after it to the
    /// right.
    pub fn insert(&mut self, index: usize, element: T, pool: &mut ListPool<T>) {
        // Increase size by 1.
        self.push(element, pool);

        // Move tail elements.
        let seq = self.as_mut_slice(pool);
        if index < seq.len() {
            let tail = &mut seq[index..];
            for i in (1..tail.len()).rev() {
                tail[i] = tail[i - 1];
            }
            tail[0] = element;
        } else {
            debug_assert_eq!(index, seq.len());
        }
    }

    /// Removes the last element from the list.
    pub fn remove_last(&mut self, len: usize, pool: &mut ListPool<T>) {
        // Check if we deleted the last element.
        if len == 1 {
            self.clear(pool);
            return;
        }

        // Do we need to reallocate to a smaller size class?
        let mut block = self.index as usize - 1;
        if is_sclass_min_length(len) {
            let sclass = sclass_for_length(len);
            block = pool.realloc(block, sclass, sclass - 1, len);
            self.index = (block + 1) as u32;
        }

        // Finally adjust the length.
        pool.data[block] = T::from(len - 1);
    }

    /// Removes the element at position `index` from the list. Potentially linear complexity.
    pub fn remove(&mut self, index: usize, pool: &mut ListPool<T>) {
        let len;
        {
            let seq = self.as_mut_slice(pool);
            len = seq.len();
            debug_assert!(index < len);

            // Copy elements down.
            for i in index..len - 1 {
                seq[i] = seq[i + 1];
            }
        }

        self.remove_last(len, pool);
    }

    /// Removes the element at `index` in constant time by switching it with the last element of
    /// the list.
    pub fn swap_remove(&mut self, index: usize, pool: &mut ListPool<T>) {
        let seq = self.as_mut_slice(pool);
        let len = seq.len();
        debug_assert!(index < len);
        if index != len - 1 {
            seq.swap(index, len - 1);
        }

        self.remove_last(len, pool);
    }

    /// Shortens the list down to `len` elements.
    ///
    /// Does nothing if the list is already shorter than `len`.
    pub fn truncate(&mut self, new_len: usize, pool: &mut ListPool<T>) {
        if new_len == 0 {
            self.clear(pool);
            return;
        }

        if let Some(len) = pool.len_of(self.clone()) {
            if len <= new_len {
                return;
            }

            let block;
            let idx = self.index as usize;
            let sclass = sclass_for_length(len);
            let new_sclass = sclass_for_length(new_len);
            if sclass != new_sclass {
                block = pool.realloc(idx - 1, sclass, new_sclass, new_len + 1);
                self.index = (block + 1) as u32;
            } else {
                block = idx - 1;
            }
            pool.data[block] = T::from(new_len);
        }
    }

    /// Grow the list by inserting `count` elements at `index`.
    ///
    /// The new elements are not initialized, they will contain whatever happened to be in memory.
    /// Since the memory comes from the pool, this will be either zero entity references or
    /// whatever where in a previously deallocated list.
    pub fn grow_at(&mut self, index: usize, count: usize, pool: &mut ListPool<T>) {
        let data = self.grow(count, pool);

        // Copy elements after `index` up.
        for i in (index + count..data.len()).rev() {
            data[i] = data[i - count];
        }
    }
}
