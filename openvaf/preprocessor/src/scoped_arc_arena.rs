use std::cell::UnsafeCell;

use sealed::Container;
pub struct ScopedArea<T: Container>(UnsafeCell<Vec<T>>);

mod sealed {
    use std::ops::Deref;
    use std::rc::Rc;
    use std::sync::Arc;

    pub trait Container: Deref + Clone {
        fn as_ptr(&self) -> *const Self::Target;
    }

    impl<T: ?Sized> Container for Arc<T> {
        fn as_ptr(&self) -> *const Self::Target {
            // triomphe Arc only supports into_raw/as_ptr for Sized types (probably an oversight).
            // Compared to using as_ptr this pointer is only save for reading because it does not have read provenance.
            // This area doesnt allow mutation tough so its fine here

            &**self
        }
    }
    impl<T: ?Sized> Container for Rc<T> {
        fn as_ptr(&self) -> *const Self::Target {
            Rc::as_ptr(self)
        }
    }
}

impl<T: Container> ScopedArea<T> {
    pub fn new() -> Self {
        Self(UnsafeCell::new(Vec::with_capacity(8)))
    }

    pub fn ensure(&self, contents: T) -> &T::Target {
        unsafe {
            // This is save because the ARC/RC will remain alive as long as self is alive
            // Therefore for the lifetime of self the backing storage can not be deallocated
            // Furthermore no mutable pointers are handed out (the unsafe cell is just used for interior mutability here)
            // So the UnsacfeCell is also save here
            let sources = &mut *self.0.get();
            // check if the same data is already guarded by the arena
            if !sources.iter().any(|x| x.as_ptr() == contents.as_ptr()) {
                sources.push(T::clone(&contents))
            }
            &*contents.as_ptr()
        }
    }

    pub fn get(&self, idx: usize) -> &T::Target {
        unsafe {
            let sources = &*self.0.get();
            &*sources[idx].as_ptr()
        }
    }
}

impl<T: Container> Default for ScopedArea<T> {
    fn default() -> Self {
        Self::new()
    }
}
