use std::ptr;

#[inline]
pub fn ensure_contains_elem<T>(vec: &mut Vec<T>, elem: usize, fill_value: impl FnMut() -> T) {
    let min_new_len = elem + 1;
    if vec.len() < min_new_len {
        vec.resize_with(min_new_len, fill_value);
    }
}

pub trait VecExtensions<T> {
    fn ensure_contains_elem(&mut self, elem: usize, fill_value: impl FnMut() -> T);
}

pub trait SliceExntesions<T> {
    /// Returns mutable references to two distinct elements, a and b. Panics if a == b.
    fn pick2_mut(&mut self, a: usize, b: usize) -> (&mut T, &mut T);

    /// Returns mutable references to three distinct elements or panics otherwise.
    fn pick3_mut(&mut self, a: usize, b: usize, c: usize) -> (&mut T, &mut T, &mut T);

    fn pick_n_mut<const N: usize>(&mut self, indices: [usize; N]) -> [&mut T; N];
}

impl<T> SliceExntesions<T> for [T] {
    #[inline]
    fn pick2_mut(&mut self, a: usize, b: usize) -> (&mut T, &mut T) {
        assert_ne!(a, b);

        let len = self.len();
        assert!(a < len && b < len);
        let ptr = self.as_mut_ptr();
        unsafe { (&mut *ptr.add(a), &mut *ptr.add(b)) }
    }

    #[inline]
    fn pick3_mut(&mut self, a: usize, b: usize, c: usize) -> (&mut T, &mut T, &mut T) {
        assert!(a != b && b != c && c != a);
        let len = self.len();
        assert!(a < len && b < len && c < len);
        let ptr = self.as_mut_ptr();
        unsafe { (&mut *ptr.add(a), &mut *ptr.add(b), &mut *ptr.add(c)) }
    }

    #[inline]
    fn pick_n_mut<const N: usize>(&mut self, indices: [usize; N]) -> [&mut T; N] {
        //safety check
        for idx1 in indices {
            assert!(idx1 < self.len());
            for idx2 in &indices[..idx1] {
                assert_ne!(idx1, *idx2);
            }
            for idx2 in &indices[idx1 + 1..] {
                assert_ne!(idx1, *idx2);
            }
        }

        let mut res = [self.as_mut_ptr(); N];
        for (res, idx) in Iterator::zip(res.iter_mut(), indices) {
            // This is save we check that the offset are unique and inbounds before
            *res = unsafe { (*res).add(idx) }
        }

        unsafe { ptr::read(&res as *const [*mut T; N] as *const [&mut T; N]) }
    }
}

impl<T> VecExtensions<T> for Vec<T> {
    fn ensure_contains_elem(&mut self, elem: usize, fill_value: impl FnMut() -> T) {
        let min_new_len = elem + 1;
        if self.len() < min_new_len {
            self.resize_with(min_new_len, fill_value);
        }
    }
}
