use super::*;
use crate::NodeData;
use std::{fmt, mem};

impl<'a, K, C> SetCursor<'a, K, C>
where
    K: Copy + fmt::Display,
    C: Comparator<K>,
{
    fn verify(&self) {
        self.path.verify(self.pool);
        // self.root.map(|root| self.pool.verify_tree(root, self.comp));
    }

    /// Get a text version of the path to the current position.
    fn tpath(&self) -> String {
        self.path.to_string()
    }
}

#[test]
fn node_size() {
    // check that nodes are cache line sized when keys are 32 bits.
    type F = SetTypes<u32>;
    assert_eq!(mem::size_of::<NodeData<F>>(), 64);
}

#[test]
fn empty() {
    let mut f = SetForest::<u32>::new();
    f.clear();

    let mut s = Set::<u32>::new();
    assert!(s.is_empty());
    s.clear(&mut f);
    assert!(!s.contains(7, &f, &()));

    // Iterator for an empty set.
    assert_eq!(s.iter(&f).next(), None);

    s.retain(&mut f, |_| unreachable!());

    let mut c = SetCursor::new(&mut s, &mut f, &());
    c.verify();
    assert_eq!(c.elem(), None);

    assert_eq!(c.goto_first(), None);
    assert_eq!(c.tpath(), "<empty path>");
}

#[test]
fn simple_cursor() {
    let mut f = SetForest::<u32>::new();
    let mut s = Set::<u32>::new();
    let mut c = SetCursor::new(&mut s, &mut f, &());

    assert!(c.insert(50));
    c.verify();
    assert_eq!(c.elem(), Some(50));

    assert!(c.insert(100));
    c.verify();
    assert_eq!(c.elem(), Some(100));

    assert!(c.insert(10));
    c.verify();
    assert_eq!(c.elem(), Some(10));

    // Basic movement.
    assert_eq!(c.next(), Some(50));
    assert_eq!(c.next(), Some(100));
    assert_eq!(c.next(), None);
    assert_eq!(c.next(), None);
    assert_eq!(c.prev(), Some(100));
    assert_eq!(c.prev(), Some(50));
    assert_eq!(c.prev(), Some(10));
    assert_eq!(c.prev(), None);
    assert_eq!(c.prev(), None);

    assert!(c.goto(50));
    assert_eq!(c.elem(), Some(50));
    assert_eq!(c.remove(), Some(50));
    c.verify();

    assert_eq!(c.elem(), Some(100));
    assert_eq!(c.remove(), Some(100));
    c.verify();
    assert_eq!(c.elem(), None);
    assert_eq!(c.remove(), None);
    c.verify();
}

#[test]
fn two_level_sparse_tree() {
    let mut f = SetForest::<u32>::new();
    let mut s = Set::<u32>::new();
    let mut c = SetCursor::new(&mut s, &mut f, &());

    // Insert enough elements that we get a two-level tree.
    // Each leaf node holds 8 elements
    assert!(c.is_empty());
    for i in 0..50 {
        assert!(c.insert(i));
        assert_eq!(c.elem(), Some(i));
    }
    assert!(!c.is_empty());

    assert_eq!(c.goto_first(), Some(0));
    assert_eq!(c.tpath(), "node2[0]--node0[0]");

    assert_eq!(c.prev(), None);
    for i in 1..50 {
        assert_eq!(c.next(), Some(i));
    }
    assert_eq!(c.next(), None);
    for i in (0..50).rev() {
        assert_eq!(c.prev(), Some(i));
    }
    assert_eq!(c.prev(), None);

    assert!(c.goto(25));
    for i in 25..50 {
        assert_eq!(c.remove(), Some(i));
        assert!(!c.is_empty());
        c.verify();
    }

    for i in (0..25).rev() {
        assert!(!c.is_empty());
        assert_eq!(c.elem(), None);
        assert_eq!(c.prev(), Some(i));
        assert_eq!(c.remove(), Some(i));
        c.verify();
    }
    assert_eq!(c.elem(), None);
    assert!(c.is_empty());
}

#[test]
fn three_level_sparse_tree() {
    let mut f = SetForest::<u32>::new();
    let mut s = Set::<u32>::new();
    let mut c = SetCursor::new(&mut s, &mut f, &());

    // Insert enough elements that we get a 3-level tree.
    // Each leaf node holds 8 elements when filled up sequentially.
    // Inner nodes hold 8 node pointers.
    assert!(c.is_empty());
    for i in 0..150 {
        assert!(c.insert(i));
        assert_eq!(c.elem(), Some(i));
    }
    assert!(!c.is_empty());

    assert!(c.goto(0));
    assert_eq!(c.tpath(), "node11[0]--node2[0]--node0[0]");

    assert_eq!(c.prev(), None);
    for i in 1..150 {
        assert_eq!(c.next(), Some(i));
    }
    assert_eq!(c.next(), None);
    for i in (0..150).rev() {
        assert_eq!(c.prev(), Some(i));
    }
    assert_eq!(c.prev(), None);

    assert!(c.goto(125));
    for i in 125..150 {
        assert_eq!(c.remove(), Some(i));
        assert!(!c.is_empty());
        c.verify();
    }

    for i in (0..125).rev() {
        assert!(!c.is_empty());
        assert_eq!(c.elem(), None);
        assert_eq!(c.prev(), Some(i));
        assert_eq!(c.remove(), Some(i));
        c.verify();
    }
    assert_eq!(c.elem(), None);
    assert!(c.is_empty());
}

// Generate a densely populated 4-level tree.
//
// Level 1: 1 root
// Level 2: 8 inner
// Level 3: 64 inner
// Level 4: 512 leafs, up to 7680 elements
//
// A 3-level tree can hold at most 960 elements.
fn dense4l(f: &mut SetForest<i32>) -> Set<i32> {
    f.clear();
    let mut s = Set::new();

    // Insert 400 elements in 7 passes over the range to avoid the half-full leaf node pattern
    // that comes from sequential insertion. This will generate a normal leaf layer.
    for n in 0..4000 {
        assert!(s.insert((n * 7) % 4000, f, &()));
    }
    s
}

#[test]
fn four_level() {
    let mut f = SetForest::<i32>::new();
    let mut s = dense4l(&mut f);

    assert_eq!(s.iter(&f).collect::<Vec<_>>()[0..10], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);

    let mut c = s.cursor(&mut f, &());

    c.verify();

    // Peel off a whole sub-tree of the root by deleting from the front.
    // The 900 element is near the front of the second sub-tree.
    assert!(c.goto(900));
    assert_eq!(c.tpath(), "node48[1]--node47[0]--node26[0]--node20[4]");
    assert!(c.goto(0));
    for i in 0..900 {
        assert!(!c.is_empty());
        assert_eq!(c.remove(), Some(i));
    }
    c.verify();
    assert_eq!(c.elem(), Some(900));

    // Delete backwards from somewhere in the middle.
    assert!(c.goto(3000));
    for i in (2000..3000).rev() {
        assert_eq!(c.prev(), Some(i));
        assert_eq!(c.remove(), Some(i));
        assert_eq!(c.elem(), Some(3000));
    }
    c.verify();

    // Remove everything in a scattered manner, triggering many collapsing patterns.
    for i in 0..4000 {
        if c.goto((i * 7) % 4000) {
            c.remove();
        }
    }
    assert!(c.is_empty());
}

#[test]
fn four_level_clear() {
    let mut f = SetForest::<i32>::new();
    let mut s = dense4l(&mut f);
    s.clear(&mut f);
}
