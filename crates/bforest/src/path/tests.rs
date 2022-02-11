use super::*;
use crate::{Forest, NodeData, NodePool};

use std::cmp::Ordering;
use std::fmt;

impl<F: Forest> Path<F> {
    /// Check the internal consistency of this path.
    pub fn verify(&self, pool: &NodePool<F>) {
        for level in 0..self.size {
            match pool[self.node[level]] {
                NodeData::Inner { size, tree, .. } => {
                    assert!(level < self.size - 1, "Expected leaf node at level {}", level);
                    assert!(
                        self.entry[level] <= size,
                        "OOB inner entry {}/{} at level {}",
                        self.entry[level],
                        size,
                        level
                    );
                    assert_eq!(
                        self.node[level + 1],
                        tree[usize::from(self.entry[level])],
                        "Node mismatch at level {}",
                        level
                    );
                }
                NodeData::Leaf { size, .. } => {
                    assert_eq!(level, self.size - 1, "Expected inner node");
                    assert!(
                        self.entry[level] <= size,
                        "OOB leaf entry {}/{}",
                        self.entry[level],
                        size,
                    );
                }
                NodeData::Free { .. } => {
                    panic!("Free {} in path", self.node[level]);
                }
            }
        }
    }
}

impl<F: Forest> fmt::Display for Path<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.size == 0 {
            write!(f, "<empty path>")
        } else {
            write!(f, "{}[{}]", self.node[0], self.entry[0])?;
            for i in 1..self.size {
                write!(f, "--{}[{}]", self.node[i], self.entry[i])?;
            }
            Ok(())
        }
    }
}

struct TC();

impl Comparator<i32> for TC {
    fn cmp(&self, a: i32, b: i32) -> Ordering {
        a.cmp(&b)
    }
}

struct TF();

impl Forest for TF {
    type Key = i32;
    type Value = char;
    type LeafKeys = [i32; 7];
    type LeafValues = [char; 7];

    fn splat_key(key: Self::Key) -> Self::LeafKeys {
        [key; 7]
    }

    fn splat_value(value: Self::Value) -> Self::LeafValues {
        [value; 7]
    }
}

#[test]
fn search_single_leaf() {
    // Testing Path::new() for trees with a single leaf node.
    let mut pool = NodePool::<TF>::new();
    let root = pool.alloc_node(NodeData::leaf(10, 'a'));
    let mut p = Path::default();
    let comp = TC();

    // Search for key less than stored key.
    assert_eq!(p.find(5, root, &pool, &comp), None);
    assert_eq!(p.size, 1);
    assert_eq!(p.node[0], root);
    assert_eq!(p.entry[0], 0);

    // Search for stored key.
    assert_eq!(p.find(10, root, &pool, &comp), Some('a'));
    assert_eq!(p.size, 1);
    assert_eq!(p.node[0], root);
    assert_eq!(p.entry[0], 0);

    // Search for key greater than stored key.
    assert_eq!(p.find(15, root, &pool, &comp), None);
    assert_eq!(p.size, 1);
    assert_eq!(p.node[0], root);
    assert_eq!(p.entry[0], 1);

    // Modify leaf node to contain two values.
    match pool[root] {
        NodeData::Leaf { ref mut size, ref mut keys, ref mut vals } => {
            *size = 2;
            keys[1] = 20;
            vals[1] = 'b';
        }
        _ => unreachable!(),
    }

    // Search for key between stored keys.
    assert_eq!(p.find(15, root, &pool, &comp), None);
    assert_eq!(p.size, 1);
    assert_eq!(p.node[0], root);
    assert_eq!(p.entry[0], 1);

    // Search for key greater than stored keys.
    assert_eq!(p.find(25, root, &pool, &comp), None);
    assert_eq!(p.size, 1);
    assert_eq!(p.node[0], root);
    assert_eq!(p.entry[0], 2);
}

#[test]
fn search_single_inner() {
    // Testing Path::new() for trees with a single inner node and two leaves.
    let mut pool = NodePool::<TF>::new();
    let leaf1 = pool.alloc_node(NodeData::leaf(10, 'a'));
    let leaf2 = pool.alloc_node(NodeData::leaf(20, 'b'));
    let root = pool.alloc_node(NodeData::inner(leaf1, 20, leaf2));
    let mut p = Path::default();
    let comp = TC();

    // Search for key less than stored keys.
    assert_eq!(p.find(5, root, &pool, &comp), None);
    assert_eq!(p.size, 2);
    assert_eq!(p.node[0], root);
    assert_eq!(p.entry[0], 0);
    assert_eq!(p.node[1], leaf1);
    assert_eq!(p.entry[1], 0);

    assert_eq!(p.find(10, root, &pool, &comp), Some('a'));
    assert_eq!(p.size, 2);
    assert_eq!(p.node[0], root);
    assert_eq!(p.entry[0], 0);
    assert_eq!(p.node[1], leaf1);
    assert_eq!(p.entry[1], 0);

    // Midway between the two leaf nodes.
    assert_eq!(p.find(15, root, &pool, &comp), None);
    assert_eq!(p.size, 2);
    assert_eq!(p.node[0], root);
    assert_eq!(p.entry[0], 0);
    assert_eq!(p.node[1], leaf1);
    assert_eq!(p.entry[1], 1);

    assert_eq!(p.find(20, root, &pool, &comp), Some('b'));
    assert_eq!(p.size, 2);
    assert_eq!(p.node[0], root);
    assert_eq!(p.entry[0], 1);
    assert_eq!(p.node[1], leaf2);
    assert_eq!(p.entry[1], 0);

    assert_eq!(p.find(25, root, &pool, &comp), None);
    assert_eq!(p.size, 2);
    assert_eq!(p.node[0], root);
    assert_eq!(p.entry[0], 1);
    assert_eq!(p.node[1], leaf2);
    assert_eq!(p.entry[1], 1);
}
