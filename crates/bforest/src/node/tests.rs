use super::*;
use core::mem;

// Forest impl for a set implementation.
struct TF();

impl Forest for TF {
    type Key = char;
    type Value = SetValue;
    type LeafKeys = [char; 15];
    type LeafValues = [SetValue; 15];

    fn splat_key(key: Self::Key) -> Self::LeafKeys {
        [key; 15]
    }

    fn splat_value(value: Self::Value) -> Self::LeafValues {
        [value; 15]
    }
}

#[test]
fn inner() {
    let n1 = Node(1);
    let n2 = Node(2);
    let n3 = Node(3);
    let n4 = Node(4);
    let mut inner = NodeData::<TF>::inner(n1, 'c', n4);
    assert_eq!(mem::size_of_val(&inner), 64);
    assert_eq!(inner.to_string(), "[ node1 c node4 ]");

    assert!(inner.try_inner_insert(0, 'a', n2));
    assert_eq!(inner.to_string(), "[ node1 a node2 c node4 ]");

    assert!(inner.try_inner_insert(1, 'b', n3));
    assert_eq!(inner.to_string(), "[ node1 a node2 b node3 c node4 ]");

    for i in 3..7 {
        assert!(inner.try_inner_insert(usize::from(i), (b'a' + i) as char, Node(i as u32 + 2),));
    }
    assert_eq!(
        inner.to_string(),
        "[ node1 a node2 b node3 c node4 d node5 e node6 f node7 g node8 ]"
    );

    // Now the node is full and insertion should fail anywhere.
    assert!(!inner.try_inner_insert(0, 'x', n3));
    assert!(!inner.try_inner_insert(4, 'x', n3));
    assert!(!inner.try_inner_insert(7, 'x', n3));

    // Splitting should be independent of the hint because we have an even number of node
    // references.
    let saved = inner;
    let sp = inner.split(1);
    assert_eq!(sp.lhs_entries, 4);
    assert_eq!(sp.rhs_entries, 4);
    assert_eq!(sp.crit_key, 'd');
    // The critical key is not present in either of the resulting nodes.
    assert_eq!(inner.to_string(), "[ node1 a node2 b node3 c node4 ]");
    assert_eq!(sp.rhs_data.to_string(), "[ node5 e node6 f node7 g node8 ]");

    assert_eq!(inner.inner_remove(0), Removed::Underflow);
    assert_eq!(inner.to_string(), "[ node2 b node3 c node4 ]");

    assert_eq!(inner.inner_remove(1), Removed::Underflow);
    assert_eq!(inner.to_string(), "[ node2 c node4 ]");

    assert_eq!(inner.inner_remove(1), Removed::Underflow);
    assert_eq!(inner.to_string(), "[ node2 ]");

    assert_eq!(inner.inner_remove(0), Removed::Empty);

    inner = saved;
    let sp = inner.split(6);
    assert_eq!(sp.lhs_entries, 4);
    assert_eq!(sp.rhs_entries, 4);
    assert_eq!(sp.crit_key, 'd');
    assert_eq!(inner.to_string(), "[ node1 a node2 b node3 c node4 ]");
    assert_eq!(sp.rhs_data.to_string(), "[ node5 e node6 f node7 g node8 ]");
}

#[test]
fn leaf() {
    let mut leaf = NodeData::<TF>::leaf('d', SetValue());
    assert_eq!(leaf.to_string(), "[ d ]");

    assert!(leaf.try_leaf_insert(0, 'a', SetValue()));
    assert_eq!(leaf.to_string(), "[ a d ]");
    assert!(leaf.try_leaf_insert(1, 'b', SetValue()));
    assert!(leaf.try_leaf_insert(2, 'c', SetValue()));
    assert_eq!(leaf.to_string(), "[ a b c d ]");
    for i in 4..15 {
        assert!(leaf.try_leaf_insert(usize::from(i), (b'a' + i) as char, SetValue()));
    }
    assert_eq!(leaf.to_string(), "[ a b c d e f g h i j k l m n o ]");

    // Now the node is full and insertion should fail anywhere.
    assert!(!leaf.try_leaf_insert(0, 'x', SetValue()));
    assert!(!leaf.try_leaf_insert(8, 'x', SetValue()));
    assert!(!leaf.try_leaf_insert(15, 'x', SetValue()));

    // The index given to `split` is not the split position, it's a hint for balancing the node.
    let saved = leaf;
    let sp = leaf.split(12);
    assert_eq!(sp.lhs_entries, 8);
    assert_eq!(sp.rhs_entries, 7);
    assert_eq!(sp.crit_key, 'i');
    assert_eq!(leaf.to_string(), "[ a b c d e f g h ]");
    assert_eq!(sp.rhs_data.to_string(), "[ i j k l m n o ]");

    assert!(leaf.try_leaf_insert(8, 'i', SetValue()));
    assert_eq!(leaf.leaf_remove(2), Removed::Healthy);
    assert_eq!(leaf.to_string(), "[ a b d e f g h i ]");
    assert_eq!(leaf.leaf_remove(7), Removed::Underflow);
    assert_eq!(leaf.to_string(), "[ a b d e f g h ]");

    leaf = saved;
    let sp = leaf.split(7);
    assert_eq!(sp.lhs_entries, 7);
    assert_eq!(sp.rhs_entries, 8);
    assert_eq!(sp.crit_key, 'h');
    assert_eq!(leaf.to_string(), "[ a b c d e f g ]");
    assert_eq!(sp.rhs_data.to_string(), "[ h i j k l m n o ]");
}

#[test]
fn optimal_split_pos() {
    // An even split is easy.
    assert_eq!(split_pos(8, 0), 4);
    assert_eq!(split_pos(8, 8), 4);

    // Easy cases for odd splits.
    assert_eq!(split_pos(7, 0), 3);
    assert_eq!(split_pos(7, 7), 4);

    // If the insertion point is the same as the split position, we
    // will append to the lhs node.
    assert_eq!(split_pos(7, 3), 3);
    assert_eq!(split_pos(7, 4), 4);
}

#[test]
fn inner_balance() {
    let n1 = Node(1);
    let n2 = Node(2);
    let n3 = Node(3);
    let mut lhs = NodeData::<TF>::inner(n1, 'a', n2);
    assert!(lhs.try_inner_insert(1, 'b', n3));
    assert_eq!(lhs.to_string(), "[ node1 a node2 b node3 ]");

    let n11 = Node(11);
    let n12 = Node(12);
    let mut rhs = NodeData::<TF>::inner(n11, 'p', n12);

    for i in 1..4 {
        assert!(rhs.try_inner_insert(usize::from(i), (b'p' + i) as char, Node(i as u32 + 12),));
    }
    assert_eq!(rhs.to_string(), "[ node11 p node12 q node13 r node14 s node15 ]");

    // 3+5 elements fit in RHS.
    assert_eq!(lhs.balance('o', &mut rhs), None);
    assert_eq!(
        rhs.to_string(),
        "[ node1 a node2 b node3 o node11 p node12 q node13 r node14 s node15 ]"
    );

    // 2+8 elements are redistributed.
    lhs = NodeData::<TF>::inner(Node(20), 'x', Node(21));
    assert_eq!(lhs.balance('y', &mut rhs), Some('o'));
    assert_eq!(lhs.to_string(), "[ node20 x node21 y node1 a node2 b node3 ]");
    assert_eq!(rhs.to_string(), "[ node11 p node12 q node13 r node14 s node15 ]");
}

#[test]
fn leaf_balance() {
    let mut lhs = NodeData::<TF>::leaf('a', SetValue());
    for i in 1..6 {
        assert!(lhs.try_leaf_insert(usize::from(i), (b'a' + i) as char, SetValue()));
    }
    assert_eq!(lhs.to_string(), "[ a b c d e f ]");

    let mut rhs = NodeData::<TF>::leaf('0', SetValue());
    for i in 1..8 {
        assert!(rhs.try_leaf_insert(usize::from(i), (b'0' + i) as char, SetValue()));
    }
    assert_eq!(rhs.to_string(), "[ 0 1 2 3 4 5 6 7 ]");

    // 6+8 elements all fits in rhs.
    assert_eq!(lhs.balance('0', &mut rhs), None);
    assert_eq!(rhs.to_string(), "[ a b c d e f 0 1 2 3 4 5 6 7 ]");

    assert!(lhs.try_leaf_insert(0, 'x', SetValue()));
    assert!(lhs.try_leaf_insert(1, 'y', SetValue()));
    assert!(lhs.try_leaf_insert(2, 'z', SetValue()));
    assert_eq!(lhs.to_string(), "[ x y z ]");

    // 3+14 elements need redistribution.
    assert_eq!(lhs.balance('a', &mut rhs), Some('0'));
    assert_eq!(lhs.to_string(), "[ x y z a b c d e f ]");
    assert_eq!(rhs.to_string(), "[ 0 1 2 3 4 5 6 7 ]");
}
