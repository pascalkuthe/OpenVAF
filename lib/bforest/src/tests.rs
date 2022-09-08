use super::*;

/// An opaque reference to a basic block in a function.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Block(u32);
impl_idx_from!(Block(u32));

#[test]
fn comparator() {
    let block1 = Block::from(1u32);
    let block2 = Block::from(2u32);
    let block3 = Block::from(3u32);
    let block4 = Block::from(4u32);
    let vals = [block1, block2, block4];
    assert_eq!(().search(block1, &vals), Ok(0));
    assert_eq!(().search(block3, &vals), Err(2));
    assert_eq!(().search(block4, &vals), Ok(2));
}

#[test]
fn slice_insertion() {
    let mut a = ['a', 'b', 'c', 'd'];

    slice_insert(&mut a[0..1], 0, 'e');
    assert_eq!(a, ['e', 'b', 'c', 'd']);

    slice_insert(&mut a, 0, 'a');
    assert_eq!(a, ['a', 'e', 'b', 'c']);

    slice_insert(&mut a, 3, 'g');
    assert_eq!(a, ['a', 'e', 'b', 'g']);

    slice_insert(&mut a, 1, 'h');
    assert_eq!(a, ['a', 'h', 'e', 'b']);
}

#[test]
fn slice_shifting() {
    let mut a = ['a', 'b', 'c', 'd'];

    slice_shift(&mut a[0..1], 1);
    assert_eq!(a, ['a', 'b', 'c', 'd']);

    slice_shift(&mut a[1..], 1);
    assert_eq!(a, ['a', 'c', 'd', 'd']);

    slice_shift(&mut a, 2);
    assert_eq!(a, ['d', 'd', 'd', 'd']);
}
