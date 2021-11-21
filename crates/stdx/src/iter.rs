use std::iter::Zip;

mod unziptuple;
pub use unziptuple::multiunzip;

pub fn zip<A, B>(a: A, b: B) -> Zip<A::IntoIter, B::IntoIter>
where
    A: IntoIterator,
    B: IntoIterator,
{
    a.into_iter().zip(b)
}
