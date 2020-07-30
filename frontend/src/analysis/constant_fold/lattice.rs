use crate::analysis::constant_fold::lattice::DiamondLattice::{NotAConstant, Unknown, Val};
use crate::ir::ids::VariableId;
use crate::StringLiteral;
use bitflags::_core::ops::{BitAnd, BitOr, BitXor, Not, Rem, Shl};
use core::ops::{Add, Div, Mul, Neg, Sub};
use index_vec::IndexVec;
use num_traits::{One, Pow, Zero};
use std::fmt::Debug;
use std::ops::Shr;

pub type ProductLattice = IndexVec<VariableId, TypedDiamondLattice>;

// TODO properly implement equality for StringLiterals
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypedDiamondLattice {
    Unknown,
    String(StringLiteral),
    Real(f64),
    Integer(i64),
    NotAConstant,
}

impl TypedDiamondLattice {
    pub fn meet(self, other: Self) -> Self {
        match (self, other) {
            (x, y) if x == y => x,
            (TypedDiamondLattice::Unknown, res) | (res, TypedDiamondLattice::Unknown) => res,
            _ => TypedDiamondLattice::NotAConstant,
        }
    }
}

impl From<DiamondLattice<f64>> for TypedDiamondLattice {
    fn from(x: DiamondLattice<f64>) -> Self {
        match x {
            DiamondLattice::Unknown => Self::Unknown,
            DiamondLattice::NotAConstant => Self::NotAConstant,
            DiamondLattice::Val(val) => Self::Real(val),
        }
    }
}

impl From<DiamondLattice<i64>> for TypedDiamondLattice {
    fn from(x: DiamondLattice<i64>) -> Self {
        match x {
            DiamondLattice::Unknown => Self::Unknown,
            DiamondLattice::NotAConstant => Self::NotAConstant,
            DiamondLattice::Val(val) => Self::Integer(val),
        }
    }
}

impl From<DiamondLattice<StringLiteral>> for TypedDiamondLattice {
    fn from(x: DiamondLattice<StringLiteral>) -> Self {
        match x {
            DiamondLattice::Unknown => Self::Unknown,
            DiamondLattice::NotAConstant => Self::NotAConstant,
            DiamondLattice::Val(val) => Self::String(val),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DiamondLattice<T: Copy> {
    Unknown,
    Val(T),
    NotAConstant,
}

impl From<TypedDiamondLattice> for DiamondLattice<f64> {
    fn from(lattice: TypedDiamondLattice) -> Self {
        match lattice {
            TypedDiamondLattice::Unknown => DiamondLattice::Unknown,
            TypedDiamondLattice::Real(val) => DiamondLattice::Val(val),
            _ => DiamondLattice::NotAConstant,
        }
    }
}

impl From<TypedDiamondLattice> for DiamondLattice<i64> {
    fn from(lattice: TypedDiamondLattice) -> Self {
        match lattice {
            TypedDiamondLattice::Unknown => DiamondLattice::Unknown,
            TypedDiamondLattice::Integer(val) => DiamondLattice::Val(val),
            _ => DiamondLattice::NotAConstant,
        }
    }
}

impl From<TypedDiamondLattice> for DiamondLattice<StringLiteral> {
    fn from(lattice: TypedDiamondLattice) -> Self {
        match lattice {
            TypedDiamondLattice::Unknown => DiamondLattice::Unknown,
            TypedDiamondLattice::String(val) => DiamondLattice::Val(val),
            _ => DiamondLattice::NotAConstant,
        }
    }
}

impl<T: Copy> DiamondLattice<T> {
    pub fn apply_binary_op<Y: Copy>(
        self,
        f: impl FnOnce(T, T) -> Y,
        other: Self,
    ) -> DiamondLattice<Y> {
        match (self, other) {
            (Val(arg1), Val(arg2)) => Val(f(arg1, arg2)),
            (NotAConstant, _) | (_, NotAConstant) => NotAConstant,
            _ => Unknown,
        }
    }

    pub fn map<Y: Copy>(self, f: impl FnOnce(T) -> Y) -> DiamondLattice<Y> {
        match self {
            Val(x) => Val(f(x)),
            NotAConstant => NotAConstant,
            Unknown => Unknown,
        }
    }

    pub fn and_then<Y: Copy>(self, f: impl FnOnce(T) -> DiamondLattice<Y>) -> DiamondLattice<Y> {
        match self {
            Val(x) => f(x),
            NotAConstant => NotAConstant,
            Unknown => Unknown,
        }
    }
}

impl<T: Debug + Copy> DiamondLattice<T> {
    pub fn expect(self, msg: &'static str) -> T {
        if let Val(val) = self {
            val
        } else {
            panic!("Expected a constant value bound found {:?}: {}", self, msg)
        }
    }

    pub fn unwrap(self) -> T {
        if let Val(val) = self {
            val
        } else {
            panic!("Expected a constant value bound found {:?}!", self)
        }
    }
}

impl<T: Neg<Output = T> + Copy> Neg for DiamondLattice<T> {
    type Output = Self;

    fn neg(self) -> Self {
        self.map(T::neg)
    }
}

impl<T: Add<Output = T> + Copy> Add for DiamondLattice<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        self.apply_binary_op(T::add, other)
    }
}

impl<T: Sub<Output = T> + Copy> Sub for DiamondLattice<T> {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        self.apply_binary_op(T::sub, other)
    }
}

impl<T: Rem<Output = T> + Copy> Rem for DiamondLattice<T> {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        self.apply_binary_op(T::rem, other)
    }
}

impl<T: Shl<Output = T> + Copy> Shl for DiamondLattice<T> {
    type Output = Self;

    fn shl(self, other: Self) -> Self {
        self.apply_binary_op(T::shl, other)
    }
}

impl<T: Shr<Output = T> + Copy> Shr for DiamondLattice<T> {
    type Output = Self;

    fn shr(self, other: Self) -> Self {
        self.apply_binary_op(T::shr, other)
    }
}

impl<T: BitXor<Output = T> + Copy> BitXor for DiamondLattice<T> {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self {
        self.apply_binary_op(T::bitxor, other)
    }
}

impl<T: BitAnd<Output = T> + Copy + Zero> BitAnd for DiamondLattice<T> {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        match (self, other) {
            (Val(x), _) | (_, Val(x)) if x.is_zero() => Val(T::zero()),
            _ => self.apply_binary_op(T::bitand, other),
        }
    }
}

impl<T: BitOr<Output = T> + Copy> BitOr for DiamondLattice<T> {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        self.apply_binary_op(T::bitor, other)
    }
}

impl<T: Not<Output = T> + Copy> Not for DiamondLattice<T> {
    type Output = Self;

    fn not(self) -> Self {
        self.map(T::not)
    }
}

impl<T: Mul<Output = T> + Copy + Zero> Mul for DiamondLattice<T> {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Val(arg1), Val(arg2)) => Val(arg1 * arg2),

            (_, Val(arg)) | (Val(arg), _) if arg.is_zero() => Val(T::zero()),

            (NotAConstant, _) | (_, NotAConstant) => NotAConstant,

            _ => Unknown,
        }
    }
}

impl<T: Div<Output = T> + Copy + Zero> Div for DiamondLattice<T> {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Val(arg1), Val(arg2)) => Val(arg1 / arg2),

            (Val(arg), _) if arg.is_zero() => Val(T::zero()),

            (NotAConstant, _) | (_, NotAConstant) => NotAConstant,

            _ => Unknown,
        }
    }
}

impl<E: Copy + Zero, T: Pow<E, Output = T> + Copy + Zero + One + PartialEq> Pow<DiamondLattice<E>>
    for DiamondLattice<T>
{
    type Output = Self;

    fn pow(self, other: DiamondLattice<E>) -> Self::Output {
        match (self, other) {
            (Val(arg1), Val(arg2)) => Val(arg1.pow(arg2)),

            (_, Val(arg2)) if arg2.is_zero() => Val(T::one()),

            (Val(arg1), _) if arg1.is_zero() => Val(T::zero()),

            (Val(arg1), _) if arg1.is_one() => Val(T::one()),

            (NotAConstant, _) | (_, NotAConstant) => NotAConstant,

            _ => Unknown,
        }
    }
}
