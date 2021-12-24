//! This module defines the `Ty` and `Const` enums to represent
//! constants and types within the CFG. Note that these types are
//! not semantically equivalent to Verilog types:
//!
//! * Nested Arrays are represented as a simple flattened array to allow for faster/easier codegen
//! * Complex numbers are added to allow efficiently generating code for small signal analysis
//! * Unsized types (empty arrays) are not represented
//!
//! CFG instructions are strictly typed. That means that based upon the Op code alone uniquely
//! determines the following properties
//!
//! * the number of operands
//! * the types of the operands
//! * the type of the result
//!
//! As a result Ty is not used in Instructions. Instead it is only in local declarations.
//! However each operand could be a constant and therefore is possibly represented in any values of
//! the CFG. Furthermore a huge amount of constants is usually cloned and stored during const propagation.
//! As a result `Const` is heavily optimized for size and should be cheap to clone.
//!
//! This is achieved by using interned strings (lasso::Spur) to represent strings

use lasso::Spur;
use stdx::{impl_debug, impl_from_typed};

#[derive(Clone, Copy)]
pub enum Const {
    Real(f64),
    Int(i32),
    Bool(bool),
    String(Spur),
    Zst,
}

impl PartialEq for Const {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Manual eq implementation: Two constants are identical if they are bitwise identical!
            (Self::Real(l0), Self::Real(r0)) => l0.to_bits() == r0.to_bits(),
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            // (Self::Complex(l0), Self::Complex(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// Equivalence relation is defined as bit Equivalence for floats
impl Eq for Const {}

impl Const {
    pub fn unwrap_real(&self) -> f64 {
        if let Const::Real(val) = self {
            *val
        } else {
            unreachable!("called unwrap_real on {:?}", self)
        }
    }

    pub fn unwrap_int(&self) -> i32 {
        if let Const::Int(val) = self {
            *val
        } else {
            unreachable!("called unwrap_int on {:?}", self)
        }
    }

    pub fn unwrap_bool(&self) -> bool {
        if let Const::Bool(val) = self {
            *val
        } else {
            unreachable!("called unwrap_bool on {:?}", self)
        }
    }
}

impl_debug! {
    match Const{
        Const::Real(val) => "f64 {:?}", val;
        Const::Int(val) => "i32 {:?}", val;
        Const::Bool(val) => "{:?}", val;
        // Const::Complex(val) => "c64 {:?}", val;
        Const::String(val) => "str {:?}", val;
        // Const::RealArray(data) => "f64[] {:?}", &data.slice;
        // Const::IntArray(data) => "i32[] {:?}", &data.slice;
        // Const::ComplexArray(data) => "c64 {:?}", &data.slice;
        // Const::StringArray(data) => "str[] {:?}", &data.slice;
        Const::Zst => "[]";
    }
}

impl_from_typed!(
    Real(f64),
    Int(i32),
    Bool(bool),
    // Complex(Complex64),
    String(Spur)
    // RealArray(Array<f64>),
    // IntArray(Array<i32>),
    // ComplexArray(Array<Complex64>),
    // StringArray(Array<Spur>)
    for Const
);

// pub type Array<T> = ThinArc<(), T>;
