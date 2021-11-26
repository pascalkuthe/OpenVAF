//! This module defines the `Ty` and `Const` enums to represent
//! constants and types within the CFG. Note that these types are
//! not semantically equivalent to Verilog types:
//!
//! * Nested Arrays are represented as a simple flattened array to allow for faster/easier codegen
//! * Complex numbers are added to allow efficently generating code for small signal analysis
//! * Unsized types (empty arrays) are not represented
//!
//! CFG instructions are strictly typed. That means that based upon the Op code alone uniqule
//! determines the following properties
//!
//! * the number of operands
//! * the types of the operands
//! * the type of the result
//!
//! As a result Ty is not used in Instructions. Instead it is only in local declarations.
//! However each operand could be a constant and therefore is possibly represented in any values of
//! the CFG. Furthermore a huge amount of constants is usually cloned and stored during const propagation.
//! As a result `Const` is heavily optimzed for size and should be cheap to clone.
//!
//! This is achieved by using interned strings (lasso::MiniSpur) to represent strings, thin arcs
//! (triomphe::ThinArc) for arrays and boxing complex numbers (to cut their size in half when not
//! used)

use lasso::MiniSpur;
use stdx::impl_debug;
use triomphe::ThinArc;

use crate::parse::{CfgParser, Parse, ParseFromStr};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Real,    // f64
    Int,     // i32
    Complex, // [f64,f64]
    String,  // *const c_str
    RealArray(u32),
    IntArray(u32),
    ComplexArray(u32),
    StringArray(u32),
}

#[derive(Clone, PartialEq, Copy)]
pub struct Complex64 {
    pub real: f64,
    pub imag: f64,
}
impl_debug! {
    match Complex64{
        Complex64{real,imag} => "{}, {}", real,imag;
    }
}

impl Parse for Complex64 {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        let real: ParseFromStr<f64> = p.parse()?;
        p.expect(",")?;
        let imag: ParseFromStr<f64> = p.parse()?;
        Ok(Complex64 { real: real.0, imag: imag.0 })
    }
}

#[derive(Clone, PartialEq)]
pub enum Const {
    Real(f64),
    Int(i32),
    Bool(bool),
    // TODO benchmark: Is this worth it?
    Complex(Box<Complex64>),
    String(MiniSpur),
    RealArray(Array<f64>),
    IntArray(Array<i32>),
    ComplexArray(Array<Complex64>),
    StringArray(Array<MiniSpur>),
}

impl_debug! {
    match Const{
        Const::Real(val) => "f64 {:?}", val;
        Const::Int(val) => "i32 {:?}", val;
        Const::Bool(val) => "{:?}", val;
        Const::Complex(val) => "c64 {:?}", val;
        Const::String(val) => "str {:?}", val;
        Const::RealArray(data) => "f64[] {:?}", &data.slice;
        Const::IntArray(data) => "i32[] {:?}", &data.slice;
        Const::ComplexArray(data) => "c64 {:?}", &data.slice;
        Const::StringArray(data) => "str[] {:?}", &data.slice;
    }
}

pub type Array<T> = ThinArc<(), T>;
