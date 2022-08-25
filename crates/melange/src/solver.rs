use std::rc::Rc;

use klu_rs::{FixedKluMatrix, KluMatrixBuilder, KluMatrixSpec};
use num_complex::Complex64;

pub type RealMatrix = FixedKluMatrix<i32, f64>;
pub type ComplexMatrix = FixedKluMatrix<i32, Complex64>;
pub type MatrixBuilder = KluMatrixBuilder<i32>;
pub type MatrixSpec = Rc<KluMatrixSpec<i32>>;
