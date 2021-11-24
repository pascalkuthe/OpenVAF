#![allow(clippy::float_cmp)]

use cfg::{Const, Place};
use data_flow::lattice::SparseFlatSetMap;

mod ops;
mod ssa_constants;

pub type BlockConsts = SparseFlatSetMap<Place, Const>;

// pub use propagation::ConstantPropagation;
// use std::mem::size_of_val;

// macro_rules! undefined_operation {
//     ($op: expr, $arg: expr) => {
//         unreachable!("Operation {} not defined for {:?}", $op, $arg)
//     };
//     ($op: expr, $lhs: expr, $rhs: expr) => {
//         unreachable!("Operation {} not defined for {:?} and {:?}", $op, $lhs, $rhs)
//     };
// }

// pub type GlobalConsts = SparseFlatSetMap<Place, Const>;
// struct ConstantFold<'a> {
//     locals: &'a BlockConsts,
// }

// impl<'lt, R: CallResolver, F: Fn(Local) -> FlatSet<ConstVal>> ConstantFold<'lt, R, F> {
//     fn resolve_rvalue(&mut self, rvalue: &RValue<R::C>, ty: Type) -> FlatSet<ConstVal> {
//         fold_rvalue(self, rvalue, ty)
//     }

//     fn resolve_operand(&self, op: &CallResolverOperand<R>) -> FlatSet<ConstVal> {
//         match op.contents {
//             OperandData::Constant(ref val) => FlatSet::Elem(val.clone()),
//             OperandData::Copy(local) => (self.resolve_special_locals)(local)
//                 .map_bottom(|| self.locals.get_cloned_flat_set(local)),

//             OperandData::Read(ref input) => self.resolver.resolve_input(input),
//         }
//     }

//     fn eval_real(
//         &self,
//         arg: &CallResolverOperand<R>,
//         eval: impl FnOnce(f64) -> f64,
//         op: &'static str,
//     ) -> FlatSet<ConstVal> {
//         self.resolve_operand(arg).map(|arg| {
//             if let Scalar(Real(val)) = arg {
//                 Scalar(Real(eval(val)))
//             } else {
//                 undefined_operation!(op, arg)
//             }
//         })
//     }

//     fn eval_cmplx(
//         &self,
//         arg: &CallResolverOperand<R>,
//         eval: impl FnOnce(Complex64) -> Complex64,
//         op: &'static str,
//     ) -> FlatSet<ConstVal> {
//         self.resolve_operand(arg).map(|arg| {
//             if let Scalar(Cmplx(val)) = arg {
//                 Scalar(Cmplx(eval(val)))
//             } else {
//                 undefined_operation!(op, arg)
//             }
//         })
//     }

//     fn eval_int(
//         &self,
//         arg: &CallResolverOperand<R>,
//         eval: impl FnOnce(i64) -> i64,
//         op: &'static str,
//     ) -> FlatSet<ConstVal> {
//         self.resolve_operand(arg).map(|arg| {
//             if let Scalar(Integer(val)) = arg {
//                 Scalar(Integer(eval(val)))
//             } else {
//                 undefined_operation!(op, arg)
//             }
//         })
//     }

//     fn eval_bool(
//         &self,
//         arg: &CallResolverOperand<R>,
//         eval: impl FnOnce(bool) -> bool,
//         op: &'static str,
//     ) -> FlatSet<ConstVal> {
//         self.resolve_operand(arg).map(|arg| {
//             if let Scalar(Bool(val)) = arg {
//                 Scalar(Bool(eval(val)))
//             } else {
//                 undefined_operation!(op, arg)
//             }
//         })
//     }

//     fn eval_real_comparison(
//         &self,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//         eval: impl FnOnce(f64, f64) -> bool,
//         op: &'static str,
//     ) -> FlatSet<ConstVal> {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
//             (Scalar(Real(lhs)), Scalar(Real(rhs))) => Scalar(Bool(eval(lhs, rhs))),
//             (lhs, rhs) => undefined_operation!(op, lhs, rhs),
//         })
//     }

//     fn eval_int_comparison(
//         &self,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//         eval: impl FnOnce(i64, i64) -> bool,
//         op: &'static str,
//     ) -> FlatSet<ConstVal> {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
//             (Scalar(Integer(lhs)), Scalar(Integer(rhs))) => Scalar(Bool(eval(lhs, rhs))),
//             (lhs, rhs) => undefined_operation!(op, lhs, rhs),
//         })
//     }

//     fn eval_bin_real(
//         &self,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//         eval: impl FnOnce(f64, f64) -> f64,
//         op: &'static str,
//     ) -> FlatSet<ConstVal> {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
//             (Scalar(Real(lhs)), Scalar(Real(rhs))) => Scalar(Real(eval(lhs, rhs))),
//             (lhs, rhs) => undefined_operation!(op, lhs, rhs),
//         })
//     }

//     fn eval_bin_int(
//         &self,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//         eval: impl FnOnce(i64, i64) -> i64,
//         op: &'static str,
//     ) -> FlatSet<ConstVal> {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
//             (Scalar(Integer(lhs)), Scalar(Integer(rhs))) => Scalar(Integer(eval(lhs, rhs))),
//             (lhs, rhs) => undefined_operation!(op, lhs, rhs),
//         })
//     }

//     fn eval_bin_bool(
//         &self,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//         eval: impl FnOnce(bool, bool) -> bool,
//         op: &'static str,
//     ) -> FlatSet<ConstVal> {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
//             (Scalar(Bool(lhs)), Scalar(Bool(rhs))) => Scalar(Bool(eval(lhs, rhs))),
//             (lhs, rhs) => undefined_operation!(op, lhs, rhs),
//         })
//     }
// }

// impl<'lt, R: CallResolver, F: Fn(Local) -> FlatSet<ConstVal>> RValueFold<R::C>
//     for ConstantFold<'lt, R, F>
// {
//     type T = FlatSet<ConstVal>;

//     fn fold_cmplx_arith_negate(&mut self, _op: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_cmplx(arg, Complex64::neg, "Complex Negate")
//     }

//     fn fold_real_arith_negate(&mut self, _op: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::neg, "Real Negate")
//     }

//     fn fold_bit_negate(&mut self, _op: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_int(arg, i64::reverse_bits, "Bit Negate")
//     }

//     fn fold_int_arith_negate(&mut self, _op: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_int(arg, i64::neg, "Integer Negate")
//     }

//     fn fold_logic_negate(&mut self, _op: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_bool(arg, |arg| !arg, "Integer Negate")
//     }

//     fn fold_cmplx_add(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
//             (Scalar(Cmplx(lhs)), Scalar(Cmplx(rhs))) => Scalar(Cmplx(lhs + rhs)),
//             (Scalar(Cmplx(lhs)), Scalar(Real(rhs))) => Scalar(Cmplx(lhs + rhs)),
//             (Scalar(Real(lhs)), Scalar(Cmplx(rhs))) => Scalar(Cmplx(lhs + rhs)),
//             (lhs, rhs) => undefined_operation!("Complex Summation", lhs, rhs),
//         })
//     }

//     fn fold_cmplx_sub(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         lhs.apply_binary_op(rhs, |lhs, rhs| match (lhs, rhs) {
//             (Scalar(Cmplx(lhs)), Scalar(Cmplx(rhs))) => Scalar(Cmplx(lhs - rhs)),
//             (Scalar(Cmplx(lhs)), Scalar(Real(rhs))) => Scalar(Cmplx(lhs - rhs)),
//             (Scalar(Real(lhs)), Scalar(Cmplx(rhs))) => Scalar(Cmplx(lhs - rhs)),
//             (lhs, rhs) => undefined_operation!("Complex Subtraction", lhs, rhs),
//         })
//     }

//     fn fold_cmplx_mul(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         match (lhs, rhs) {
//             (Elem(Scalar(Cmplx(arg1))), Elem(Scalar(Cmplx(arg2)))) => {
//                 Elem(Scalar(Cmplx(arg1 * arg2)))
//             }
//             (Elem(Scalar(Cmplx(arg1))), Elem(Scalar(Real(arg2)))) => {
//                 Elem(Scalar(Cmplx(arg1 * arg2)))
//             }
//             (Elem(Scalar(Real(arg1))), Elem(Scalar(Cmplx(arg2)))) => {
//                 Elem(Scalar(Cmplx(arg1 * arg2)))
//             }

//             (Elem(arg1), Elem(arg2)) => {
//                 undefined_operation!("Complex Multiplication", arg1, arg2)
//             }

//             (Elem(Scalar(Cmplx(arg))), _) | (_, Elem(Scalar(Cmplx(arg))))
//                 if arg == Complex64::new(0.0, 0.0) =>
//             {
//                 Elem(Scalar(Cmplx(Complex64::new(0.0, 0.0))))
//             }

//             (Elem(Scalar(Real(arg))), _) | (_, Elem(Scalar(Real(arg)))) if arg == 0.0 => {
//                 Elem(Scalar(Cmplx(Complex64::new(0.0, 0.0))))
//             }

//             (Top, _) | (_, Top) => Top,

//             (_, _) => Bottom,
//         }
//     }

//     fn fold_cmplx_div(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         match (lhs, rhs) {
//             (Elem(Scalar(Cmplx(arg1))), Elem(Scalar(Cmplx(arg2)))) => {
//                 Elem(Scalar(Cmplx(arg1 / arg2)))
//             }
//             (Elem(Scalar(Real(arg1))), Elem(Scalar(Cmplx(arg2)))) => {
//                 Elem(Scalar(Cmplx(arg1 / arg2)))
//             }
//             (Elem(Scalar(Cmplx(arg1))), Elem(Scalar(Real(arg2)))) => {
//                 Elem(Scalar(Cmplx(arg1 / arg2)))
//             }

//             (Elem(arg1), Elem(arg2)) => {
//                 undefined_operation!("Complex Division", arg1, arg2)
//             }

//             (Elem(Scalar(Cmplx(arg))), _) if arg == Complex64::new(0.0, 0.0) => {
//                 Elem(Scalar(Cmplx(Complex64::new(0.0, 0.0))))
//             }

//             (Elem(Scalar(Real(arg))), _) if arg == 0.0 => {
//                 Elem(Scalar(Cmplx(Complex64::new(0.0, 0.0))))
//             }

//             (Top, _) | (_, Top) => Top,

//             (_, _) => Bottom,
//         }
//     }

//     fn fold_real_add(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_real(lhs, rhs, f64::add, "Real Summation")
//     }

//     fn fold_real_sub(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_real(lhs, rhs, f64::sub, "Real Subtraction")
//     }

//     fn fold_real_mul(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         match (lhs, rhs) {
//             (Elem(Scalar(Real(arg1))), Elem(Scalar(Real(arg2)))) => Elem(Scalar(Real(arg1 * arg2))),

//             (Elem(arg1), Elem(arg2)) => {
//                 undefined_operation!("Real Multiplication", arg1, arg2)
//             }

//             (Elem(Scalar(Real(arg))), _) | (_, Elem(Scalar(Real(arg)))) if arg == 0.0 => {
//                 Elem(Scalar(Real(0.0)))
//             }

//             (Top, _) | (_, Top) => Top,

//             _ => Bottom,
//         }
//     }

//     fn fold_real_div(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         match (lhs, rhs) {
//             (Elem(Scalar(Real(arg1))), Elem(Scalar(Real(arg2)))) => Elem(Scalar(Real(arg1 / arg2))),

//             (Elem(arg1), Elem(arg2)) => undefined_operation!("Real Division", arg1, arg2),

//             (Elem(Scalar(Real(arg))), _) if arg == 0.0 => Elem(Scalar(Real(0.0))),

//             (Top, _) | (_, Top) => Top,

//             _ => Bottom,
//         }
//     }

//     fn fold_real_rem(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_real(lhs, rhs, f64::rem, "Real Remainder")
//     }

//     fn fold_int_add(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(lhs, rhs, i64::add, "Integer Subtraction")
//     }

//     fn fold_int_sub(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(lhs, rhs, i64::sub, "Integer Subtraction")
//     }

//     fn fold_int_mul(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         match (lhs, rhs) {
//             (Elem(Scalar(Integer(arg1))), Elem(Scalar(Integer(arg2)))) => {
//                 Elem(Scalar(Integer(arg1 * arg2)))
//             }

//             (Elem(arg1), Elem(arg2)) => {
//                 undefined_operation!("Integer Multiplication", arg1, arg2)
//             }

//             (Elem(Scalar(Integer(0))), _) | (_, Elem(Scalar(Integer(0)))) => {
//                 Elem(Scalar(Integer(0)))
//             }

//             (Top, _) | (_, Top) => Top,

//             _ => Bottom,
//         }
//     }

//     fn fold_int_div(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         match (lhs, rhs) {
//             (Elem(Scalar(Integer(arg1))), Elem(Scalar(Integer(arg2)))) => {
//                 Elem(Scalar(Integer(arg1 / arg2)))
//             }

//             (Elem(arg1), Elem(arg2)) => undefined_operation!("Real Division", arg1, arg2),

//             (Elem(Scalar(Integer(0))), _) => Elem(Scalar(Integer(0))),

//             (Top, _) | (_, Top) => Top,

//             _ => Bottom,
//         }
//     }

//     fn fold_int_rem(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(lhs, rhs, i64::rem, "Integer Remainder")
//     }

//     fn fold_shiftl(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(lhs, rhs, i64::shl, "Integer Shift Left")
//     }

//     fn fold_shiftr(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(lhs, rhs, i64::shr, "Integer Shift Right")
//     }

//     fn fold_lt_real(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 < arg2, "Real Less Than")
//     }

//     fn fold_le_real(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 <= arg2, "Real Less Equal")
//     }

//     fn fold_gt_real(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 > arg2, "Real Greater than")
//     }

//     fn fold_ge_real(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_real_comparison(lhs, rhs, |arg1, arg2| arg1 >= arg2, "Real Greater Equal")
//     }

//     fn fold_lt_int(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_int_comparison(lhs, rhs, |arg1, arg2| arg1 < arg2, "Integer Less Then")
//     }

//     fn fold_le_int(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_int_comparison(lhs, rhs, |arg1, arg2| arg1 <= arg2, "Integer Less Equal")
//     }

//     fn fold_gt_int(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_int_comparison(lhs, rhs, |arg1, arg2| arg1 > arg2, "Integer Greater Then")
//     }

//     fn fold_ge_int(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_int_comparison(lhs, rhs, |arg1, arg2| arg1 >= arg2, "Integer Greater Equal")
//     }

//     fn fold_eq(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//         _ty: Type,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         lhs.apply_binary_op(rhs, |x, y| Scalar(Bool(x == y)))
//     }

//     fn fold_ne(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//         _ty: Type,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(lhs);
//         let rhs = self.resolve_operand(rhs);
//         lhs.apply_binary_op(rhs, |x, y| Scalar(Bool(x != y)))
//     }

//     fn fold_xor(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(lhs, rhs, i64::bitxor, "Integer XOR")
//     }

//     fn fold_nxor(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(lhs, rhs, |lhs, rhs| !(lhs ^ rhs), "Integer NXOR")
//     }

//     fn fold_and(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(lhs, rhs, i64::bitand, "Integer NXOR")
//     }

//     fn fold_or(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(lhs, rhs, i64::bitor, "Integer NXOR")
//     }

//     fn fold_bool_xor(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_bool(lhs, rhs, bool::bitxor, "Integer NXOR")
//     }

//     fn fold_bool_nxor(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_bool(lhs, rhs, |lhs, rhs| !(lhs ^ rhs), "Integer NXOR")
//     }

//     fn fold_bool_and(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_bool(lhs, rhs, bool::bitand, "Integer NXOR")
//     }

//     fn fold_bool_or(
//         &mut self,
//         _op: Span,
//         lhs: &CallResolverOperand<R>,
//         rhs: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_bool(lhs, rhs, bool::bitor, "Integer NXOR")
//     }

//     fn fold_exp(&mut self, _span: Span, arg: &CallResolverOperand<R>, _limit: bool) -> Self::T {
//         self.eval_real(arg, f64::exp, "EXP")
//     }

//     fn fold_ln(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::ln, "LN")
//     }

//     fn fold_log(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::log10, "LOG")
//     }

//     fn fold_clog2(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_int(
//             arg,
//             |val| 8 * size_of_val(&val) as i64 - val.abs().leading_zeros() as i64,
//             "clog2",
//         )
//     }

//     fn fold_sqrt(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::sqrt, "SQRT")
//     }

//     fn fold_cmplx_abs(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_cmplx(arg, |x| x.norm().into(), "CMPLX ABS")
//     }

//     fn fold_real_abs(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::abs, "Real ABS")
//     }

//     fn fold_int_abs(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_int(arg, i64::abs, "Integer ABS")
//     }

//     fn fold_ceil(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::ceil, "Real Ceil")
//     }

//     fn fold_floor(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::floor, "Real Flor")
//     }

//     fn fold_sin(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::sin, "Real Sin")
//     }

//     fn fold_cos(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::cos, "Real Cos")
//     }

//     fn fold_tan(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::tan, "Real Tan")
//     }

//     fn fold_sinh(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::sinh, "Real SinH")
//     }

//     fn fold_cosh(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::cosh, "Real CosH")
//     }

//     fn fold_tanh(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::tanh, "Real TanH")
//     }

//     fn fold_asin(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::asin, "Real ASIN")
//     }

//     fn fold_acos(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::acos, "Real ACOS")
//     }

//     fn fold_atan(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::atan, "Real ATAN")
//     }

//     fn fold_asinh(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::asin, "Real ASINH")
//     }

//     fn fold_acosh(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::acosh, "Real ACOSH")
//     }

//     fn fold_atanh(&mut self, _span: Span, arg: &CallResolverOperand<R>) -> Self::T {
//         self.eval_real(arg, f64::atanh, "Real ATANH")
//     }

//     fn fold_pow(
//         &mut self,
//         _span: Span,
//         arg1: &CallResolverOperand<R>,
//         arg2: &CallResolverOperand<R>,
//     ) -> Self::T {
//         let lhs = self.resolve_operand(arg1);
//         let rhs = self.resolve_operand(arg2);
//         match (lhs, rhs) {
//             (Elem(Scalar(Real(arg1))), Elem(Scalar(Real(arg2)))) => {
//                 Elem(Scalar(Real(arg1.powf(arg2))))
//             }

//             (Elem(arg1), Elem(arg2)) => undefined_operation!("POW", arg1, arg2),

//             (Elem(Scalar(Real(arg))), _) if arg == 0.0 => Elem(Scalar(Real(0.0))),
//             (Elem(Scalar(Real(arg))), _) if arg == 1.0 => Elem(Scalar(Real(1.0))),

//             (_, Elem(Scalar(Real(arg)))) if arg == 0.0 => Elem(Scalar(Real(1.0))),

//             (Top, _) | (_, Top) => Top,

//             _ => Bottom,
//         }
//     }

//     fn fold_hypot(
//         &mut self,
//         _span: Span,
//         arg1: &CallResolverOperand<R>,
//         arg2: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_real(arg1, arg2, f64::hypot, "HYPOT")
//     }

//     fn fold_real_min(
//         &mut self,
//         _span: Span,
//         arg1: &CallResolverOperand<R>,
//         arg2: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_real(arg1, arg2, f64::min, "Real Min")
//     }

//     fn fold_int_min(
//         &mut self,
//         _span: Span,
//         arg1: &CallResolverOperand<R>,
//         arg2: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(arg1, arg2, i64::min, "Integer Min")
//     }

//     fn fold_real_max(
//         &mut self,
//         _span: Span,
//         arg1: &CallResolverOperand<R>,
//         arg2: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_real(arg1, arg2, f64::max, "Real Max")
//     }

//     fn fold_int_max(
//         &mut self,
//         _span: Span,
//         arg1: &CallResolverOperand<R>,
//         arg2: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_int(arg1, arg2, i64::max, "Integer Max")
//     }

//     fn fold_atan2(
//         &mut self,
//         _span: Span,
//         arg1: &CallResolverOperand<R>,
//         arg2: &CallResolverOperand<R>,
//     ) -> Self::T {
//         self.eval_bin_real(arg1, arg2, f64::atan2, "ATAN2")
//     }

//     fn fold_cast(&mut self, arg: &CallResolverOperand<R>, dst: Type) -> Self::T {
//         let arg = self.resolve_operand(arg);
//         arg.map(|arg| {
//             match (dst, arg) {
//                 (Type::REAL, Scalar(Integer(val))) => Scalar(Real(val as f64)),
//                 (Type::REAL, Scalar(Bool(val))) => Scalar(Real(val as i64 as f64)),
//                 (Type::BOOL, Scalar(Integer(val))) => Scalar(Bool(val != 0)),
//                 (Type::BOOL, Scalar(Real(val))) => Scalar(Bool(val != 0.0)),
//                 (Type::INT, Scalar(Real(val))) => Scalar(Integer(val.round() as i64)),
//                 (Type::INT, Scalar(Bool(val))) => Scalar(Integer(val as i64)),
//                 (Type::CMPLX, Scalar(Real(val))) => Scalar(Cmplx(Complex64::new(val, 0.0))),
//                 (Type::CMPLX, Scalar(Integer(val))) => {
//                     Scalar(Cmplx(Complex64::new(val as f64, 0.0)))
//                 }

//                 // TODO array casts?
//                 _ => unreachable!("Malformed MIR"),
//             }
//         })
//     }

//     fn fold_use(&mut self, arg: &CallResolverOperand<R>) -> Self::T {
//         self.resolve_operand(arg)
//     }

//     fn fold_select(
//         &mut self,
//         cond: &CallResolverOperand<R>,
//         true_val: &CallResolverOperand<R>,
//         false_val: &CallResolverOperand<R>,
//     ) -> Self::T {
//         let cond = self.resolve_operand(cond);
//         cond.and_then(|cond| match cond {
//             Scalar(Bool(false)) => self.resolve_operand(false_val),
//             Scalar(Bool(true)) => self.resolve_operand(true_val),
//             cond => undefined_operation!("SELECT", cond),
//         })
//     }

//     fn fold_call(
//         &mut self,
//         call: &R::C,
//         args: &IndexSlice<CallArg, [CallResolverOperand<R>]>,
//         _span: Span,
//     ) -> Self::T {
//         let args = args.iter().map(|op| self.resolve_operand(op)).collect_vec();
//         call.const_fold(&args)
//     }

//     fn fold_array(&mut self, args: &[CallResolverOperand<R>], _span: Span, ty: Type) -> Self::T {
//         let mut unknown = false;
//         let mut res = Vec::with_capacity(args.len());
//         for arg in args {
//             match self.resolve_operand(arg) {
//                 FlatSet::Top => return Top,
//                 FlatSet::Bottom => unknown = true,
//                 FlatSet::Elem(_) if unknown => (),
//                 FlatSet::Elem(val) => val.flatten(&mut res),
//             }
//         }

//         if unknown {
//             FlatSet::Bottom
//         } else {
//             FlatSet::Elem(ConstVal::Array(res.into_boxed_slice(), ty))
//         }
//     }
// }
