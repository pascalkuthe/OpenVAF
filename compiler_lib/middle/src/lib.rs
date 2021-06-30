/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use openvaf_data_structures::index_vec::{define_index_type, IndexSlice, IndexVec};
pub use openvaf_hir::{Branch, DisciplineAccess, Net};

pub use openvaf_ir::ids::{
    AttributeId, BranchId, DisciplineId, IdRange, IntegerExpressionId, ModuleId, NatureId, NetId,
    ParameterId, PortId, RealExpressionId, StatementId, StringExpressionId, SyntaxCtx, VariableId,
};
pub use openvaf_ir::{
    id_type, impl_id_type, Attributes, DoubleArgMath, NoiseSource, ParameterExcludeConstraint,
    ParameterRangeConstraint, ParameterRangeConstraintBound, Port, PrintOnFinish, SingleArgMath,
    Spanned, StopTaskKind, UnaryOperator, Unknown,
};

use openvaf_session::sourcemap::{Span, StringLiteral};

use openvaf_session::symbols::Ident;

use crate::cfg::ControlFlowGraph;

pub mod cfg;
pub mod const_fold;
pub mod derivatives;
pub mod dfa;
mod fold;
mod util;

pub use fold::{fold_rvalue, RValueFold};
use openvaf_data_structures::HashMap;
pub use osdi_types::{SimpleType, Type, TypeInfo};
use std::fmt::{Debug, Display, Formatter};
use std::iter::FromIterator;

use openvaf_diagnostics::lints::{Lint, LintLevel};
use openvaf_hir::Discipline;

pub type ConstVal = osdi_types::ConstVal<StringLiteral>;
pub type SimpleConstVal = osdi_types::SimpleConstVal<StringLiteral>;

use crate::derivatives::RValueAutoDiff;
use crate::dfa::lattice::FlatSet;
use openvaf_data_structures::arrayvec::ArrayVec;
use openvaf_data_structures::sync::RwLock;
use openvaf_diagnostics::ListFormatter;
use openvaf_ir::convert::Convert;
pub use osdi_types;
use std::fmt;

#[derive(Debug, Clone)]
pub struct SyntaxContextData {
    pub span: Span,
    pub lint_levels: HashMap<Lint, LintLevel>,
    pub parent: Option<SyntaxCtx>,
}

pub type Statement<C> = (StmntKind<C>, SyntaxCtx);

impl_id_type!(SyntaxCtx in Mir<C> => syntax_ctx as SyntaxContextData where<C: CallType>);

impl_id_type!(BranchId in Mir<C> => branches as Branch where<C: CallType>);

impl_id_type!(NetId in Mir<C> => nets as Net where<C: CallType>);

impl_id_type!(PortId in Mir<C> => ports as Port where<C: CallType>);

impl_id_type!(VariableId in Mir<C> => variables as  Variable where<C: CallType>);

impl_id_type!(ModuleId in Mir<C> => modules as Module<C> where<C: CallType>);

impl_id_type!(DisciplineId in Mir<C> => disciplines as Discipline where<C: CallType>);

impl_id_type!(NatureId in Mir<C> => natures as Nature where<C: CallType>);

impl_id_type!(ParameterId in Mir<C> => parameters as Parameter where<C: CallType>);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct NoInput;

pub type CallTypeDerivative<C> = Derivative<<C as CallType>::I>;

#[derive(Clone, Debug, PartialEq)]
pub enum Derivative<I: InputKind> {
    One,
    Zero,
    Operand(OperandData<I>),
}

impl<I: InputKind> Derivative<I> {
    pub fn into_operand(self) -> OperandData<I> {
        match self {
            Self::One => OperandData::Constant(1.0.into()),
            Self::Zero => OperandData::Constant(0.0.into()),
            Self::Operand(operand) => operand,
        }
    }

    pub fn into_option(self) -> Option<OperandData<I>> {
        match self {
            Self::One => Some(OperandData::Constant(1.0.into())),
            Self::Zero => None,
            Self::Operand(operand) => Some(operand),
        }
    }
}

impl InputKind for NoInput {
    fn derivative<C: CallType>(&self, _unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        unreachable!("This cfg has no input")
    }

    fn ty<C: CallType>(&self, _mir: &Mir<C>) -> Type {
        unreachable!("This cfg has no input")
    }
}

impl Display for NoInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("ILLEGAL")
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ParameterInput {
    Value(ParameterId),
    Given(ParameterId),
}

impl InputKind for ParameterInput {
    fn derivative<C: CallType>(&self, unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        if matches!((unknown, self), (Unknown::Parameter(x), Self::Value(y)) if &x == y) {
            Derivative::One
        } else {
            Derivative::Zero
        }
    }

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type {
        match self {
            Self::Value(param) => mir[*param].ty,
            Self::Given(_) => Type::BOOL,
        }
    }
}

impl Display for ParameterInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(param) => Debug::fmt(param, f),
            Self::Given(param) => write!(f, "$param_given({:?})", param),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ParameterCallType {}

impl CallType for ParameterCallType {
    type I = ParameterInput;

    fn const_fold(&self, _args: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        match *self {}
    }

    fn derivative<C: CallType>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<Self>]>,
        _ad: &mut RValueAutoDiff<Self, C>,
        _span: Span,
    ) -> Option<RValue<Self>> {
        match *self {}
    }
}

impl Display for ParameterCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("ILLEGAL")
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum RealConstCallType {}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum RealConstInputType {}

impl InputKind for RealConstInputType {
    fn derivative<C: CallType>(&self, _: Unknown, _: &Mir<C>) -> Derivative<Self> {
        match *self {}
    }

    fn ty<C: CallType>(&self, _: &Mir<C>) -> Type {
        match *self {}
    }
}

impl Display for RealConstCallType {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl Display for RealConstInputType {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl CallType for RealConstCallType {
    type I = RealConstInputType;

    fn const_fold(&self, _: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        match *self {}
    }

    fn derivative<C: CallType>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<Self>]>,
        _ad: &mut RValueAutoDiff<Self, C>,
        _span: Span,
    ) -> Option<RValue<Self>> {
        match *self {}
    }
}

/// An Expression used for variable default values etc.
/// In MIR Expressions are replaced by RValues which can have at most two operands.
///
/// As such more complex (nested) expressions allowed in parameter/variable default values
/// which can not be constant folded (because they are allowed to depend on parameters)
/// have to be represented as a [`ControlFlowGraph`] that calculates the values.
///
/// Furthermroe the [`Local`] that the resulting value can be read from after executing the cfg is also required

#[derive(Clone, Debug)]
pub struct Expression<C: CallType>(pub ControlFlowGraph<C>, pub COperand<C>);

impl<C: CallType> Expression<C> {
    pub fn new_const(val: ConstVal, span: Span) -> Self {
        Self(
            ControlFlowGraph::empty(),
            Operand::new(OperandData::Constant(val), span),
        )
    }
}

impl<C: CallType> Expression<C> {
    pub fn map<X: CallType>(self, conversion: &mut impl CallTypeConversion<C, X>) -> Expression<X> {
        Expression(self.0.map(conversion), conversion.map_operand(self.1))
    }
}

impl<A, B> Convert<Expression<B>> for Expression<A>
where
    B: CallType,
    A: Into<B> + CallType,
    A::I: Into<B::I>,
{
    fn convert(self) -> Expression<B> {
        Expression(self.0.convert(), self.1.convert())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Mir<C: CallType> {
    /// All branches in this project
    /// Remain unchanged from the HIR
    pub branches: IndexVec<BranchId, Branch>,

    /// All nets in this project
    /// Remain unchanged from the HIR
    pub nets: IndexVec<NetId, Net>,

    /// All ports in this project
    /// Remain unchanged from the HIR
    pub ports: IndexVec<PortId, Port>,

    /// All disciplines in this project
    /// Remain unchanged from the HIR
    pub disciplines: IndexVec<DisciplineId, Discipline>,

    pub modules: IndexVec<ModuleId, Module<C>>,
    pub parameters: IndexVec<ParameterId, Parameter>,
    pub variables: IndexVec<VariableId, Variable>,
    pub natures: IndexVec<NatureId, Nature>,

    pub syntax_ctx: IndexVec<SyntaxCtx, SyntaxContextData>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,

    ShiftLeft,
    ShiftRight,

    Xor,
    NXor,
    And,
    Or,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Plus => f.write_str("+"),
            BinOp::Minus => f.write_str("-"),
            BinOp::Multiply => f.write_str("*"),
            BinOp::Divide => f.write_str("/"),
            BinOp::Modulus => f.write_str("%"),
            BinOp::ShiftLeft => f.write_str("<<"),
            BinOp::ShiftRight => f.write_str(">>"),
            BinOp::Xor => f.write_str("XOR"),
            BinOp::NXor => f.write_str("EQ"),
            BinOp::And => f.write_str("&"),
            BinOp::Or => f.write_str("|"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ComparisonOp {
    LessThen,
    LessEqual,
    GreaterThen,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Display for ComparisonOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let raw = match self {
            ComparisonOp::LessThen => "<",
            ComparisonOp::LessEqual => "<=",
            ComparisonOp::GreaterThen => ">",
            ComparisonOp::GreaterEqual => ">=",
            ComparisonOp::Equal => "==",
            ComparisonOp::NotEqual => "!=",
        };
        f.write_str(raw)
    }
}

pub trait InputKind: Clone + Sized + Debug + PartialEq + Display {
    fn derivative<C: CallType>(&self, unknown: Unknown, mir: &Mir<C>) -> Derivative<Self>;

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type;
}

define_index_type! {
    pub struct CallArg = u8;

    DISPLAY_FORMAT = "{}";

    DEBUG_FORMAT = stringify!(<CallArg {}>);

    IMPL_RAW_CONVERSIONS = true;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalDeclaration {
    pub kind: LocalKind,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VariableLocalKind {
    User,
    #[cfg(not(feature = "arbitrary_order_derivative"))]
    Derivative(ArrayVec<Unknown, 7>), // Aligment
    #[cfg(feature = "arbitrary_order_derivative")]
    Derivative,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LocalKind {
    /// A local correspond to a variable
    /// These locals are not ssa and as such are mapped to alloca/pointers
    /// Note that multiple locals may exist for the same variable (eg  derivatives)
    /// The Local is always what uniquely identifies the memory location
    Variable(VariableId, VariableLocalKind),

    /// There shall only be one local for every combination of DisciplineAccess
    /// and BranchId. This is automatically enforced during HIR lowering.
    /// Adding a second local for the same branch/discipline combination will result in UB (in the output binary not the compiler itself
    Branch(DisciplineAccess, BranchId, VariableLocalKind),

    /// Temporary values introduced by OpenVAF
    /// These act like SSA as such it is UB (in the output binary not the compiler itself) to write to the same local twice
    /// (will probably cause a panic a some stage or a sigfault during codegen)
    Temporary,
}

pub type Operand<I> = Spanned<OperandData<I>>;

#[derive(Clone, PartialEq)]
pub enum OperandData<I: InputKind> {
    Constant(ConstVal),

    Copy(Local),

    Read(I),
}

impl<A, B> Convert<OperandData<B>> for OperandData<A>
where
    A: InputKind + Into<B>,
    B: InputKind,
{
    fn convert(self) -> OperandData<B> {
        match self {
            Self::Constant(val) => OperandData::Constant(val),
            Self::Copy(local) => OperandData::Copy(local),
            Self::Read(input) => OperandData::Read(input.into()),
        }
    }
}

impl<I: InputKind> OperandData<I> {
    pub fn ty<MC: CallType, C: CallType<I = I>>(
        &self,
        mir: &Mir<MC>,
        cfg: &ControlFlowGraph<C>,
    ) -> Type {
        match self {
            Self::Constant(val) => val.ty(),
            Self::Copy(local) => cfg.locals[*local].ty,
            Self::Read(input) => input.ty(mir),
        }
    }
}

impl<I: InputKind> Display for OperandData<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OperandData::Constant(val) => write!(f, "{:?}", val),
            OperandData::Copy(local) => Display::fmt(local, f),
            OperandData::Read(input) => Display::fmt(input, f),
        }
    }
}

impl<I: InputKind> Debug for OperandData<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

define_index_type! {

    pub struct Local = u32;

    DISPLAY_FORMAT = "_{}";
    DEBUG_FORMAT = "_{}";

    IMPL_RAW_CONVERSIONS = true;
}

pub trait CallType: Debug + Clone + PartialEq + Display {
    type I: InputKind;

    fn const_fold(&self, call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal>;
    fn derivative<C: CallType>(
        &self,
        args: &IndexSlice<CallArg, [COperand<Self>]>,
        ad: &mut RValueAutoDiff<Self, C>,
        span: Span,
    ) -> Option<RValue<Self>>;
}

pub type COperand<C> = Operand<<C as CallType>::I>;
pub type COperandData<C> = OperandData<<C as CallType>::I>;

#[derive(Clone, Debug, PartialEq)]
pub struct TyRValue<C: CallType> {
    pub val: RValue<C>,
    pub ty: Type,
}

impl<C: CallType> Into<Type> for TyRValue<C> {
    fn into(self) -> Type {
        self.ty
    }
}

impl<C: CallType> From<TyRValue<C>> for RValue<C> {
    fn from(typed: TyRValue<C>) -> Self {
        typed.val
    }
}

#[derive(Clone, PartialEq)]
pub enum RValue<C: CallType> {
    UnaryOperation(Spanned<UnaryOperator>, COperand<C>),
    BinaryOperation(Spanned<BinOp>, COperand<C>, COperand<C>),

    SingleArgMath(Spanned<SingleArgMath>, COperand<C>),
    DoubleArgMath(Spanned<DoubleArgMath>, COperand<C>, COperand<C>),
    Comparison(Spanned<ComparisonOp>, COperand<C>, COperand<C>, Type),

    ///
    Select(COperand<C>, COperand<C>, COperand<C>),
    Cast(COperand<C>),
    Use(COperand<C>),
    Call(C, IndexVec<CallArg, COperand<C>>, Span),
    Array(Vec<COperand<C>>, Span),
}

impl<C: CallType> Display for RValue<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            RValue::UnaryOperation(operator, operand) => {
                write!(f, "{}{}", operator, operand)
            }

            RValue::BinaryOperation(op, lhs, rhs) => {
                write!(f, "{} {} {}", lhs, op, rhs)
            }

            RValue::SingleArgMath(fun, op) => {
                write!(f, "{}({})", fun, op)
            }

            RValue::DoubleArgMath(fun, op1, op2) => {
                write!(f, "{}({}, {})", fun, op1, op2)
            }

            RValue::Comparison(op, lhs, rhs, _) => {
                write!(f, "{} {} {}", lhs, op, rhs)
            }

            RValue::Select(cond, true_val, false_val) => {
                write!(f, "if {} {{ {} }} else {{ {} }}", cond, true_val, false_val)
            }
            RValue::Cast(op) => {
                write!(f, "{} as _", op)
            }
            RValue::Use(op) => Display::fmt(op, f),
            RValue::Call(call, operands, _) => {
                write!(
                    f,
                    "{}({})",
                    call,
                    ListFormatter::with_final_seperator(operands.raw.as_slice(), ", ")
                )
            }
            RValue::Array(vals, _) => {
                write!(
                    f,
                    "[{}]",
                    ListFormatter::with_final_seperator(vals.as_slice(), ", ")
                )
            }
        }
    }
}

impl<C: CallType> Debug for RValue<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl<A, B> Convert<RValue<B>> for RValue<A>
where
    B: CallType,
    A: Into<B> + CallType,
    A::I: Into<B::I>,
{
    fn convert(self) -> RValue<B> {
        match self {
            Self::UnaryOperation(op, arg) => RValue::UnaryOperation(op, arg.convert()),
            Self::BinaryOperation(op, lhs, rhs) => {
                RValue::BinaryOperation(op, lhs.convert(), rhs.convert())
            }
            Self::SingleArgMath(op, arg) => RValue::SingleArgMath(op, arg.convert()),
            Self::DoubleArgMath(op, lhs, rhs) => {
                RValue::DoubleArgMath(op, lhs.convert(), rhs.convert())
            }
            Self::Comparison(op, lhs, rhs, ty) => {
                RValue::Comparison(op, lhs.convert(), rhs.convert(), ty)
            }
            Self::Select(cond, lhs, rhs) => {
                RValue::Select(cond.convert(), lhs.convert(), rhs.convert())
            }
            Self::Cast(arg) => RValue::Cast(arg.convert()),
            Self::Use(arg) => RValue::Use(arg.convert()),
            Self::Call(call, args, span) => RValue::Call(
                call.into(),
                args.into_iter().map(Operand::convert).collect(),
                span,
            ),
            Self::Array(args, span) => {
                RValue::Array(args.into_iter().map(Operand::convert).collect(), span)
            }
        }
    }
}

impl<C: CallType> RValue<C> {
    pub fn operands(&self) -> impl Iterator<Item = &COperand<C>> {
        match self {
            Self::UnaryOperation(_, op)
            | Self::SingleArgMath(_, op)
            | Self::Cast(op)
            | Self::Use(op) => vec![op].into_iter(),

            Self::BinaryOperation(_, op1, op2)
            | Self::Comparison(_, op1, op2, _)
            | Self::DoubleArgMath(_, op1, op2) => vec![op1, op2].into_iter(),

            Self::Select(op1, op2, op3) => vec![op1, op2, op3].into_iter(),
            Self::Call(_x, args, _) => Vec::from_iter(args.iter()).into_iter(),
            Self::Array(args, _) => Vec::from_iter(args.iter()).into_iter(),
        }
    }

    pub fn locals(&self) -> impl Iterator<Item = Local> + '_ {
        self.operands().filter_map(|operand| {
            if let OperandData::Copy(local) = operand.contents {
                Some(local)
            } else {
                None
            }
        })
    }

    pub fn for_locals(&self, mut f: impl FnMut(Local)) {
        match self {
            Self::UnaryOperation(
                _,
                Operand {
                    contents: OperandData::Copy(local),
                    ..
                },
            )
            | Self::SingleArgMath(
                _,
                Operand {
                    contents: OperandData::Copy(local),
                    ..
                },
            )
            | Self::Cast(Operand {
                contents: OperandData::Copy(local),
                ..
            })
            | Self::Use(Operand {
                contents: OperandData::Copy(local),
                ..
            }) => f(*local),

            Self::BinaryOperation(_, op1, op2)
            | Self::Comparison(_, op1, op2, _)
            | Self::DoubleArgMath(_, op1, op2) => {
                if let OperandData::Copy(local) = op1.contents {
                    f(local);
                }
                if let OperandData::Copy(local) = op2.contents {
                    f(local);
                }
            }

            Self::Select(op1, op2, op3) => {
                if let OperandData::Copy(local) = op1.contents {
                    f(local);
                }
                if let OperandData::Copy(local) = op2.contents {
                    f(local);
                }
                if let OperandData::Copy(local) = op3.contents {
                    f(local);
                }
            }
            Self::Call(_, args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = arg.contents {
                        f(local)
                    }
                }
            }
            Self::Array(args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = arg.contents {
                        f(local)
                    }
                }
            }
            _ => (),
        }
    }

    pub fn map_operands<X: CallType>(
        self,
        conversion: &mut impl CallTypeConversion<C, X>,
    ) -> RValue<X> {
        match self {
            RValue::UnaryOperation(op, arg) => {
                RValue::UnaryOperation(op, conversion.map_operand(arg))
            }
            RValue::BinaryOperation(op, arg1, arg2) => RValue::BinaryOperation(
                op,
                conversion.map_operand(arg1),
                conversion.map_operand(arg2),
            ),
            RValue::SingleArgMath(fun, arg) => {
                RValue::SingleArgMath(fun, conversion.map_operand(arg))
            }
            RValue::DoubleArgMath(fun, arg1, arg2) => RValue::DoubleArgMath(
                fun,
                conversion.map_operand(arg1),
                conversion.map_operand(arg2),
            ),
            RValue::Comparison(op, arg1, arg2, ty) => RValue::Comparison(
                op,
                conversion.map_operand(arg1),
                conversion.map_operand(arg2),
                ty,
            ),
            RValue::Select(cond, true_val, false_val) => RValue::Select(
                conversion.map_operand(cond),
                conversion.map_operand(true_val),
                conversion.map_operand(false_val),
            ),
            RValue::Cast(op) => RValue::Cast(conversion.map_operand(op)),
            RValue::Use(op) => RValue::Use(conversion.map_operand(op)),
            RValue::Call(call, args, span) => conversion.map_call_val(call, args, span),
            RValue::Array(vals, span) => RValue::Array(
                vals.into_iter()
                    .map(|op| conversion.map_operand(op))
                    .collect(),
                span,
            ),
        }
    }

    pub fn operands_mut(&mut self) -> impl Iterator<Item = &mut COperand<C>> {
        match self {
            Self::UnaryOperation(_, op)
            | Self::SingleArgMath(_, op)
            | Self::Cast(op)
            | Self::Use(op) => vec![op].into_iter(),

            Self::BinaryOperation(_, op1, op2)
            | Self::Comparison(_, op1, op2, _)
            | Self::DoubleArgMath(_, op1, op2) => vec![op1, op2].into_iter(),

            Self::Select(op1, op2, op3) => vec![op1, op2, op3].into_iter(),
            Self::Call(_x, args, _) => Vec::from_iter(args.iter_mut()).into_iter(),
            Self::Array(args, _) => Vec::from_iter(args.iter_mut()).into_iter(),
        }
    }

    pub fn for_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        match self {
            Self::UnaryOperation(
                _,
                Operand {
                    contents: OperandData::Copy(local),
                    ..
                },
            )
            | Self::SingleArgMath(
                _,
                Operand {
                    contents: OperandData::Copy(local),
                    ..
                },
            )
            | Self::Cast(Operand {
                contents: OperandData::Copy(local),
                ..
            })
            | Self::Use(Operand {
                contents: OperandData::Copy(local),
                ..
            }) => f(local),

            Self::BinaryOperation(_, op1, op2)
            | Self::Comparison(_, op1, op2, _)
            | Self::DoubleArgMath(_, op1, op2) => {
                if let OperandData::Copy(local) = &mut op1.contents {
                    f(local);
                }
                if let OperandData::Copy(local) = &mut op2.contents {
                    f(local);
                }
            }

            Self::Select(op1, op2, op3) => {
                if let OperandData::Copy(local) = &mut op1.contents {
                    f(local);
                }
                if let OperandData::Copy(local) = &mut op2.contents {
                    f(local);
                }
                if let OperandData::Copy(local) = &mut op3.contents {
                    f(local);
                }
            }

            Self::Call(_, args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = &mut arg.contents {
                        f(local)
                    }
                }
            }
            Self::Array(args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = &mut arg.contents {
                        f(local)
                    }
                }
            }
            _ => (),
        }
    }

    pub fn locals_mut(&mut self) -> impl Iterator<Item = &mut Local> {
        self.operands_mut().filter_map(|operand| {
            if let OperandData::Copy(local) = &mut operand.contents {
                Some(local)
            } else {
                None
            }
        })
    }

    pub fn span(&self) -> Span {
        match self {
            Self::UnaryOperation(_, op)
            | Self::SingleArgMath(_, op)
            | Self::Cast(op)
            | Self::Use(op) => op.span,

            Self::BinaryOperation(_, op1, op2)
            | Self::Comparison(_, op1, op2, _)
            | Self::DoubleArgMath(_, op1, op2) => op1.span.extend(op2.span),

            Self::Select(op1, _op2, op3) => op1.span.extend(op3.span),
            Self::Call(_, _, span) => *span,
            Self::Array(_, span) => *span,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum StmntKind<C: CallType> {
    Assignment(Local, RValue<C>),
    Call(C, IndexVec<CallArg, COperand<C>>, Span),

    /// No Operation (does nothing)
    /// Statements are often overwritten with NoOp instead of being deleted because its cheaper
    NoOp,
}

impl<C: CallType> Display for StmntKind<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StmntKind::Assignment(dst, val) => write!(f, "{} = {}", dst, val),
            StmntKind::Call(call, args, _) => write!(
                f,
                "{}({})",
                call,
                ListFormatter::with_final_seperator(&args.as_slice().raw, ", ")
            ),
            StmntKind::NoOp => Ok(()),
        }
    }
}

impl<C: CallType> Debug for StmntKind<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl<A, B> Convert<StmntKind<B>> for StmntKind<A>
where
    B: CallType,
    A: Into<B> + CallType,
    A::I: Into<B::I>,
{
    fn convert(self) -> StmntKind<B> {
        match self {
            Self::Assignment(dst, rval) => StmntKind::Assignment(dst, rval.convert()),
            Self::Call(call, args, span) => StmntKind::Call(
                call.into(),
                args.into_iter().map(Operand::convert).collect(),
                span,
            ),
            Self::NoOp => StmntKind::NoOp,
        }
    }
}

impl<C: CallType> StmntKind<C> {
    pub fn read_locals(&self) -> impl Iterator<Item = Local> {
        // All stmnts excepot array construction and calls have 3 or less operands
        // Even calls have 3 or less arguments in practice in 90% of cases
        let mut buff = Vec::with_capacity(3);
        self.for_read_locals(|local| buff.push(local));
        buff.into_iter()
    }

    pub fn read_locals_mut(&mut self) -> impl Iterator<Item = &mut Local> {
        // This is mostly three adress code
        let mut buff = Vec::with_capacity(3);
        // This is save since each local is an individual location in memory the interface of the for_locals funciton just doesn't capture that
        self.for_read_locals_mut(|local| buff.push(local as *mut Local));
        buff.into_iter().map(|local_ptr| unsafe { &mut *local_ptr })
    }

    pub fn for_read_locals(&self, mut f: impl FnMut(Local)) {
        match *self {
            Self::Assignment(_, ref val) => val.for_locals(f),
            Self::Call(_, ref args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = arg.contents {
                        f(local)
                    }
                }
            }
            Self::NoOp => {}
        }
    }

    pub fn for_read_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        match self {
            Self::Assignment(_, val) => val.for_locals_mut(f),
            Self::Call(_, args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = &mut arg.contents {
                        f(local)
                    }
                }
            }
            Self::NoOp => {}
        }
    }

    pub fn locals(&self) -> impl Iterator<Item = Local> {
        // All stmnts excepot array construction and calls have 3 or less operands
        // Even calls have 3 or less arguments in practice in 90% of cases
        let mut buff = Vec::with_capacity(3);
        self.for_locals(|local| buff.push(local));
        buff.into_iter()
    }

    pub fn locals_mut(&mut self) -> impl Iterator<Item = &mut Local> {
        // This is mostly three adress code
        let mut buff = Vec::with_capacity(3);
        // This is save since each local is an individual location in memory the interface of the for_locals funciton just doesn't capture that
        self.for_locals_mut(|local| buff.push(local as *mut Local));
        buff.into_iter().map(|local_ptr| unsafe { &mut *local_ptr })
    }

    pub fn for_locals(&self, mut f: impl FnMut(Local)) {
        match *self {
            Self::Assignment(dst, ref val) => {
                f(dst);
                val.for_locals(f)
            }
            Self::Call(_, ref args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = arg.contents {
                        f(local)
                    }
                }
            }
            Self::NoOp => {}
        }
    }

    pub fn for_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        match self {
            Self::Assignment(dst, val) => {
                f(dst);
                val.for_locals_mut(f)
            }
            Self::Call(_, args, _) => {
                for arg in args {
                    if let OperandData::Copy(local) = &mut arg.contents {
                        f(local)
                    }
                }
            }
            Self::NoOp => {}
        }
    }
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub ident: Ident,
    pub variable_type: Type,
    pub default: RwLock<Expression<ParameterCallType>>,
    pub unit: Option<StringLiteral>,
    pub desc: Option<StringLiteral>,
    pub sctx: SyntaxCtx,
    pub ty: Type,
}

#[derive(Copy, Clone, Debug)]
pub struct Nature {
    pub ident: Ident,
    pub abstol: f64,
    pub units: StringLiteral,
    pub access: Ident,
    pub idt_nature: NatureId,
    pub ddt_nature: NatureId,
    pub sctx: SyntaxCtx,
}

#[derive(Debug, Clone)]
pub struct Module<I: CallType> {
    pub ident: Ident,
    pub ports: IdRange<PortId>,
    pub parameters: IdRange<ParameterId>,
    pub analog_cfg: RwLock<ControlFlowGraph<I>>,
    pub sctx: SyntaxCtx,
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub ident: Ident,
    pub ty: Type,
    pub default: RwLock<Expression<ParameterCallType>>,
    pub kind: RwLock<ParameterConstraint>,
    pub unit: Option<StringLiteral>,
    pub desc: Option<StringLiteral>,
    pub sctx: SyntaxCtx,
}

/// A Parameter kind indicates what kind of constraints are placed on a parmeter
///
///
/// These differ between Ordered (float, integer) and unordered (string) values
/// Unordered
///
/// # Note
/// Don't confuse parameter kind with parameter type.
/// Each parameter kind actually correspond to multiple types

#[derive(Debug, Clone)]
pub enum ParameterConstraint {
    Ordered {
        included: Vec<ParameterRangeConstraint<Expression<ParameterCallType>>>,
        excluded: Vec<ParameterExcludeConstraint<Expression<ParameterCallType>>>,
    },

    UnOrdered {
        included: Vec<Expression<ParameterCallType>>,
        excluded: Vec<Expression<ParameterCallType>>,
    },
}

pub trait CallTypeConversion<S: CallType, D: CallType>: Sized {
    fn map_operand(&mut self, op: COperand<S>) -> COperand<D> {
        let contents = match op.contents {
            OperandData::Read(input) => self.map_input(input),
            OperandData::Constant(val) => OperandData::Constant(val),
            OperandData::Copy(loc) => OperandData::Copy(loc),
        };
        Spanned {
            contents,
            span: op.span,
        }
    }

    fn map_input(&mut self, _src: S::I) -> COperandData<D>;

    fn map_call_val(
        &mut self,
        call: S,
        args: IndexVec<CallArg, COperand<S>>,
        span: Span,
    ) -> RValue<D>;

    fn map_call_stmnt(
        &mut self,
        call: S,
        args: IndexVec<CallArg, COperand<S>>,
        span: Span,
    ) -> StmntKind<D>;

    fn map_stmnt(&mut self, kind: StmntKind<S>) -> StmntKind<D> {
        match kind {
            StmntKind::Assignment(dst, val) => StmntKind::Assignment(dst, val.map_operands(self)),
            StmntKind::Call(call, args, span) => self.map_call_stmnt(call, args, span),
            StmntKind::NoOp => StmntKind::NoOp,
        }
    }
}
