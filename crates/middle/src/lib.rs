/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use data_structures::index_vec::{define_index_type, IndexVec};

pub use ir::ids::{
    AttributeId, BranchId, DisciplineId, IdRange, ModuleId, NatureId, NetId, ParameterId, PortId,
    StatementId, SyntaxCtx, VariableId,
};
pub use ir::{
    impl_id_type, Attributes, DisciplineAccess, ParameterExcludeConstraint,
    ParameterRangeConstraint, ParameterRangeConstraintBound, Spanned, UnaryOperator, Unknown,
};

use session::sourcemap::{Span, StringLiteral};

use session::symbols::Ident;

use crate::cfg::ControlFlowGraph;
use derive_more::{Display, From};
use std::fmt::{Debug, Display, Formatter};

pub use crate::functions::CfgFunctions;
pub use fold::{fold_rvalue, RValueFold};
pub use osdi_types::{SimpleType, Type, TypeInfo};

use diagnostics::lints::{Lint, LintLevel};

pub mod cfg;
pub mod const_fold;
pub mod derivatives;
pub mod dfa;
mod fold;
pub mod functions;
pub mod inputs;
mod util;

pub type ConstVal = osdi_types::ConstVal<StringLiteral>;
pub type SimpleConstVal = osdi_types::SimpleConstVal<StringLiteral>;

use crate::functions::{DefaultFunctions, ParameterCallType};
pub use crate::inputs::CfgInputs;
use crate::inputs::DefaultInputs;
use data_structures::arrayvec::ArrayVec;
use data_structures::{iter::Itertools, sync::RwLock, HashMap};
use diagnostics::ListFormatter;
use ir::ids::{CallArg, NodeId};
use ir::{Math1, Math2};
pub use osdi_types;
use std::convert::TryInto;
use std::fmt;

#[derive(Debug, Clone)]
pub struct SyntaxContextData {
    pub span: Span,
    pub lint_levels: HashMap<Lint, LintLevel>,
    pub parent: Option<SyntaxCtx>,
}

pub type Statement<C> = (StmntKind<C>, SyntaxCtx);

impl_id_type!(SyntaxCtx in Mir<C> => syntax_ctx as SyntaxContextData where<C: CfgFunctions>);
impl_id_type!(BranchId in Mir<C> => branches as Branch where<C: CfgFunctions>);
impl_id_type!(NodeId in Mir<C> => nodes as Node where<C: CfgFunctions>);
impl_id_type!(PortId in Mir<C> => ports as Port where<C: CfgFunctions>);
impl_id_type!(VariableId in Mir<C> => variables as  Variable where<C: CfgFunctions>);
impl_id_type!(ModuleId in Mir<C> => modules as Module<C> where<C: CfgFunctions>);
impl_id_type!(DisciplineId in Mir<C> => disciplines as Discipline where<C: CfgFunctions>);
impl_id_type!(NatureId in Mir<C> => natures as Nature where<C: CfgFunctions>);
impl_id_type!(ParameterId in Mir<C> => parameters as Parameter where<C: CfgFunctions>);

pub type CallTypeDerivative<C> = Derivative<<C as CfgFunctions>::I>;

#[derive(Clone, Debug, PartialEq)]
pub enum Derivative<I: CfgInputs> {
    One,
    Zero,
    Operand(OperandData<I>),
}

impl<I: CfgInputs> Derivative<I> {
    #[inline]
    pub fn map_input<X: CfgInputs>(self, map: impl FnOnce(I) -> X) -> Derivative<X> {
        match self {
            Derivative::One => Derivative::One,
            Derivative::Zero => Derivative::Zero,
            Derivative::Operand(op) => Derivative::Operand(op.map_input(map)),
        }
    }

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

/// An Expression used for variable default values etc.
/// In MIR Expressions are replaced by RValues which can have at most two operands.
///
/// As such more complex (nested) expressions allowed in parameter/variable default values
/// which can not be constant folded (because they are allowed to depend on parameters)
/// have to be represented as a [`ControlFlowGraph`] that calculates the values.
///
/// Furthermroe the [`Local`] that the resulting value can be read from after executing the cfg is also required

#[derive(Clone, Debug)]
pub struct Expression<C: CfgFunctions = DefaultFunctions>(pub ControlFlowGraph<C>, pub COperand<C>);

impl<C: CfgFunctions> Expression<C> {
    pub fn new_const(val: ConstVal, span: Span) -> Self {
        Self(ControlFlowGraph::empty(), Operand::new(OperandData::Constant(val), span))
    }
}

impl<C: CfgFunctions> Expression<C> {
    pub fn map<X: CfgFunctions>(self, conversion: &mut impl CfgConversion<C, X>) -> Expression<X> {
        Expression(self.0.map(conversion), conversion.map_operand(self.1))
    }
}

#[derive(Debug, Clone, Default)]
pub struct Mir<C: CfgFunctions = DefaultFunctions> {
    /// All branches in this project
    /// Remain unchanged from the HIR
    pub branches: IndexVec<BranchId, Branch>,

    /// All nodes (roughly corresponds to nets) in this project
    /// Remain unchanged from the HIR
    pub nodes: IndexVec<NodeId, Node>,

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Display)]
pub enum BinOp {
    #[display(fmt = "+")]
    Plus,
    #[display(fmt = "-")]
    Minus,
    #[display(fmt = "*")]
    Multiply,
    #[display(fmt = "/")]
    Divide,
    #[display(fmt = "%")]
    Modulus,

    #[display(fmt = "<<")]
    ShiftLeft,
    #[display(fmt = ">>")]
    ShiftRight,

    #[display(fmt = "BitXOR")]
    Xor,
    #[display(fmt = "BitEQ")]
    NXor,
    #[display(fmt = "&")]
    And,
    #[display(fmt = "|")]
    Or,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Display)]
pub enum ComparisonOp {
    #[display(fmt = "<")]
    LessThen,
    #[display(fmt = "<=")]
    LessEqual,
    #[display(fmt = ">")]
    GreaterThen,
    #[display(fmt = ">=")]
    GreaterEqual,
    #[display(fmt = "==")]
    Equal,
    #[display(fmt = "!=")]
    NotEqual,
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

pub type Operand<I = DefaultInputs> = Spanned<OperandData<I>>;

#[derive(Clone, PartialEq, From)]
pub enum OperandData<I: CfgInputs = DefaultInputs> {
    Constant(ConstVal),

    Copy(Local),

    Read(I),
}

impl<I: CfgInputs> OperandData<I> {
    pub fn ty<MC: CfgFunctions, C: CfgFunctions<I = I>>(
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

    #[inline]
    pub fn map_input<X: CfgInputs>(self, map: impl FnOnce(I) -> X) -> OperandData<X> {
        match self {
            Self::Constant(val) => OperandData::Constant(val),
            Self::Copy(local) => OperandData::Copy(local),
            Self::Read(input) => OperandData::Read(map(input)),
        }
    }
}

impl<I: CfgInputs> Display for OperandData<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OperandData::Constant(val) => write!(f, "{:?}", val),
            OperandData::Copy(local) => Display::fmt(local, f),
            OperandData::Read(input) => Display::fmt(input, f),
        }
    }
}

impl<I: CfgInputs> Debug for OperandData<I> {
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

pub type COperand<C> = Operand<<C as CfgFunctions>::I>;
pub type COperandData<C> = OperandData<<C as CfgFunctions>::I>;

#[derive(Clone, Debug, PartialEq)]
pub struct TyRValue<C: CfgFunctions = DefaultFunctions> {
    pub val: RValue<C>,
    pub ty: Type,
}

#[allow(clippy::from_over_into)]
impl<C: CfgFunctions> Into<Type> for TyRValue<C> {
    fn into(self) -> Type {
        self.ty
    }
}

impl<C: CfgFunctions> From<TyRValue<C>> for RValue<C> {
    fn from(typed: TyRValue<C>) -> Self {
        typed.val
    }
}

#[derive(Clone, PartialEq, From)]
pub enum RValue<C: CfgFunctions = DefaultFunctions> {
    UnaryOperation(Spanned<UnaryOperator>, COperand<C>),
    BinaryOperation(Spanned<BinOp>, COperand<C>, COperand<C>),

    Math1(Spanned<Math1>, COperand<C>),
    Math2(Spanned<Math2>, COperand<C>, COperand<C>),
    Comparison(Spanned<ComparisonOp>, COperand<C>, COperand<C>, Type),

    ///
    Select(COperand<C>, COperand<C>, COperand<C>),
    Cast(COperand<C>),
    #[from]
    Use(COperand<C>),
    Call(C, IndexVec<CallArg, COperand<C>>, Span),
    Array(Vec<COperand<C>>, Span),
}

impl<C: CfgFunctions> Display for RValue<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            RValue::UnaryOperation(operator, operand) => {
                write!(f, "{}{}", operator, operand)
            }

            RValue::BinaryOperation(op, lhs, rhs) => {
                write!(f, "{} {} {}", lhs, op, rhs)
            }

            RValue::Math1(fun, op) => {
                write!(f, "{}({})", fun, op)
            }

            RValue::Math2(fun, op1, op2) => {
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
                write!(f, "[{}]", ListFormatter::with_final_seperator(vals.as_slice(), ", "))
            }
        }
    }
}

impl<C: CfgFunctions> Debug for RValue<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl<C: CfgFunctions> RValue<C> {
    pub fn operands(&self) -> impl Iterator<Item = &COperand<C>> {
        let res = match self {
            Self::UnaryOperation(_, op) | Self::Math1(_, op) | Self::Cast(op) | Self::Use(op) => {
                vec![op]
            }

            Self::BinaryOperation(_, op1, op2)
            | Self::Comparison(_, op1, op2, _)
            | Self::Math2(_, op1, op2) => vec![op1, op2],

            Self::Select(op1, op2, op3) => vec![op1, op2, op3],
            Self::Call(_x, args, _) => args.iter().collect_vec(),
            Self::Array(args, _) => args.iter().into_iter().collect_vec(),
        };
        res.into_iter()
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
            Self::UnaryOperation(_, Operand { contents: OperandData::Copy(local), .. })
            | Self::Math1(_, Operand { contents: OperandData::Copy(local), .. })
            | Self::Cast(Operand { contents: OperandData::Copy(local), .. })
            | Self::Use(Operand { contents: OperandData::Copy(local), .. }) => f(*local),

            Self::BinaryOperation(_, op1, op2)
            | Self::Comparison(_, op1, op2, _)
            | Self::Math2(_, op1, op2) => {
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

    pub fn map_operands<X: CfgFunctions>(
        self,
        conversion: &mut impl CfgConversion<C, X>,
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
            RValue::Math1(fun, arg) => RValue::Math1(fun, conversion.map_operand(arg)),
            RValue::Math2(fun, arg1, arg2) => {
                RValue::Math2(fun, conversion.map_operand(arg1), conversion.map_operand(arg2))
            }
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
            RValue::Array(vals, span) => {
                RValue::Array(vals.into_iter().map(|op| conversion.map_operand(op)).collect(), span)
            }
        }
    }

    pub fn operands_mut(&mut self) -> impl Iterator<Item = &mut COperand<C>> {
        match self {
            Self::UnaryOperation(_, op) | Self::Math1(_, op) | Self::Cast(op) | Self::Use(op) => {
                vec![op]
            }

            Self::BinaryOperation(_, op1, op2)
            | Self::Comparison(_, op1, op2, _)
            | Self::Math2(_, op1, op2) => vec![op1, op2],

            Self::Select(op1, op2, op3) => vec![op1, op2, op3],
            Self::Call(_x, args, _) => args.iter_mut().collect_vec(),
            Self::Array(args, _) => args.iter_mut().collect_vec(),
        }
        .into_iter()
    }

    pub fn for_locals_mut(&mut self, mut f: impl FnMut(&mut Local)) {
        match self {
            Self::UnaryOperation(_, Operand { contents: OperandData::Copy(local), .. })
            | Self::Math1(_, Operand { contents: OperandData::Copy(local), .. })
            | Self::Cast(Operand { contents: OperandData::Copy(local), .. })
            | Self::Use(Operand { contents: OperandData::Copy(local), .. }) => f(local),

            Self::BinaryOperation(_, op1, op2)
            | Self::Comparison(_, op1, op2, _)
            | Self::Math2(_, op1, op2) => {
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
            Self::UnaryOperation(_, op) | Self::Math1(_, op) | Self::Cast(op) | Self::Use(op) => {
                op.span
            }

            Self::BinaryOperation(_, op1, op2)
            | Self::Comparison(_, op1, op2, _)
            | Self::Math2(_, op1, op2) => op1.span.extend(op2.span),

            Self::Select(op1, _op2, op3) => op1.span.extend(op3.span),
            Self::Call(_, _, span) => *span,
            Self::Array(_, span) => *span,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum StmntKind<C: CfgFunctions> {
    Assignment(Local, RValue<C>),
    Call(C, IndexVec<CallArg, COperand<C>>, Span),

    /// No Operation (does nothing)
    /// Statements are often overwritten with NoOp instead of being deleted because its cheaper
    NoOp,
}

impl<C: CfgFunctions> Display for StmntKind<C> {
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

impl<C: CfgFunctions> Debug for StmntKind<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl<C: CfgFunctions> StmntKind<C> {
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
pub struct Module<I: CfgFunctions = DefaultFunctions> {
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

pub struct DefaultConversion;

impl<S, D> CfgConversion<S, D> for DefaultConversion
where
    S: CfgFunctions + Into<D>,
    D: CfgFunctions,
    S::I: Into<D::I>,
{
    fn map_input(&mut self, src: S::I) -> COperandData<D> {
        OperandData::Read(src.into())
    }

    fn map_call_val(
        &mut self,
        call: S,
        args: IndexVec<CallArg, COperand<S>>,
        span: Span,
    ) -> RValue<D> {
        RValue::Call(
            call.into(),
            args.into_iter().map(|arg| CfgConversion::<S, D>::map_operand(self, arg)).collect(),
            span,
        )
    }

    fn map_call_stmnt(
        &mut self,
        call: S,
        args: IndexVec<CallArg, COperand<S>>,
        span: Span,
    ) -> StmntKind<D> {
        StmntKind::Call(
            call.into(),
            args.into_iter().map(|arg| CfgConversion::<S, D>::map_operand(self, arg)).collect(),
            span,
        )
    }
}

pub struct TryDefaultConversion;

impl<S, D> CfgConversion<S, D> for TryDefaultConversion
where
    S: CfgFunctions + TryInto<D>,
    D: CfgFunctions,
    S::I: TryInto<D::I>,
{
    fn map_input(&mut self, src: S::I) -> COperandData<D> {
        OperandData::Read(
            src.try_into().unwrap_or_else(|_| unreachable!("Assured CfgInput conversion failed")),
        )
    }

    fn map_call_val(
        &mut self,
        call: S,
        args: IndexVec<CallArg, COperand<S>>,
        span: Span,
    ) -> RValue<D> {
        RValue::Call(
            call.try_into()
                .unwrap_or_else(|_| unreachable!("Assured CfgFunctions conversion failed")),
            args.into_iter().map(|arg| CfgConversion::<S, D>::map_operand(self, arg)).collect(),
            span,
        )
    }

    fn map_call_stmnt(
        &mut self,
        call: S,
        args: IndexVec<CallArg, COperand<S>>,
        span: Span,
    ) -> StmntKind<D> {
        StmntKind::Call(
            call.try_into()
                .unwrap_or_else(|_| unreachable!("Assured CfgFunctions conversion failed")),
            args.into_iter().map(|arg| CfgConversion::<S, D>::map_operand(self, arg)).collect(),
            span,
        )
    }
}

pub trait CfgConversion<S: CfgFunctions, D: CfgFunctions>: Sized {
    fn map_operand(&mut self, op: COperand<S>) -> COperand<D> {
        let contents = match op.contents {
            OperandData::Read(input) => self.map_input(input),
            OperandData::Constant(val) => OperandData::Constant(val),
            OperandData::Copy(loc) => OperandData::Copy(loc),
        };
        Spanned { contents, span: op.span }
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

#[derive(Clone, Copy, Debug)]
pub struct Node {
    pub ident: Ident,
    pub discipline: Option<DisciplineId>,
    pub sctx: SyntaxCtx,
}

#[derive(Clone, Copy, Debug)]
pub struct Port {
    pub input: bool,
    pub output: bool,
    pub node: NodeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Branch {
    pub ident: Ident,
    pub hi: NodeId,
    pub lo: NodeId,
    pub sctx: SyntaxCtx,
    pub generated: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct Discipline {
    pub ident: Ident,
    pub flow_nature: Option<NatureId>,
    pub potential_nature: Option<NatureId>,
    pub continuous: bool,
    pub sctx: SyntaxCtx,
}
