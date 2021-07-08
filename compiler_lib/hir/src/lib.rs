/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

pub mod functions;

use openvaf_data_structures::index_vec::IndexVec;

pub use openvaf_ast::{BinaryOperator, Type};

pub use openvaf_ir::ids::{
    AttributeId, BranchId, DisciplineId, ExpressionId, FunctionId, IdRange, ModuleId, NatureId,
    NetId, ParameterId, PortId, StatementId, SyntaxCtx, VariableId,
};

pub use openvaf_ir::{
    impl_id_type, AttrSpanned, Attribute, Attributes, Math1, Math2, ParameterExcludeConstraint,
    ParameterRangeConstraint, Spanned, UnaryOperator, Unknown,
};

use openvaf_session::symbols::Ident;

use crate::functions::Function;
use derive_more::Display;
use enumset::{enum_set, EnumSet, EnumSetType};
pub use openvaf_ast::ConstVal;
use openvaf_ir::ids::{CallArg, NodeId};
use openvaf_ir::DisciplineAccess;
pub use openvaf_middle::{Discipline, Node, Port};
use openvaf_session::sourcemap::Span;

pub mod lowering;

pub type AllowedOperations = EnumSet<AllowedOperation>;

pub const ALLOWED_OPS_CONST_EXPRESSION: EnumSet<AllowedOperation> =
    enum_set!(AllowedOperation::ParameterReferences);
pub const ALLOWED_OPS_ANALOG_FUNCTION_BEHAVIOUR: EnumSet<AllowedOperation> = enum_set!(
    AllowedOperation::ParameterReferences
        | AllowedOperation::VariableReferences
        | AllowedOperation::SystemFunctionCalls
        | AllowedOperation::UserFunctionCalls
);

pub const ALLOWED_OPS_CONDITIONAL_ANALOG_BEHAVIOUR: EnumSet<AllowedOperation> = enum_set!(
    AllowedOperation::ParameterReferences
        | AllowedOperation::VariableReferences
        | AllowedOperation::SystemFunctionCalls
        | AllowedOperation::BranchAccess
        | AllowedOperation::UserFunctionCalls
        | AllowedOperation::Contribute
        | AllowedOperation::NamedBlocks
);

pub const ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR: EnumSet<AllowedOperation> = enum_set!(
    AllowedOperation::ParameterReferences
        | AllowedOperation::VariableReferences
        | AllowedOperation::SystemFunctionCalls
        | AllowedOperation::BranchAccess
        | AllowedOperation::UserFunctionCalls
        | AllowedOperation::Contribute
        | AllowedOperation::AnalogFilters
        | AllowedOperation::NamedBlocks
);

/// Some expressions are only allowed in certain a certain context.
/// OpenVAF use bitflags to keep track of what is currently allowed during ast lowering
#[derive(Display, Debug, EnumSetType)]
pub enum AllowedOperation {
    #[display(fmt = "referencing nets")]
    NetReferences,
    #[display(fmt = "referencing ports")]
    PortReferences,
    #[display(fmt = "referencing parameter")]
    ParameterReferences,
    #[display(fmt = "referencing variables")]
    VariableReferences,
    #[display(fmt = "calling analog filter functions")]
    AnalogFilters,
    #[display(fmt = "accessing branches")]
    BranchAccess,
    #[display(fmt = "calling system functions")]
    SystemFunctionCalls,
    #[display(fmt = "calling the $temperature system function")]
    Temperature,
    #[display(fmt = "calling VerilogA functions")]
    UserFunctionCalls,
    #[display(fmt = "contributing to branches")]
    UserFunctionReference,
    #[display(fmt = "a user defined function")]
    Contribute,
    #[display(fmt = "declaring named blocks")]
    NamedBlocks,
}

#[derive(Default, Debug, Clone)]
pub struct Hir {
    pub parameters: IndexVec<ParameterId, Parameter>,
    pub branches: IndexVec<BranchId, Branch>,
    pub nodes: IndexVec<NodeId, Node>,
    pub ports: IndexVec<PortId, Port>,
    pub variables: IndexVec<VariableId, Variable>,
    pub modules: IndexVec<ModuleId, Module>,
    pub functions: IndexVec<FunctionId, UserFunction>,
    pub disciplines: IndexVec<DisciplineId, Discipline>,
    pub natures: IndexVec<NatureId, Nature>,
    pub expressions: IndexVec<ExpressionId, Spanned<Expression>>,
    pub attributes: IndexVec<AttributeId, Attribute>,
    pub statements: IndexVec<StatementId, (Statement, SyntaxCtx)>,
    pub syntax_ctx: IndexVec<SyntaxCtx, SyntaxContextData>,
}

impl_id_type!(BranchId in Hir => branches as Branch);
impl_id_type!(NodeId in Hir => nodes as Node);
impl_id_type!(PortId in Hir => ports as Port);
impl_id_type!(VariableId in Hir => variables as  Variable);
impl_id_type!(ModuleId in Hir => modules as Module);
impl_id_type!(FunctionId in Hir => functions as UserFunction);
impl_id_type!(DisciplineId in Hir => disciplines as Discipline);
impl_id_type!(ExpressionId in Hir => expressions as Spanned<Expression>);
impl_id_type!(AttributeId in Hir => attributes as Attribute);
impl_id_type!(StatementId in Hir => statements as (Statement, SyntaxCtx));
impl_id_type!(NatureId in Hir => natures as Nature);
impl_id_type!(ParameterId in Hir => parameters as Parameter);
impl_id_type!(SyntaxCtx in Hir => syntax_ctx as SyntaxContextData);

#[derive(Clone, Copy, Debug)]
pub struct Variable {
    pub ident: Ident,
    pub ty: Type,
    pub default: Option<ExpressionId>,
    pub sctx: SyntaxCtx,
}

impl Variable {
    pub const PLACEHOLDER: Self = Self {
        ident: Ident::DUMMY,
        ty: Type::INT,
        default: None,
        sctx: SyntaxCtx::ROOT,
    };
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub ident: Ident,
    pub constraints: ParameterConstraint,
    pub default: ExpressionId,
    pub ty: Type,
    pub sctx: SyntaxCtx,
}
impl Parameter {
    pub const PLACEHOLDER: Self = Self {
        ident: Ident::DUMMY,
        constraints: ParameterConstraint::Ordered(Vec::new(), Vec::new()),
        default: ExpressionId::from_raw_unchecked(u32::MAX),
        ty: Type::INT,
        sctx: SyntaxCtx::ROOT,
    };
}

#[derive(Clone, Debug)]
pub enum ParameterConstraint {
    Ordered(
        Vec<ParameterRangeConstraint<ExpressionId>>,
        Vec<ParameterExcludeConstraint<ExpressionId>>,
    ),
    Unordered(Vec<ExpressionId>, Vec<ExpressionId>),
}

#[derive(Clone, Debug)]
pub struct UserFunction {
    pub ident: Ident,
    pub args: Vec<FunctionArg>,
    pub return_variable: VariableId,
    pub body: Block,
    pub sctx: SyntaxCtx,
}

impl UserFunction {
    pub const PLACEHOLDER: Self = Self {
        ident: Ident::DUMMY,
        args: Vec::new(),
        return_variable: VariableId::from_raw_unchecked(u32::MAX),
        body: Vec::new(),
        sctx: SyntaxCtx::ROOT,
    };
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub struct FunctionArg {
    pub local_var: VariableId,
    pub input: bool,
    pub output: bool,
}

#[derive(Copy, Clone, Debug)]
pub struct Nature {
    pub ident: Ident,
    pub abstol: ExpressionId,
    pub units: ExpressionId,
    pub access: Ident,
    pub idt_nature: NatureId,
    pub ddt_nature: NatureId,
    pub sctx: SyntaxCtx,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub ident: Ident,
    pub ports: IdRange<PortId>,
    pub parameters: IdRange<ParameterId>,
    pub analog: Block,
    pub sctx: SyntaxCtx,
}
pub type Block = Vec<StatementId>;

#[derive(Clone, Debug)]
pub enum Statement {
    Condition(ExpressionId, Block, Block),
    While(ExpressionId, Block),
    For(ForLoop),
    Contribute(DisciplineAccess, BranchId, ExpressionId),
    Assignment(VariableId, ExpressionId),
    FunctionCall(Function, IndexVec<CallArg, ExpressionId>, Span),
    Case(Cases),
}

#[derive(Clone, Debug)]
pub struct Cases {
    pub expr: ExpressionId,
    pub cases: Vec<CaseItem>,
    pub default: Block,
}

#[derive(Clone, Debug)]
pub struct CaseItem {
    pub values: Vec<ExpressionId>,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub struct ForLoop {
    pub cond: ExpressionId,
    pub init: StatementId,
    pub incr: StatementId,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct SyntaxContextData {
    pub span: Span,
    pub attributes: Attributes,
    pub parent: Option<SyntaxCtx>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    BinaryOperator(ExpressionId, Spanned<BinaryOperator>, ExpressionId),
    UnaryOperator(Spanned<UnaryOperator>, ExpressionId),
    Condition(ExpressionId, ExpressionId, ExpressionId),

    PartialDerivative(ExpressionId, Unknown),
    FunctionCall(Function, IndexVec<CallArg, ExpressionId>, Span),

    Constant(ConstVal),

    VariableReference(VariableId),
    NodeReference(NodeId),
    PortReference(PortId),
    ParameterReference(ParameterId),
    NatureReference(NatureId),
    BranchAccess(DisciplineAccess, BranchId),

    Array(Vec<ExpressionId>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BranchKind {
    PortBranch,
    Unnamed,
    UnnamedToGnd,
    Explicit,
}

#[derive(Clone, Debug)]
pub struct Branch {
    pub ident: Ident,
    pub hi: NodeId,
    pub lo: NodeId,
    pub sctx: SyntaxCtx,
    pub kind: BranchKind,
    pub current_contributions: Vec<Span>,
    pub voltage_contributions: Vec<Span>,
    pub current_acccess: Vec<Span>,
    pub voltage_access: Vec<Span>,
}
