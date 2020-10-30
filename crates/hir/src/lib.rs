/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use openvaf_data_structures::index_vec::{index_vec, IndexVec};

use openvaf_ast::Ast;
pub use openvaf_ast::{BinaryOperator, NetType, Type};

pub use openvaf_ir::ids::{
    AttributeId, BranchId, DisciplineId, ExpressionId, FunctionId, IdRange, ModuleId, NatureId,
    NetId, ParameterId, PortId, StatementId, SyntaxCtx, VariableId,
};

pub use openvaf_ir::{
    impl_id_type, Attribute, Attributes, DoubleArgMath, Node, NoiseSource,
    ParameterExcludeConstraint, ParameterRangeConstraint, Port, SingleArgMath, Spanned,
    StopTaskKind, UnaryOperator, Unknown,
};

pub type SystemFunctionCall = openvaf_ir::SystemFunctionCall<PortId, ParameterId>;

use openvaf_session::sourcemap::span::DUMMY_SP;
use openvaf_session::symbols::Ident;
use std::mem::take;

pub use openvaf_ast::ConstVal;
use openvaf_session::sourcemap::Span;
use std::fmt::{Display, Formatter};

/// An High level (tree) IR representing a Verilog-AMS project;
/// It provides stable indicies for every Node because the entire Tree is immutable once created;
/// It uses preallocated constant size arrays for better performance
/// Compared to an AST all references are resolved to their respective ids here and unnecessary constructs like blocks are ignored

#[derive(Default, Debug, Clone)]
pub struct Hir {
    pub parameters: IndexVec<ParameterId, Parameter>,
    pub branches: IndexVec<BranchId, Branch>,
    pub nets: IndexVec<NetId, Net>,
    pub ports: IndexVec<PortId, Port>,
    pub variables: IndexVec<VariableId, Variable>,
    pub modules: IndexVec<ModuleId, Module>,
    pub functions: IndexVec<FunctionId, Function>,
    pub disciplines: IndexVec<DisciplineId, Discipline>,
    pub natures: IndexVec<NatureId, Nature>,
    pub expressions: IndexVec<ExpressionId, Spanned<Expression>>,
    pub attributes: IndexVec<AttributeId, Attribute>,
    pub statements: IndexVec<StatementId, (Statement, SyntaxCtx)>,
    pub syntax_ctx: IndexVec<SyntaxCtx, SyntaxContextData>,
}
impl Hir {
    pub fn init(ast: &mut Ast) -> Self {
        let mut syntax_ctx = IndexVec::with_capacity(
            ast.parameters.len()
                + ast.variables.len()
                + ast.nets.len()
                + ast.disciplines.len()
                + ast.statements.len()
                + ast.branches.len()
                + ast.blocks.len()
                + ast.modules.len()
                + ast.functions.len()
                + 20,
        );

        syntax_ctx.push(SyntaxContextData {
            span: DUMMY_SP,
            attributes: Attributes::EMPTY,
            parent: None,
        });

        Self {
            // The following AST items do change (references are mapped but the data structure doesnt change so we init it like this)
            variables: index_vec![Variable::PLACEHOLDER; ast.variables.len()],
            attributes: take(&mut ast.attributes),
            ports: take(&mut ast.ports),

            parameters: index_vec![Parameter::PLACEHOLDER; ast.parameters.len()],
            functions: index_vec![Function::PLACEHOLDER; ast.functions.len()],

            branches: IndexVec::with_capacity(ast.branches.len()),
            nets: IndexVec::new(),
            modules: IndexVec::with_capacity(ast.modules.len()),
            disciplines: IndexVec::new(),
            natures: IndexVec::new(),
            expressions: IndexVec::with_capacity(ast.expressions.len()),
            statements: IndexVec::with_capacity(ast.statements.len()),
            syntax_ctx,
        }
    }
}

impl_id_type!(BranchId in Hir => branches as Branch);
impl_id_type!(NetId in Hir => nets as Net);
impl_id_type!(PortId in Hir => ports as Port);
impl_id_type!(VariableId in Hir => variables as  Variable);
impl_id_type!(ModuleId in Hir => modules as Module);
impl_id_type!(FunctionId in Hir => functions as Function);
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
pub struct Function {
    pub ident: Ident,
    pub args: Vec<FunctionArg>,
    pub return_variable: VariableId,
    pub body: Block,
    pub sctx: SyntaxCtx,
}

impl Function {
    pub const PLACEHOLDER: Self = Self {
        ident: Ident::DUMMY,
        args: Vec::new(),
        return_variable: VariableId::from_raw_unchecked(u32::MAX),
        body: Vec::new(),
        sctx: SyntaxCtx::ROOT,
    };
}

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub struct FunctionArg {
    pub local_var: VariableId,
    pub input: bool,
    pub output: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct Discipline {
    pub ident: Ident,
    pub flow_nature: Option<NatureId>,
    pub potential_nature: Option<NatureId>,
    pub continuous: bool,
    pub sctx: SyntaxCtx,
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

#[derive(Clone, Copy, Debug)]
pub struct Branch {
    pub ident: Ident,
    pub hi: NetId,
    pub lo: NetId,
    pub sctx: SyntaxCtx,
}

#[derive(Clone, Copy, Debug)]
pub struct Net {
    pub ident: Ident,
    pub discipline: DisciplineId,
    pub net_type: NetType,
    pub sctx: SyntaxCtx,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum DisciplineAccess {
    Potential,
    Flow,
}

impl Display for DisciplineAccess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Potential => f.write_str("pot"),
            Self::Flow => f.write_str("flow"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Condition(ExpressionId, Block, Block),

    While(ExpressionId, Block),
    For(ForLoop),

    Contribute(DisciplineAccess, BranchId, ExpressionId),
    //  TODO IndirectContribute(),
    Assignment(VariableId, ExpressionId),

    StopTask(StopTaskKind, Option<ExpressionId>),

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

#[derive(Clone, Debug)]
pub enum Expression {
    BinaryOperator(ExpressionId, Spanned<BinaryOperator>, ExpressionId),
    BuiltInFunctionCall2p(DoubleArgMath, ExpressionId, ExpressionId),

    UnaryOperator(Spanned<UnaryOperator>, ExpressionId),
    BuiltInFunctionCall1p(SingleArgMath, ExpressionId),

    Condition(ExpressionId, ExpressionId, ExpressionId),

    PartialDerivative(ExpressionId, Unknown),
    TimeDerivative(ExpressionId),

    FunctionCall(FunctionId, Vec<ExpressionId>),
    SystemFunctionCall(SystemFunctionCall),

    Noise(NoiseSource<ExpressionId, ()>, Option<ExpressionId>),

    Primary(Primary),
    Array(Vec<ExpressionId>),
}

#[derive(Clone, Debug)]
pub enum Primary {
    Constant(ConstVal),

    VariableReference(VariableId),
    NetReference(NetId),
    PortReference(PortId),
    ParameterReference(ParameterId),

    PortFlowAccess(PortId),
    BranchAccess(DisciplineAccess, BranchId),
}
