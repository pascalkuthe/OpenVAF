/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use index_vec::{index_vec, IndexVec};

use crate::ast::{Ast, BinaryOperator, NetType, UnaryOperator, Variable};

use crate::derivatives::Unknown;
use crate::ir::ids::{
    AttributeId, BranchId, DisciplineId, ExpressionId, FunctionId, IdRange, ModuleId, NatureId,
    NetId, ParameterId, PortId, StatementId, VariableId,
};
use crate::ir::{
    Attribute, Attributes, DoubleArgMath, Node, NoiseSource, ParameterExcludeConstraint,
    ParameterRangeConstraint, Port, SingleArgMath, Spanned, StopTaskKind, SystemFunctionCall,
};
use crate::literals::StringLiteral;
use crate::sourcemap::span::DUMMY_SP;
use crate::symbol::Ident;
use std::mem::take;

/// An High level (tree) IR representing a Verilog-AMS project;
/// It provides stable indicies for every Node because the entire Tree is immutable once created;
/// It uses preallocated constant size arrays for better performance
/// Compared to an AST all references are resolved to their respective ids here and unnecessary constructs like blocks are ignored

#[derive(Default, Debug, Clone)]
pub struct Hir {
    pub parameters: IndexVec<ParameterId, Node<Parameter>>,
    pub branches: IndexVec<BranchId, Node<Branch>>,
    pub nets: IndexVec<NetId, Node<Net>>,
    pub ports: IndexVec<PortId, Port>,
    pub variables: IndexVec<VariableId, Node<Variable>>,
    pub modules: IndexVec<ModuleId, Node<Module>>,
    pub functions: IndexVec<FunctionId, Node<Function>>,
    pub disciplines: IndexVec<DisciplineId, Node<Discipline>>,
    pub natures: IndexVec<NatureId, Node<Nature>>,
    pub expressions: IndexVec<ExpressionId, Spanned<Expression>>,
    pub attributes: IndexVec<AttributeId, Attribute>,
    pub statements: IndexVec<StatementId, Node<Statement>>,
}
impl Hir {
    pub(crate) fn init(ast: &mut Ast) -> Self {
        Self {
            // The following AST items do change (references are mapped but the data structure doesnt change so we init it like this)
            variables: take(&mut ast.variables),
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
        }
    }
}

impl_id_type!(BranchId in Hir::branches -> Node<Branch>);
impl_id_type!(NetId in Hir::nets -> Node<Net>);
impl_id_type!(PortId in Hir::ports -> Port);
impl_id_type!(VariableId in Hir::variables ->  Node<Variable>);
impl_id_type!(ModuleId in Hir::modules -> Node<Module>);
impl_id_type!(FunctionId in Hir::functions -> Node<Function>);
impl_id_type!(DisciplineId in Hir::disciplines -> Node<Discipline>);
impl_id_type!(ExpressionId in Hir::expressions -> Spanned<Expression>);
impl_id_type!(AttributeId in Hir::attributes -> Attribute);
impl_id_type!(StatementId in Hir::statements -> Node<Statement>);
impl_id_type!(NatureId in Hir::natures -> Node<Nature>);
impl_id_type!(ParameterId in Hir::parameters -> Node<Parameter>);

#[derive(Clone, Debug)]
pub struct Parameter {
    pub ident: Ident,
    pub param_type: ParameterType,
    pub default: ExpressionId,
}
impl Parameter {
    pub const PLACEHOLDER: Node<Self> = {
        let contents = Parameter {
            ident: Ident::DUMMY,
            param_type: ParameterType::Real(Vec::new(), Vec::new()),
            default: ExpressionId::from_raw_unchecked(u32::MAX),
        };

        Node {
            contents,
            attributes: Attributes::EMPTY,
            span: DUMMY_SP,
        }
    };
}

#[derive(Clone, Debug)]
pub enum ParameterType {
    Real(
        Vec<ParameterRangeConstraint<ExpressionId>>,
        Vec<ParameterExcludeConstraint<ExpressionId>>,
    ),
    Integer(
        Vec<ParameterRangeConstraint<ExpressionId>>,
        Vec<ParameterExcludeConstraint<ExpressionId>>,
    ),
    String(Vec<ExpressionId>, Vec<ExpressionId>),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub ident: Ident,
    pub args: Vec<FunctionArg>,
    pub return_variable: VariableId,
    pub body: Block,
}

impl Function {
    pub const PLACEHOLDER: Node<Self> = {
        let contents = Self {
            ident: Ident::DUMMY,
            args: Vec::new(),
            return_variable: VariableId::from_raw_unchecked(u32::MAX),
            body: IdRange(
                StatementId::from_raw_unchecked(u32::MAX)
                    ..StatementId::from_raw_unchecked(u32::MAX),
            ),
        };
        Node {
            attributes: Attributes {
                start: AttributeId::from_raw_unchecked(u16::MAX),
                len: 0,
            },
            span: DUMMY_SP,
            contents,
        }
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
}

#[derive(Copy, Clone, Debug)]
pub struct Nature {
    pub ident: Ident,
    pub abstol: ExpressionId,
    pub units: ExpressionId,
    pub access: Ident,
    pub idt_nature: NatureId,
    pub ddt_nature: NatureId,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub ident: Ident,
    pub ports: IdRange<PortId>,
    pub parameters: IdRange<ParameterId>,
    pub analog: Block,
}
pub type Block = IdRange<StatementId>;

#[derive(Clone, Copy, Debug)]
pub struct Branch {
    pub ident: Ident,
    pub hi: NetId,
    pub lo: NetId,
}

#[derive(Clone, Copy, Debug)]
pub struct Net {
    pub ident: Ident,
    pub discipline: DisciplineId,
    pub net_type: NetType,
}
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum DisciplineAccess {
    Potential,
    Flow,
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
    pub init: (VariableId, ExpressionId),
    pub incr: (VariableId, ExpressionId),
    pub body: Block,
}

#[derive(Clone, Debug)]
pub enum Expression {
    BinaryOperator(ExpressionId, Spanned<BinaryOperator>, ExpressionId),
    UnaryOperator(Spanned<UnaryOperator>, ExpressionId),
    Condtion(ExpressionId, ExpressionId, ExpressionId),
    Primary(Primary),
}
#[derive(Clone, Debug)]
pub enum Primary {
    Integer(i64),
    UnsignedInteger(u32),
    Real(f64),
    String(StringLiteral),

    VariableReference(VariableId),
    NetReference(NetId),
    PortReference(PortId),
    ParameterReference(ParameterId),

    PortFlowAccess(PortId),
    BranchAccess(DisciplineAccess, BranchId),
    Derivative(ExpressionId, Unknown),

    BuiltInFunctionCall1p(SingleArgMath, ExpressionId),
    BuiltInFunctionCall2p(DoubleArgMath, ExpressionId, ExpressionId),
    FunctionCall(FunctionId, Vec<ExpressionId>),
    SystemFunctionCall(SystemFunctionCall<ExpressionId, ExpressionId, PortId, ParameterId>),
    Noise(NoiseSource<ExpressionId, ()>, Option<ExpressionId>),
}
