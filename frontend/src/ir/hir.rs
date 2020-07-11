/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use crate::ir::*;
use index_vec::*;

use crate::ast::Parameter;
use crate::ast::{BinaryOperator, NetType, UnaryOperator, Variable};

use crate::derivatives::Unknown;
use crate::ir::ids::IdRange;
use crate::literals::StringLiteral;
use crate::sourcemap::span::DUMMY_SP;
use crate::symbol::Ident;
use crate::{Ast, Span};
use std::mem::take;

//pub mod visitor;

/// An High level (tree) IR representing a Verilog-AMS project;
/// It provides stable indicies for every Node because the entire Tree is immutable once created;
/// It uses preallocated constant size arrays for better performance
/// Compared to an AST all references are resolved to their respective ids here and unnecessary constructs like blocks are ignored

#[derive(Default, Debug, Clone)]
pub struct Hir {
    pub parameters: IndexVec<ParameterId, AttributeNode<Parameter>>,
    pub branches: IndexVec<BranchId, AttributeNode<BranchDeclaration>>,
    pub nets: IndexVec<NetId, AttributeNode<Net>>,
    pub ports: IndexVec<PortId, Port>,
    pub variables: IndexVec<VariableId, AttributeNode<Variable>>,
    pub modules: IndexVec<ModuleId, AttributeNode<Module>>,
    pub functions: IndexVec<FunctionId, AttributeNode<Function>>,
    pub disciplines: IndexVec<DisciplineId, AttributeNode<Discipline>>,
    pub natures: IndexVec<NatureId, AttributeNode<Nature>>,
    pub expressions: IndexVec<ExpressionId, Node<Expression>>,
    pub attributes: IndexVec<AttributeId, Attribute>,
    pub statements: IndexVec<StatementId, Statement>,
}
impl Hir {
    pub(crate) fn init(ast: &mut Ast) -> Self {
        Self {
            // The following AST items do change (references are mapped but the data structure doesnt change so we init it like this)
            parameters: take(&mut ast.parameters),
            variables: take(&mut ast.variables),
            attributes: take(&mut ast.attributes),

            // We init empty vecs because that doesnt allocate. We will reinit these during AST lowering
            branches: IndexVec::with_capacity(ast.branches.len()),
            nets: IndexVec::new(),
            ports: IndexVec::new(),
            modules: IndexVec::with_capacity(ast.modules.len()),
            functions: index_vec![Function::placeholder();ast.functions.len()],
            disciplines: IndexVec::new(),
            natures: IndexVec::new(),
            expressions: IndexVec::with_capacity(ast.expressions.len()),
            statements: IndexVec::with_capacity(ast.statements.len()),
        }
    }
}

impl_id_type!(BranchId in Hir::branches -> AttributeNode<BranchDeclaration>);
impl_id_type!(NetId in Hir::nets -> AttributeNode<Net>);
impl_id_type!(PortId in Hir::ports -> Port);
impl_id_type!(VariableId in Hir::variables ->  AttributeNode<Variable>);
impl_id_type!(ModuleId in Hir::modules -> AttributeNode<Module>);
impl_id_type!(FunctionId in Hir::functions -> AttributeNode<Function>);
impl_id_type!(DisciplineId in Hir::disciplines -> AttributeNode<Discipline>);
impl_id_type!(ExpressionId in Hir::expressions -> Node<Expression>);
impl_id_type!(AttributeId in Hir::attributes -> Attribute);
impl_id_type!(StatementId in Hir::statements -> Statement);
impl_id_type!(NatureId in Hir::natures -> AttributeNode<Nature>);
impl_id_type!(ParameterId in Hir::parameters -> AttributeNode<Parameter>);

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Ident,
    pub args: Vec<FunctionArg>,
    pub return_variable: VariableId,
    pub body: Block,
}

impl Function {
    #[inline]
    #[must_use]
    pub const fn placeholder() -> AttributeNode<Function> {
        let contents = Self {
            name: Ident::DUMMY_IDNT,
            args: Vec::new(),
            return_variable: VariableId::from_raw_unchecked(0),
            body: IdRange(StatementId::from_raw_unchecked(0)..StatementId::from_raw_unchecked(0)),
        };
        AttributeNode {
            attributes: Attributes {
                start: AttributeId::from_raw_unchecked(0),
                len: 0,
            },
            span: DUMMY_SP,
            contents,
        }
    }
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
    pub continuous: Option<bool>,
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
    pub port_list: IdRange<PortId>,
    pub analog: Block,
}
pub type Block = IdRange<StatementId>;
#[derive(Clone, Debug)]
pub struct Condition {
    pub condition: ExpressionId,
    pub if_statements: Block,
    pub else_statements: Block,
}
#[derive(Clone, Copy, Debug)]
pub struct Port {
    pub input: bool,
    pub output: bool,
    pub net: NetId,
}

#[derive(Clone, Copy, Debug)]
pub struct BranchDeclaration {
    pub name: Ident,
    pub branch: Branch,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Branch {
    Port(PortId),
    Nets(NetId, NetId),
}
#[derive(Clone, Copy, Debug)]
pub struct Net {
    pub name: Ident,
    pub discipline: DisciplineId,
    pub signed: bool,
    pub net_type: NetType,
}
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum DisciplineAccess {
    Potential,
    Flow,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Condition(AttributeNode<Condition>),

    While(AttributeNode<WhileLoop>),
    For(AttributeNode<ForLoop>),

    Contribute(Attributes, DisciplineAccess, BranchId, ExpressionId),
    //  TODO IndirectContribute(),
    Assignment(Attributes, VariableId, ExpressionId),

    StopTask(AttributeNode<StopTaskKind>, PrintOnFinish),

    Case(AttributeNode<Cases>),
}

#[derive(Clone, Debug)]
pub struct Cases {
    pub expr: ExpressionId,
    pub cases: Vec<Node<CaseItem>>,
    pub default: Block,
}

#[derive(Clone, Debug)]
pub struct CaseItem {
    pub values: Vec<ExpressionId>,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub struct ForLoop {
    pub condition: ExpressionId,
    pub initial_var: VariableId,
    pub initial_expr: ExpressionId,
    pub increment_var: VariableId,
    pub increment_expr: ExpressionId,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub struct WhileLoop {
    pub condition: ExpressionId,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub enum Expression {
    BinaryOperator(ExpressionId, Node<BinaryOperator>, ExpressionId),
    UnaryOperator(Node<UnaryOperator>, ExpressionId),
    Condtion(ExpressionId, Span, ExpressionId, Span, ExpressionId),
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

    BranchAccess(DisciplineAccess, BranchId),
    Derivative(ExpressionId, Unknown),

    BuiltInFunctionCall1p(BuiltInFunctionCall1p, ExpressionId),
    BuiltInFunctionCall2p(BuiltInFunctionCall2p, ExpressionId, ExpressionId),
    FunctionCall(FunctionId, Vec<ExpressionId>),
    SystemFunctionCall(SystemFunctionCall<ExpressionId, ExpressionId, PortId, ParameterId>),
    Noise(NoiseSource<ExpressionId, ()>, Option<StringLiteral>),
}
