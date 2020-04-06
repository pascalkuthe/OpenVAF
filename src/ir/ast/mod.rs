//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use core::fmt::Debug;
use std::ops::Range;
use std::ptr::NonNull;

use intrusive_collections::__core::cell::RefCell;

use crate::compact_arena::{
    CompressedRange, InvariantLifetime, NanoArena, SafeRange, SmallArena, StringArena, TinyArena,
};
use crate::ir::*;
use crate::symbol::Ident;
use crate::symbol_table::SymbolTable;
use crate::Span;

// pub use visitor::Visitor;

/*The FOLLOWING MACRO is adapted from https://github.com/llogiq/compact_arena (mk_tiny_arena!) under MIT-License:

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
    ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
    TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
    PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
    SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
    IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
*/

#[macro_export]
macro_rules! mk_ast {
    ($name:ident) => {
        let tag = $crate::compact_arena::invariant_lifetime();
        let _guard;
        let mut $name = unsafe {
            // this is not per-se unsafe but we need it to be public and
            // calling it with a non-unique `tag` would allow arena mixups,
            // which may introduce UB in `Index`/`IndexMut`
            $crate::Ast::new(tag)
        };
        // this doesn't make it to MIR, but ensures that borrowck will not
        // unify the lifetimes of two macro calls by binding the lifetime to
        // drop scope
        if false {
            struct Guard<'tag>(&'tag $crate::compact_arena::InvariantLifetime<'tag>);
            impl<'tag> ::core::ops::Drop for Guard<'tag> {
                fn drop(&mut self) {}
            }
            _guard = Guard(&tag);
        }
    };
}

//pub mod printer;
// pub mod visitor;

/// An Ast representing a parsed Verilog-AMS project (root file);
/// It provides stable indicies for every Node because the entire Tree is immutable once created;
/// It uses preallocated constant size arrays for performance

//TODO configure to use different arena sizes
pub struct Ast<'tag> {
    //Declarations
    pub(crate) branches: NanoArena<'tag, AttributeNode<'tag, BranchDeclaration>>,
    pub(crate) nets: TinyArena<'tag, AttributeNode<'tag, Net>>,
    pub(crate) ports: NanoArena<'tag, AttributeNode<'tag, Port>>,
    pub(crate) variables: TinyArena<'tag, AttributeNode<'tag, Variable<'tag>>>,
    pub(crate) parameters: TinyArena<'tag, AttributeNode<'tag, Parameter<'tag>>>,
    pub(crate) modules: NanoArena<'tag, AttributeNode<'tag, Module<'tag>>>,
    pub(crate) functions: NanoArena<'tag, AttributeNode<'tag, Function<'tag>>>,
    pub(crate) disciplines: NanoArena<'tag, AttributeNode<'tag, Discipline>>,
    pub(crate) natures: NanoArena<'tag, AttributeNode<'tag, Nature<'tag>>>,
    //Ast Items
    pub(crate) expressions: TinyArena<'tag, Node<Expression<'tag>>>,
    pub(crate) blocks: TinyArena<'tag, AttributeNode<'tag, SeqBlock<'tag>>>,
    pub(crate) attributes: TinyArena<'tag, Attribute<'tag>>,
    pub(crate) statements: TinyArena<'tag, Statement<'tag>>,
    pub(crate) string_literals: StringArena<'tag>,
    pub top_symbols: SymbolTable<'tag>,
}

impl<'tag> Ast<'tag> {
    /// Initializes a new Ast instance directly on the heap (otherwise this would likely cause a stack overflow)
    /// # Safety
    /// You should never call this yourself use mk_ast! instead!
    /// The tag might not be unique to this arena otherwise which would allow using ids from a different arena which is undefined behavior;
    /// Apart from that this function is safe. All internal unsafe functions calls are just there because initializing on heap doesn't currently work using safe code
    ///
    pub unsafe fn new(_tag: InvariantLifetime<'tag>) -> Box<Self> {
        let layout = std::alloc::Layout::new::<Self>();
        #[allow(clippy::cast_ptr_alignment)]
        //the ptr cast below has the right alignment since we are allocation using the right layout
        let mut res: NonNull<Ast<'tag>> = NonNull::new(std::alloc::alloc(layout) as *mut Self)
            .unwrap_or_else(|| std::alloc::handle_alloc_error(layout));
        NanoArena::init(&mut res.as_mut().branches);
        TinyArena::init(&mut res.as_mut().nets);
        NanoArena::init(&mut res.as_mut().ports);
        TinyArena::init(&mut res.as_mut().variables);
        TinyArena::init(&mut res.as_mut().parameters);
        NanoArena::init(&mut res.as_mut().modules);
        NanoArena::init(&mut res.as_mut().functions);
        NanoArena::init(&mut res.as_mut().disciplines);
        NanoArena::init(&mut res.as_mut().natures);
        TinyArena::init(&mut res.as_mut().expressions);
        TinyArena::init(&mut res.as_mut().blocks);
        TinyArena::init(&mut res.as_mut().attributes);
        TinyArena::init(&mut res.as_mut().statements);
        StringArena::init(&mut res.as_mut().string_literals);
        std::ptr::write(
            &mut res.as_mut().top_symbols,
            SymbolTable::with_capacity(64),
        );
        Box::from_raw(res.as_ptr())
    }
}

//TODO cfg options for different id sizes/allocs
impl_id_type!(BranchId in Ast::branches -> AttributeNode<'tag,BranchDeclaration>);

impl_id_type!(NetId in Ast::nets -> AttributeNode<'tag,Net>);

impl_id_type!(PortId in Ast::ports -> AttributeNode<'tag,Port>);

impl_id_type!(ParameterId in Ast::parameters -> AttributeNode<'tag,Parameter<'tag>>);

impl_id_type!(VariableId in Ast::variables -> AttributeNode<'tag,Variable<'tag>>);

impl<'tag> Write<VariableId<'tag>> for Ast<'tag> {
    type Data = AttributeNode<'tag, Variable<'tag>>;
    fn write(&mut self, index: VariableId<'tag>, value: Self::Data) {
        unsafe {
            //this is save for copy types  that dont implement drop
            self.variables
                .write(index.0, ::core::mem::MaybeUninit::new(value))
        }
    }
}

impl_id_type!(ModuleId in Ast::modules -> AttributeNode<'tag,Module<'tag>>);

impl_id_type!(FunctionId in Ast::functions -> AttributeNode<'tag,Function<'tag>>);

impl_id_type!(DisciplineId in Ast::disciplines -> AttributeNode<'tag,Discipline>);

impl_id_type!(ExpressionId in Ast::expressions -> Node<Expression<'tag>>);

impl_id_type!(AttributeId in Ast::attributes -> Attribute<'tag>);

impl_id_type!(StatementId in Ast::statements -> Statement<'tag>);

impl_id_type!(BlockId in Ast::blocks -> AttributeNode<'tag,SeqBlock<'tag>>);

impl_id_type!(NatureId in Ast::natures -> AttributeNode<'tag,Nature<'tag>>);

#[derive(Clone, Debug)]
pub enum TopNode<'ast> {
    Module(ModuleId<'ast>),
    Nature(NatureId<'ast>),
    Discipline(DisciplineId<'ast>),
}

#[derive(Copy, Clone)]
pub struct Nature<'ast> {
    pub name: Ident,
    pub abstol: ExpressionId<'ast>,
    pub units: ExpressionId<'ast>,
    pub access: Ident,
    pub idt_nature: Option<Ident>,
    pub ddt_nature: Option<Ident>,
}

pub enum NatureParentType {
    Nature,
    DisciplineFlow,
    DisciplinePotential,
}

#[derive(Clone)]
pub struct Module<'ast> {
    pub name: Ident,
    pub port_list: SafeRange<PortId<'ast>>,
    pub parameter_list: SafeRange<ParameterId<'ast>>,
    pub variables: SafeRange<VariableId<'ast>>,
    pub branches: SafeRange<BranchId<'ast>>,
    pub symbol_table: SymbolTable<'ast>,
    pub children: Vec<ModuleItem<'ast>>,
}

#[derive(Clone, Debug)]
pub struct Parameter<'ast> {
    pub name: Ident,
    pub parameter_type: ParameterType<'ast>,
    pub default_value: Option<ExpressionId<'ast>>,
}

#[derive(Clone, Debug)]
pub enum ParameterType<'ast> {
    Numerical {
        parameter_type: VariableType,
        included_ranges: Vec<Range<NumericalParameterRangeBound<'ast>>>,
        excluded_ranges: Vec<NumericalParameterRangeExclude<'ast>>,
    },
    String(
        //TODO string parameters
    ),
}
#[derive(Clone, Copy, Debug)]
pub struct NumericalParameterRangeBound<'ast> {
    pub inclusive: bool,
    pub bound: ExpressionId<'ast>,
}
#[derive(Clone, Debug)]
pub enum NumericalParameterRangeExclude<'ast> {
    Value(ExpressionId<'ast>),
    Range(Range<NumericalParameterRangeBound<'ast>>),
}

#[derive(Clone, Copy, Debug)]
pub struct Port {
    pub name: Ident,
    pub input: bool,
    pub output: bool,
    pub discipline: Ident, //TODO discipline
    pub signed: bool,
    pub net_type: NetType,
}

impl Default for Port {
    fn default() -> Self {
        Self {
            name: Ident::empty(),
            input: false,
            output: false,
            discipline: Ident::empty(),
            signed: false,
            net_type: NetType::UNDECLARED,
        }
    }
}

#[derive(Clone, Debug)]
pub struct BranchDeclaration {
    pub name: Ident,
    pub branch: Branch,
}

#[derive(Clone, Debug)]
pub enum Branch {
    Port(HierarchicalId),
    NetToGround(HierarchicalId),
    Nets(HierarchicalId, HierarchicalId),
}

#[derive(Clone, Copy, Debug)]
pub enum ModuleItem<'ast> {
    AnalogStmt(StatementId<'ast>),
    GenerateStatement, //TODO
}

#[derive(Clone, Copy, Debug)]
pub struct Discipline {
    pub name: Ident,
    pub flow_nature: Option<Ident>,
    pub potential_nature: Option<Ident>,
    pub continuous: Option<bool>,
}

#[derive(Clone, Debug)]
pub struct Function<'ast> {
    pub name: Ident,
    pub args: Vec<Ident>,
    pub body: StatementId<'ast>,
}

#[derive(Debug, Clone, Copy)]
pub struct Net {
    pub name: Ident,
    pub discipline: Ident,
    pub signed: bool,
    pub net_type: NetType,
}

#[derive(Clone, Copy, Debug)]
pub struct Variable<'ast> {
    pub name: Ident,
    pub variable_type: VariableType,
    pub default_value: Option<ExpressionId<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NetType {
    UNDECLARED,
    REG,
    WREAL,
    SUPPLY0,
    SUPPLY1,
    TRI,
    TRIAND,
    TRIOR,
    TRI0,
    TRI1,
    WIRE,
    UWIRE,
    WAND,
    WOR,
    GROUND,
}

#[derive(Clone)]
pub enum Statement<'ast> {
    Block(BlockId<'ast>),
    Condition(AttributeNode<'ast, Condition<'ast>>),
    Contribute(
        Attributes<'ast>,
        Ident,
        Node<BranchAccess>,
        ExpressionId<'ast>,
    ),
    //  TODO IndirectContribute(),
    Assign(Attributes<'ast>, HierarchicalId, ExpressionId<'ast>),
    FunctionCall(Attributes<'ast>, HierarchicalId, Vec<ExpressionId<'ast>>),
    BuiltInFunctionCall(AttributeNode<'ast, BuiltInFunctionCall<'ast>>),
}
#[derive(Clone)]
pub struct SeqBlock<'ast> {
    pub scope: Option<BlockScope<'ast>>,
    pub statements: Vec<StatementId<'ast>>,
}
#[derive(Clone, Debug)]
pub struct BlockScope<'ast> {
    pub name: Ident,
    pub symbols: SymbolTable<'ast>,
}

#[derive(Clone)]
pub struct Condition<'ast> {
    pub main_condition: ExpressionId<'ast>,
    pub main_condition_statement: StatementId<'ast>,
    pub else_ifs: Vec<(ExpressionId<'ast>, StatementId<'ast>)>,
    pub else_statement: Option<StatementId<'ast>>,
}

#[derive(Clone, Debug)]
pub enum Expression<'ast> {
    BinaryOperator(ExpressionId<'ast>, Node<BinaryOperator>, ExpressionId<'ast>),
    UnaryOperator(Node<UnaryOperator>, ExpressionId<'ast>),
    Condtion(
        ExpressionId<'ast>,
        Span,
        ExpressionId<'ast>,
        Span,
        ExpressionId<'ast>,
    ),
    Primary(Primary<'ast>),
}

#[derive(Clone, Debug)]
pub enum BranchAccess {
    Explicit(HierarchicalId),
    Implicit(Branch),
}

#[derive(Clone, Debug)]
pub enum Primary<'ast> {
    Integer(i64),
    UnsignedInteger(u32),
    Real(f64),
    String(CompressedRange<'ast>),
    VariableOrNetReference(HierarchicalId),
    FunctionCall(HierarchicalId, Vec<ExpressionId<'ast>>),
    SystemFunctionCall(Ident /*TODO args*/),
    BranchAccess(Ident, Node<BranchAccess>),
    BuiltInFunctionCall(BuiltInFunctionCall<'ast>),
}

#[derive(Copy, Clone, Debug)]
pub enum BuiltInFunctionCall<'ast> {
    Pow(ExpressionId<'ast>, ExpressionId<'ast>),
    Sqrt(ExpressionId<'ast>),

    Hypot(ExpressionId<'ast>, ExpressionId<'ast>),
    Exp(ExpressionId<'ast>),
    Ln(ExpressionId<'ast>),
    Log(ExpressionId<'ast>),

    Min(ExpressionId<'ast>, ExpressionId<'ast>),
    Max(ExpressionId<'ast>, ExpressionId<'ast>),
    Abs(ExpressionId<'ast>),
    Floor(ExpressionId<'ast>),
    Ceil(ExpressionId<'ast>),

    Sin(ExpressionId<'ast>),
    Cos(ExpressionId<'ast>),
    Tan(ExpressionId<'ast>),

    ArcSin(ExpressionId<'ast>),
    ArcCos(ExpressionId<'ast>),
    ArcTan(ExpressionId<'ast>),
    ArcTan2(ExpressionId<'ast>, ExpressionId<'ast>),

    SinH(ExpressionId<'ast>),
    CosH(ExpressionId<'ast>),
    TanH(ExpressionId<'ast>),

    ArcSinH(ExpressionId<'ast>),
    ArcCosH(ExpressionId<'ast>),
    ArcTanH(ExpressionId<'ast>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Sum,
    Subtract,
    Multiply,
    Divide,
    Exponent,
    Modulus,

    ShiftLeft,
    ShiftRight,

    LessThen,
    LessEqual,
    GreaterThen,
    GreaterEqual,
    LogicEqual,
    LogicalNotEqual,

    LogicOr,
    LogicAnd,

    Xor,
    NXor,
    And,
    Or,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    BitNegate,
    LogicNegate,
    ArithmeticNegate,
    ExplicitPositive,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VariableType {
    TIME,
    INTEGER,
    REAL,
    REALTIME,
}

#[derive(Clone, Debug)]
pub struct HierarchicalId {
    pub names: Vec<Ident>,
}

impl HierarchicalId {
    pub fn span(&self) -> Span {
        self.names[0]
            .span
            .extend(self.names[self.names.len() - 1].span)
    }
}
impl From<Vec<Ident>> for HierarchicalId {
    fn from(raw: Vec<Ident>) -> Self {
        Self { names: raw }
    }
}
