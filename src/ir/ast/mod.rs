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
use std::rc::Rc;

pub use visitor::Visitor;

use crate::compact_arena::{Idx16, Idx8, InvariantLifetime, NanoArena, SafeRange, TinyArena};
use crate::ir::{
    AttributeId, BlockId, BranchId, DisciplineId, ExpressionId, FunctionId, ModuleId, NetId,
    PortId, StatementId, VariableId,
};
use crate::symbol::Ident;
use crate::symbol_table::SymbolTable;
use crate::Span;

/*The FOLLOWING MACRO is adapted from https://github.com/llogiq/compact_arena (mk_tiny_arena!) under MIT-License

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

pub mod visitor;

pub type Attributes<'ast> = SafeRange<AttributeId<'ast>>;
#[derive(Clone, Copy, Debug)]
pub struct Node<T: Clone> {
    pub source: Span,
    pub contents: T,
}
impl<T: Clone> Node<T> {
    pub fn new(contents: T, source: Span) -> Self {
        Self { contents, source }
    }
}

/// An Ast representing a parser Verilog-AMS project (root file);
/// It provides stable indicies for every Node because the entire is immutable once created;
/// It uses preallocated constant size arrays for performance so you should box this as this is a lot of data to put on the stack

//TODO make this into a general proc macro with lifetimes like compact arena
pub struct Ast<'tag> {
    //TODO configure to use different arena sizes
    //Declarations
    //    parameters: NanoArena<'tag,Parameter>,
    //    nature: NanoArena<'tag,Nature>
    pub(super) branches: NanoArena<'tag, AttributeNode<'tag, BranchDeclaration>>,
    pub(super) nets: TinyArena<'tag, AttributeNode<'tag, Net>>,
    pub(super) ports: NanoArena<'tag, AttributeNode<'tag, Port>>,
    pub(super) variables: TinyArena<'tag, AttributeNode<'tag, Variable<'tag>>>,
    pub(super) modules: NanoArena<'tag, AttributeNode<'tag, Module<'tag>>>,
    pub(super) functions: NanoArena<'tag, AttributeNode<'tag, Function<'tag>>>,
    pub(super) disciplines: NanoArena<'tag, AttributeNode<'tag, Discipline>>,
    //Ast Items
    pub(super) expressions: TinyArena<'tag, Node<Expression<'tag>>>,
    pub(super) blocks: NanoArena<'tag, AttributeNode<'tag, SeqBlock<'tag>>>,
    pub(super) attributes: TinyArena<'tag, Attribute>,
    pub(super) statements: TinyArena<'tag, Statement<'tag>>,
    pub top_nodes: Vec<TopNode<'tag>>, //would prefer this to be stored here instead of somewhere else on the heap but its probably fine for now
    pub top_symbols: SymbolTable<'tag>,
}
///this module contains copys of the dfinitions of tiny/small arena so we are able to acess internal fields for initialisation on the heap using pointers

impl<'tag> Ast<'tag> {
    /// # Safety
    /// You should never call this yourself use mk_ast! instead!
    /// The tag might not be unique to this arena otherwise which would allow using ids from a different arena which is undfined behavior;
    /// Apart from that this function should be safe all internal unsafe functions calls are there to allow
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
        NanoArena::init(&mut res.as_mut().modules);
        NanoArena::init(&mut res.as_mut().functions);
        NanoArena::init(&mut res.as_mut().disciplines);
        TinyArena::init(&mut res.as_mut().expressions);
        NanoArena::init(&mut res.as_mut().blocks);
        TinyArena::init(&mut res.as_mut().attributes);
        TinyArena::init(&mut res.as_mut().statements);
        std::ptr::write(&mut res.as_mut().top_nodes, Vec::with_capacity(64));
        std::ptr::write(
            &mut res.as_mut().top_symbols,
            SymbolTable::with_capacity(64),
        );
        Box::from_raw(res.as_ptr())
    }
}

//TODO cfg options for different id sizes/allocs
impl_id_type_for_container!(BranchId(Idx8): AttributeNode<'tag,BranchDeclaration>; in Ast::branches);
impl_id_type_for_container!(NetId(Idx16): AttributeNode<'tag,Net>; in Ast::nets);
impl_id_type_for_container!(PortId(Idx8): AttributeNode<'tag,Port>; in Ast::ports);
impl_id_type_for_container!(VariableId(Idx16): AttributeNode<'tag,Variable<'tag>>; in Ast::variables);
impl_id_type_for_container!(ModuleId(Idx8): AttributeNode<'tag,Module<'tag>>; in Ast::modules);
impl_id_type_for_container!(FunctionId(Idx8): AttributeNode<'tag,Function<'tag>>; in Ast::functions);
impl_id_type_for_container!(DisciplineId(Idx8): AttributeNode<'tag,Discipline>; in Ast::disciplines);
impl_id_type_for_container!(ExpressionId(Idx16): Node<Expression<'tag>>; in Ast::expressions);
impl_id_type_for_container!(AttributeId(Idx16): Attribute; in Ast::attributes);
impl_id_type_for_container!(StatementId(Idx16): Statement<'tag>; in Ast::statements);
impl_id_type_for_container!(BlockId(Idx8): AttributeNode<'tag,SeqBlock<'tag>>; in Ast::blocks);

pub type Attribute = ();

#[derive(Clone, Copy)]
pub struct AttributeNode<'ast, T: Clone> {
    pub attributes: Attributes<'ast>,
    pub source: Span,
    pub contents: T,
}

#[derive(Clone)]
pub enum TopNode<'tag> {
    Module(ModuleId<'tag>),
    Nature,
    Discipline,
}

#[derive(Clone)]
pub struct Module<'ast> {
    pub name: Ident,
    pub port_list: SafeRange<PortId<'ast>>,
    //    pub parameter_list: Option<Range<ParameterId<'ast>>>
    pub symbol_table: SymbolTable<'ast>,
    pub children: Vec<ModuleItem<'ast>>,
}
#[derive(Clone, Copy)]
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

#[derive(Clone)]
pub struct BranchDeclaration {
    pub name: Ident,
    pub branch: Branch,
}

#[derive(Clone)]
pub enum Branch {
    Port(HierarchicalId),
    Nets(HierarchicalId, HierarchicalId),
}

#[derive(Clone, Copy)]
pub enum ModuleItem<'ast> {
    AnalogStmt(StatementId<'ast>),
    GenerateStatement, //TODO
}
#[derive(Clone, Copy, Debug)]
pub struct Discipline {
    pub name: Ident,
    pub flow_nature: Ident,
    pub potential_nature: Ident,
}
#[derive(Clone)]
pub struct Function<'ast> {
    pub name: Ident,
    pub args: Rc<Vec<Ident>>,
    pub body: StatementId<'ast>,
}
#[derive(Debug, Clone, Copy)]
pub struct Net {
    pub name: Ident,
    pub discipline: Ident,
    pub signed: bool,
    pub net_type: NetType,
}
#[derive(Clone, Copy)]
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
}

#[derive(Clone)]
pub enum Statement<'ast> {
    Block(BlockId<'ast>),
    Condition(AttributeNode<'ast, Condition<'ast>>),
    Contribute(Attributes<'ast>, Ident, BranchAccess, ExpressionId<'ast>),
    //  TODO IndirectContribute(),
    Assign(Attributes<'ast>, HierarchicalId, ExpressionId<'ast>),
    FunctionCall(
        Attributes<'ast>,
        HierarchicalId,
        Rc<Vec<ExpressionId<'ast>>>,
    ),
}
#[derive(Clone)]
pub struct SeqBlock<'ast> {
    pub scope: Option<BlockScope<'ast>>,
    pub statements: Vec<StatementId<'ast>>,
}
#[derive(Clone)]
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

#[derive(Clone)]
pub enum Expression<'ast> {
    BinaryOperator(ExpressionId<'ast>, Node<BinaryOperator>, ExpressionId<'ast>),
    UnaryOperator(Node<UnaryOperator>, ExpressionId<'ast>),
    Primary(Primary<'ast>),
}

#[derive(Clone)]
pub enum BranchAccess {
    Explicit(HierarchicalId),
    Implicit(Branch),
}

#[derive(Clone)]
pub enum Primary<'ast> {
    Integer(i64),
    UnsignedInteger(u32),
    Real(f64),
    VariableOrNetReference(HierarchicalId),
    FunctionCall(HierarchicalId, Option<SafeRange<ExpressionId<'ast>>>),
    BranchAccess(Ident, BranchAccess),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Condition,
    Either,
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
impl From<Vec<Ident>> for HierarchicalId {
    fn from(raw: Vec<Ident>) -> Self {
        Self { names: raw }
    }
}
