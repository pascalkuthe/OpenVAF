use std::ops::Range;
use std::path::Iter;
use std::ptr;
use std::ptr::NonNull;
use std::rc::Rc;

use crate::compact_arena::{NanoArena, SafeRange, TinyArena};
use crate::ir::ast::{
    Ast, Attribute, AttributeNode, Attributes, BinaryOperator, Function, ModuleItem, Nature,
    NetType, Node, TopNode, UnaryOperator, Variable,
};
use crate::ir::{
    AttributeId, BranchId, DisciplineId, ExpressionId, FunctionId, ModuleId, NatureId, NetId,
    PortId, StatementId, VariableId,
};
use crate::symbol::Ident;

/// An High level (tree) IR representing a Verilog-AMS project;
/// It provides stable indicies for every Node because the entire Tree is immutable once created;
/// It uses preallocated constant size arrays for better performance
/// Compared to an AST all references are resolved to their respective ids here and unnecessary constructs like blocks are ignored

//TODO make this into a general proc macro with lifetimes like compact arena
pub struct Hir<'tag> {
    //TODO unsized
    //TODO configure to use different arena sizes
    //Declarations
    //    parameters: NanoArena<'tag,Parameter>,
    //    nature: NanoArena<'tag,Nature>
    branches: NanoArena<'tag, AttributeNode<'tag, BranchDeclaration<'tag>>>,
    nets: TinyArena<'tag, AttributeNode<'tag, Net<'tag>>>,
    ports: NanoArena<'tag, Port<'tag>>,
    variables: TinyArena<'tag, AttributeNode<'tag, Variable<'tag>>>,
    modules: NanoArena<'tag, AttributeNode<'tag, Module<'tag>>>,
    functions: NanoArena<'tag, AttributeNode<'tag, Function<'tag>>>,
    disciplines: NanoArena<'tag, AttributeNode<'tag, Discipline<'tag>>>,
    natures: NanoArena<'tag, AttributeNode<'tag, Nature>>,
    //Ast Items
    expressions: TinyArena<'tag, Node<Expression<'tag>>>,
    attributes: TinyArena<'tag, Attribute>,
    statements: TinyArena<'tag, Statement<'tag>>,
}
///this module contains copys of the definitions of tiny/small arena so we are able to acess internal fields for initialisation on the heap using pointers

impl<'tag> Hir<'tag> {
    /// # Safety
    /// You should never call this yourself use mk_ast! instead!
    /// The tag might not be unique to this arena otherwise which would allow using ids from a different arena which is undfined behavior;
    /// Apart from that this function should be safe all internal unsafe functions calls are there to allow
    pub(crate) unsafe fn partial_initalize<'astref>(ast: &'astref mut Ast<'tag>) -> Box<Self> {
        let layout = std::alloc::Layout::new::<Self>();
        #[allow(clippy::cast_ptr_alignment)]
        //the ptr cast below has the right alignment since we are allocation using the right layout
        let mut res: NonNull<Self> = NonNull::new(std::alloc::alloc(layout) as *mut Self)
            .unwrap_or_else(|| std::alloc::handle_alloc_error(layout));
        TinyArena::copy_to(&mut res.as_mut().variables, &ast.variables);
        TinyArena::copy_to(&mut res.as_mut().attributes, &ast.attributes);
        NanoArena::init_from(&mut res.as_mut().branches, &ast.branches);
        TinyArena::init_from(&mut res.as_mut().nets, &ast.nets);
        NanoArena::init_from(&mut res.as_mut().ports, &ast.ports);
        NanoArena::init_from(&mut res.as_mut().modules, &ast.modules);
        NanoArena::init_from(&mut res.as_mut().functions, &ast.functions);
        NanoArena::init_from(&mut res.as_mut().disciplines, &ast.disciplines);
        NanoArena::init_from(&mut res.as_mut().natures, &ast.natures);
        TinyArena::init_from(&mut res.as_mut().expressions, &ast.expressions);
        TinyArena::init(&mut res.as_mut().statements);
        Box::from_raw(res.as_ptr())
    }
}

impl_id_type!(BranchId in Hir::branches -> AttributeNode<'tag,BranchDeclaration<'tag>>);
impl_id_type!(NetId in Hir::nets -> AttributeNode<'tag,Net<'tag>>);
impl_id_type!(PortId in Hir::ports -> Port<'tag>);
impl_id_type!(VariableId in Hir::variables ->  AttributeNode<'tag,Variable<'tag>>);
impl_id_type!(ModuleId in Hir::modules -> AttributeNode<'tag,Module<'tag>>);
impl_id_type!(FunctionId in Hir::functions -> AttributeNode<'tag,Function<'tag>>);
impl_id_type!(DisciplineId in Hir::disciplines -> AttributeNode<'tag,Discipline<'tag>>);
impl_id_type!(ExpressionId in Hir::expressions -> Node<Expression<'tag>>);
impl_id_type!(AttributeId in Hir::attributes -> Attribute);
impl_id_type!(StatementId in Hir::statements -> Statement<'tag>);
impl_id_type!(NatureId in Hir::natures -> AttributeNode<'tag,Nature>);

#[derive(Clone, Copy)]
pub struct Discipline<'tag> {
    pub name: Ident,
    pub flow_nature: NatureId<'tag>,
    pub potential_nature: NatureId<'tag>,
}

#[derive(Clone)]
pub struct Module<'hir> {
    pub name: Ident,
    pub port_list: SafeRange<PortId<'hir>>,
    //    pub parameter_list: Option<Range<ParameterId<'ast>>>
    pub analog: Block<'hir>,
}
pub type Block<'hir> = SafeRange<StatementId<'hir>>;
#[derive(Clone)]
pub struct Condition<'hir> {
    pub main_condition: ExpressionId<'hir>,
    pub main_condition_statements: Block<'hir>,
    pub else_ifs: Vec<(ExpressionId<'hir>, SafeRange<StatementId<'hir>>)>,
    pub else_statement: SafeRange<StatementId<'hir>>,
}
#[derive(Clone, Copy)]
pub struct Port<'tag> {
    pub input: bool,
    pub output: bool,
    pub net: NetId<'tag>,
}

#[derive(Clone)]
pub struct BranchDeclaration<'hir> {
    pub name: Ident,
    pub branch: Branch<'hir>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Branch<'hir> {
    Port(PortId<'hir>),
    Nets(NetId<'hir>, NetId<'hir>),
}
#[derive(Clone, Copy)]
pub struct Net<'hir> {
    pub name: Ident,
    pub discipline: DisciplineId<'hir>,
    pub signed: bool,
    pub net_type: NetType,
}
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum DisciplineAccess {
    Potential,
    Flow,
}

#[derive(Clone)]
pub enum Statement<'hir> {
    Condition(AttributeNode<'hir, Condition<'hir>>),
    ConditionStart {
        condition_info_and_end: StatementId<'hir>,
    },
    Contribute(
        Attributes<'hir>,
        DisciplineAccess,
        BranchAccess<'hir>,
        ExpressionId<'hir>,
    ),
    //  TODO IndirectContribute(),
    Assignment(Attributes<'hir>, VariableId<'hir>, ExpressionId<'hir>),
    FunctionCall(Attributes<'hir>, FunctionId<'hir>, Vec<ExpressionId<'hir>>),
}

#[derive(Clone)]
pub enum Expression<'hir> {
    BinaryOperator(ExpressionId<'hir>, Node<BinaryOperator>, ExpressionId<'hir>),
    UnaryOperator(Node<UnaryOperator>, ExpressionId<'hir>),
    Primary(Primary<'hir>),
}
#[derive(Clone)]
pub enum Primary<'hir> {
    Integer(i64),
    UnsignedInteger(u32),
    Real(f64),
    VariableReference(VariableId<'hir>),
    NetReference(NetId<'hir>),
    PortReference(PortId<'hir>),
    //ParameterReference(ParameterId<'hir>),
    FunctionCall(FunctionId<'hir>, Rc<Vec<ExpressionId<'hir>>>),
    BranchAccess(DisciplineAccess, BranchAccess<'hir>),
}
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum BranchAccess<'hir> {
    Named(BranchId<'hir>),
    Unnamed(Branch<'hir>),
}
