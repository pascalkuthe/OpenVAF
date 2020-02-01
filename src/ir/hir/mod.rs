use std::ops::Range;
use std::ptr;
use std::ptr::NonNull;
use std::rc::Rc;

use bumpalo::Bump;

use crate::compact_arena::{Idx16, Idx8, NanoArena, TinyArena};
use crate::ir::ast::{
    Ast, Attribute, AttributeNode, Attributes, BinaryOperator, Discipline, Function, ModuleItem,
    NetType, Node, TopNode, UnaryOperator, Variable,
};
use crate::ir::{
    ast, AttributeId, BranchId, DisciplineId, ExpressionId, FunctionId, ModuleId, NetId, PortId,
    StatementId, VariableId,
};
use crate::symbol::Ident;
use crate::util::SafeRange;

pub(crate) mod lowering;

/// An Ast representing a parser Verilog-AMS project (root file);
/// It provides stable indicies for every Node because the entire is immutable once created;
/// It uses preallocated constant size arrays for performance so you should box this as this is a lot of data to put on the stack

//TODO make this into a general proc macro with lifetimes like compact arena
pub struct Hir<'tag> {
    //TODO unsized
    //TODO configure to use different arena sizes
    //Declarations
    //    parameters: NanoArena<'tag,Parameter>,
    //    nature: NanoArena<'tag,Nature>
    branches: NanoArena<'tag, AttributeNode<'tag, BranchDeclaration<'tag>>>,
    nets: TinyArena<'tag, AttributeNode<'tag, Net<'tag>>>,
    ports: NanoArena<'tag, AttributeNode<'tag, Port<'tag>>>,
    variables: TinyArena<'tag, AttributeNode<'tag, Variable<'tag>>>,
    modules: NanoArena<'tag, AttributeNode<'tag, Module<'tag>>>,
    functions: NanoArena<'tag, AttributeNode<'tag, Function<'tag>>>,
    disciplines: NanoArena<'tag, AttributeNode<'tag, Discipline>>,
    //Ast Items
    expressions: TinyArena<'tag, Node<Expression<'tag>>>,
    attributes: TinyArena<'tag, Attribute>,
    statements: TinyArena<'tag, Statement<'tag>>,
    pub top_nodes: Vec<TopNode<'tag>>, //would prefer this to be stored here instead of somewhere else on the heap but its probably fine for now
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
        //TODO natures (remove disciplines then
        NanoArena::copy_to(&mut res.as_mut().disciplines, &ast.disciplines);
        TinyArena::copy_to(&mut res.as_mut().variables, &ast.variables);
        TinyArena::copy_to(&mut res.as_mut().attributes, &ast.attributes);
        NanoArena::init_from(&mut res.as_mut().branches, &ast.branches);
        TinyArena::init_from(&mut res.as_mut().nets, &ast.nets);
        NanoArena::init_from(&mut res.as_mut().ports, &ast.ports);
        NanoArena::init_from(&mut res.as_mut().modules, &ast.modules);
        NanoArena::init_from(&mut res.as_mut().functions, &ast.functions);
        //        NanoArena::init(&mut res.as_mut().disciplines);
        TinyArena::init_from(&mut res.as_mut().expressions, &ast.expressions);
        TinyArena::init(&mut res.as_mut().statements);
        //TODO parameters
        //TODO natures
        ptr::write(&mut res.as_mut().top_nodes, Vec::new());
        std::mem::swap(&mut res.as_mut().top_nodes, &mut ast.top_nodes);
        Box::from_raw(res.as_ptr())
    }
}

impl_id_type_for_container!(BranchId(Idx8): AttributeNode<'tag,BranchDeclaration<'tag>>; in Hir::branches);
impl_id_type_for_container!(NetId(Idx16): AttributeNode<'tag,Net<'tag>>; in Hir::nets);
impl_id_type_for_container!(PortId(Idx8): AttributeNode<'tag,Port<'tag>>; in Hir::ports);
impl_id_type_for_container!(VariableId(Idx16): AttributeNode<'tag,Variable<'tag>>; in Hir::variables);
impl_id_type_for_container!(ModuleId(Idx8): AttributeNode<'tag,Module<'tag>>; in Hir::modules);
impl_id_type_for_container!(FunctionId(Idx8): AttributeNode<'tag,Function<'tag>>; in Hir::functions);
impl_id_type_for_container!(DisciplineId(Idx8): AttributeNode<'tag,Discipline>; in Hir::disciplines);
impl_id_type_for_container!(ExpressionId(Idx16): Node<Expression<'tag>>; in Hir::expressions);
impl_id_type_for_container!(AttributeId(Idx16): Attribute; in Hir::attributes);
impl_id_type_for_container!(StatementId(Idx16): Statement<'tag>; in Hir::statements);

#[derive(Clone)]
pub struct Module<'hir> {
    pub name: Ident,
    pub port_list: Option<SafeRange<PortId<'hir>>>,
    //    pub parameter_list: Option<Range<ParameterId<'ast>>>
    pub analog: Block<'hir>,
}
pub type Block<'hir> = SafeRange<StatementId<'hir>>;
#[derive(Clone)]
pub struct Condition<'hir> {
    pub main_condition: ExpressionId<'hir>,
    pub main_condition_statements: Block<'hir>,
    pub else_ifs: Vec<(ExpressionId<'hir>, SafeRange<StatementId<'hir>>)>,
    pub else_statement: Option<SafeRange<StatementId<'hir>>>,
}
#[derive(Clone, Copy)]
pub struct Port<'tag> {
    pub name: Ident,
    pub input: bool,
    pub output: bool,
    pub discipline: DisciplineId<'tag>, //TODO discipline
    pub signed: bool,
    pub net_type: NetType,
}

#[derive(Clone)]
pub struct BranchDeclaration<'hir> {
    pub name: Ident,
    pub branch: Branch<'hir>,
}

#[derive(Clone)]
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
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
        Node<ExpressionId<'hir>>,
    ),
    //  TODO IndirectContribute(),
    Assignment(Attributes<'hir>, VariableId<'hir>, Node<ExpressionId<'hir>>),
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
    FunctionCall(FunctionId<'hir>, Option<Range<ExpressionId<'hir>>>),
    BranchAccess(DisciplineAccess, BranchAccess<'hir>),
}
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum BranchAccess<'hir> {
    Named(BranchId<'hir>),
    Unnamed(NetId<'hir>, NetId<'hir>),
}
