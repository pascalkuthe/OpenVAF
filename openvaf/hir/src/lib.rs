//! HIR (previously known as descriptors) provides a high-level object oriented
//! access to Verilog-A code.
//!
//! HIR is the public API of all the compiler logic above syntax trees.
//! It is written in "OO" style. Each type is self contained (as in, it knows it's
//! parents and full context). It should be "clean code".
//!
//! `hir_*` crates are the implementation of the compiler logic.
//! They are written in "ECS" style, with relatively little abstractions.
//! Many types are not self-contained, and explicitly use local indexes, arenas, etc.

use std::sync::Arc;

use basedb::diagnostics::sink::Buffer;
use basedb::diagnostics::ConsoleSink;
use basedb::BaseDB;
use basedb::FileId;
use hir_def::db::HirDefDB;
use hir_def::nameres::{DefMap, LocalScopeId, ScopeDefItem};
use hir_def::DefWithBodyId;
use hir_def::DisciplineId;
use hir_def::LocalFunctionArgId;
use hir_def::NatureAttrId;
use hir_def::NatureId;
use hir_def::{
    AliasParamId, BlockId, BlockLoc, BranchId, FunctionId, Lookup, ModuleId, ModuleLoc, NodeId,
    ParamId, VarId,
};
use hir_ty::db::HirTyDB as HirDatabase;
use hir_ty::inference;
use salsa::InternKey;
use smol_str::SmolStr;
use syntax::ast;

pub use basedb::diagnostics::DiagnosticSink;
pub use hir_def::body::{ConstraintValue, ParamConstraint};
pub use hir_def::expr::CaseCond;
pub use hir_def::nameres::diagnostics::PathResolveError;
pub use hir_def::{BuiltIn, Case, Literal, ParamSysFun, Path, Type};
pub use hir_ty::builtin;
pub use rec_declarations::RecDeclarations;
pub use syntax::name::Name;

pub use crate::attributes::AstCache;
pub use crate::body::{
    AssignmentLhs, Body, BodyRef, ContributeKind, Expr, ExprId, Ref, ResolvedFun, Stmt, StmtId,
};
pub use crate::db::CompilationDB;

mod attributes;
mod body;
mod db;
pub mod diagnostics;
mod rec_declarations;

pub mod signatures {
    pub use hir_ty::builtin::{
        ABSDELAY_MAX, ABS_INT, ABS_REAL, DDX_POT, IDTMOD_IC, IDTMOD_IC_MODULUS,
        IDTMOD_IC_MODULUS_OFFSET, IDTMOD_IC_MODULUS_OFFSET_NATURE, IDTMOD_IC_MODULUS_OFFSET_TOL,
        IDTMOD_NO_IC, IDT_IC, IDT_IC_ASSERT, IDT_IC_ASSERT_NATURE, IDT_IC_ASSERT_TOL, IDT_NO_IC,
        LIMIT_BUILTIN_FUNCTION, MAX_INT, MAX_REAL, NATURE_ACCESS_BRANCH, NATURE_ACCESS_NODES,
        NATURE_ACCESS_NODE_GND, NATURE_ACCESS_PORT_FLOW, SIMPARAM_DEFAULT, SIMPARAM_NO_DEFAULT,
    };
    pub use hir_ty::types::{BOOL_EQ, INT_EQ, INT_OP, REAL_EQ, REAL_OP, STR_EQ};
}

/// A root file represents a compilation root file.
/// A phsical file may be part of multiple root filer trough
/// include statements. This is however not the case here
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CompilationUnit {
    root_file: FileId,
}

impl CompilationUnit {
    pub fn name(self, db: &CompilationDB) -> String {
        db.file_path(self.root_file).name().unwrap_or_else(|| String::from("~.va"))
    }

    pub fn diagnostics(self, db: &CompilationDB, sink: &mut impl DiagnosticSink) {
        diagnostics::collect(db, self.root_file, sink)
    }

    pub fn root_file(self) -> FileId {
        self.root_file
    }

    pub fn test_diagnostics(&self, db: &CompilationDB) -> String {
        let mut buf = Buffer::no_color();
        {
            let mut sink = ConsoleSink::buffer(db, &mut buf);
            sink.annonymize_paths();
            self.diagnostics(db, &mut sink);
        }
        let data = buf.into_inner();
        String::from_utf8(data).unwrap()
    }

    pub fn modules(self, db: &CompilationDB) -> Vec<Module> {
        let root_def_map = db.def_map(self.root_file);
        root_def_map[root_def_map.entry()]
            .declarations
            .iter()
            .filter_map(|(_, def)| {
                if let ScopeDefItem::ModuleId(id) = *def {
                    Some(Module { id })
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn ast(&self, db: &CompilationDB) -> attributes::AstCache {
        attributes::AstCache::new(db, self.root_file)
    }

    pub fn preprocess(&self, db: &CompilationDB) -> syntax::Preprocess {
        db.preprocess(self.root_file)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module {
    id: ModuleId,
}

stdx::impl_debug! {
    match Module{
        Module{ id } => "{id:?}";
    }
}

impl Module {
    pub fn name(self, db: &CompilationDB) -> String {
        db.module_data(self.id).name.to_string()
    }

    pub fn uuid(self, _db: &CompilationDB) -> u32 {
        self.id.as_intern_id().as_u32()
    }

    fn lookup(self, db: &CompilationDB) -> ModuleLoc {
        self.id.lookup(db)
    }

    /// list of all child scopes.
    pub fn child_scopes(self, db: &CompilationDB) -> Vec<Scope> {
        Scope::Module(self).children(db)
    }

    /// list of all declarations.
    pub fn declarations(self, db: &CompilationDB) -> Vec<Scope> {
        Scope::Module(self).children(db)
    }

    pub fn internal_nodes(self, db: &CompilationDB) -> Vec<Node> {
        db.module_data(self.id).internal_nodes.iter().map(|&id| Node { id }).collect()
    }

    pub fn ports(self, db: &CompilationDB) -> Vec<Node> {
        db.module_data(self.id).ports.iter().map(|&id| Node { id }).collect()
    }

    pub fn rec_declarations(self, db: &CompilationDB) -> RecDeclarations<'_> {
        RecDeclarations::new(Scope::Module(self), db)
    }

    pub fn analog_initial_block(&self, db: &CompilationDB) -> Body {
        Body::new(DefWithBodyId::ModuleId { initial: true, module: self.id }, db)
    }

    pub fn analog_block(&self, db: &CompilationDB) -> Body {
        Body::new(DefWithBodyId::ModuleId { initial: false, module: self.id }, db)
    }

    // todo: just temporary for VAE, this needs to be cleaned up
    pub fn lookup_var(
        &self,
        db: &CompilationDB,
        path: &Path,
    ) -> Result<Variable, PathResolveError> {
        let scope = self.id.lookup(db).scope;
        scope.resolve_item_path(db, path).map(|id| Variable { id })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block {
    id: BlockId,
}

stdx::impl_debug! {
    match Block{
        Block{ id } => "{id:?}";
    }
}

impl Block {
    pub fn name(self, db: &CompilationDB) -> String {
        self.lookup(db).name(db).to_string()
    }

    fn lookup(self, db: &CompilationDB) -> BlockLoc {
        self.id.lookup(db)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function {
    id: FunctionId,
}

stdx::impl_debug! {
    match Function{
        Function{ id } => "{id:?}";
    }
}

impl Function {
    pub fn name(self, db: &CompilationDB) -> String {
        db.function_data(self.id).name.to_string()
    }

    pub fn return_ty(self, db: &CompilationDB) -> Type {
        db.function_data(self.id).return_ty.clone()
    }

    pub fn arg(self, idx: usize, db: &CompilationDB) -> FunctionArg {
        debug_assert!(db.function_data(self.id).args.len() <= idx);
        FunctionArg { fun_id: self.id, arg_id: idx.into() }
    }
    pub fn args(self, db: &CompilationDB) -> impl Iterator<Item = FunctionArg> + Clone {
        let args = db.function_data(self.id).args.len();
        (0..args).map(move |i| FunctionArg { fun_id: self.id, arg_id: i.into() })
    }

    pub fn body(&self, db: &CompilationDB) -> Body {
        Body::new(self.id.into(), db)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionArg {
    fun_id: FunctionId,
    arg_id: LocalFunctionArgId,
}

impl FunctionArg {
    pub fn function(self) -> Function {
        Function { id: self.fun_id }
    }

    pub fn name(self, db: &CompilationDB) -> String {
        db.function_data(self.fun_id).args[self.arg_id].name.to_string()
    }

    pub fn ty(self, db: &CompilationDB) -> Type {
        db.function_data(self.fun_id).args[self.arg_id].ty.clone()
    }

    pub fn is_input(self, db: &CompilationDB) -> bool {
        db.function_data(self.fun_id).args[self.arg_id].is_input
    }

    pub fn is_output(self, db: &CompilationDB) -> bool {
        db.function_data(self.fun_id).args[self.arg_id].is_output
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum BranchWrite {
    Named(Branch),
    Unnamed { hi: Node, lo: Option<Node> },
}

impl BranchWrite {
    pub fn nodes(self, db: &CompilationDB) -> (Node, Option<Node>) {
        match self {
            BranchWrite::Named(branch) => match branch.kind(db) {
                BranchKind::Nodes(hi, lo) => (hi, Some(lo)),
                BranchKind::NodeGnd(hi) => (hi, None),
                BranchKind::PortFlow(_) => unreachable!(),
            },
            BranchWrite::Unnamed { hi, lo } => (hi, lo),
        }
    }
}

impl From<inference::BranchWrite> for BranchWrite {
    #[inline]
    fn from(inner: inference::BranchWrite) -> Self {
        match inner {
            inference::BranchWrite::Named(branch) => BranchWrite::Named(Branch { id: branch }),
            inference::BranchWrite::Unnamed { hi, lo } => {
                BranchWrite::Unnamed { hi: Node { id: hi }, lo: lo.map(|id| Node { id }) }
            }
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Scope {
    Module(Module),
    Block(Block),
    Function(Function),
}

impl Scope {
    fn def_map_and_scope(self, db: &CompilationDB) -> (LocalScopeId, Arc<DefMap>) {
        match self {
            Scope::Module(module) => {
                let id = module.lookup(db);
                (id.scope.local_scope, id.def_map(db))
            }
            Scope::Block(block) => {
                let def_map = db.block_def_map(block.id).expect("block is named");
                (def_map.entry(), def_map)
            }
            Scope::Function(func) => {
                let def_map = db.function_def_map(func.id);
                (def_map.entry(), def_map)
            }
        }
    }

    /// Iterates over all child modules.
    pub fn children(self, db: &CompilationDB) -> Vec<Scope> {
        let (scope, def_map) = self.def_map_and_scope(db);
        def_map[scope]
            .children
            .values()
            .map(|&scope| match def_map[scope].origin {
                hir_def::nameres::ScopeOrigin::Root => {
                    unreachable!("Root scope can not be a child scope")
                }
                hir_def::nameres::ScopeOrigin::Module(id) => Scope::Module(Module { id }),
                hir_def::nameres::ScopeOrigin::Block(id) => Scope::Block(Block { id }),
                hir_def::nameres::ScopeOrigin::Function(id) => Scope::Function(Function { id }),
            })
            .collect()
    }

    /// Iterates over all child modules.
    pub fn declarations(self, db: &CompilationDB) -> Vec<(Name, ScopeDef)> {
        let (scope, def_map) = self.def_map_and_scope(db);
        def_map[scope]
            .declarations
            .iter()
            .filter_map(|(name, &def)| {
                let res = match def {
                    ScopeDefItem::ModuleId(id) => ScopeDef::ModuleInstance(Module { id }),
                    ScopeDefItem::BlockId(id) => ScopeDef::Block(Block { id }),
                    ScopeDefItem::NodeId(id) => ScopeDef::Node(Node { id }),
                    ScopeDefItem::VarId(id) => ScopeDef::Variable(Variable { id }),
                    ScopeDefItem::ParamId(id) => ScopeDef::Parameter(Parameter { id }),
                    ScopeDefItem::AliasParamId(id) => {
                        ScopeDef::AliasParameter(AliasParameter { id })
                    }
                    ScopeDefItem::BranchId(id) => ScopeDef::Branch(Branch { id }),
                    ScopeDefItem::FunctionId(id) => ScopeDef::Function(Function { id }),
                    // implementation details
                    ScopeDefItem::BuiltIn(_)
                    | ScopeDefItem::NatureId(_)
                    | ScopeDefItem::NatureAccess(_)
                    | ScopeDefItem::DisciplineId(_)
                    | ScopeDefItem::ParamSysFun(_)
                    | ScopeDefItem::FunctionReturn(_)
                    | ScopeDefItem::FunctionArgId(_)
                    | ScopeDefItem::NatureAttrId(_) => return None,
                };
                Some((name.to_owned(), res))
            })
            .collect()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Node {
    id: NodeId,
}

stdx::impl_debug! {
    match Node{
        Node{ id } => "{id:?}";
    }
}

impl Node {
    #[inline]
    pub fn name(self, db: &CompilationDB) -> SmolStr {
        db.node_data(self.id).name.clone().into()
    }

    #[inline]
    pub fn discipline(self, db: &CompilationDB) -> Discipline {
        let id = db.node_discipline(self.id).unwrap();
        Discipline { id }
    }

    #[inline]
    pub fn is_input(self, db: &CompilationDB) -> bool {
        db.node_data(self.id).is_input
    }

    #[inline]
    pub fn is_output(self, db: &CompilationDB) -> bool {
        db.node_data(self.id).is_output
    }

    #[inline]
    pub fn is_port(self, db: &CompilationDB) -> bool {
        db.node_data(self.id).is_port()
    }

    #[inline]
    pub fn is_gnd(self, db: &CompilationDB) -> bool {
        db.node_data(self.id).is_gnd
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable {
    id: VarId,
}

stdx::impl_debug! {
    match Variable{
        Variable{ id } => "{id:?}";
    }
}

impl Variable {
    pub fn name(self, db: &CompilationDB) -> SmolStr {
        db.var_data(self.id).name.clone().into()
    }

    pub fn ty(self, db: &CompilationDB) -> Type {
        db.var_data(self.id).ty.clone()
    }

    pub fn init(self, db: &CompilationDB) -> Body {
        Body::new(self.id.into(), db)
    }

    pub fn get_attr(&self, db: &CompilationDB, ast: &AstCache, name: &str) -> Option<ast::Attr> {
        ast.resolve_attribute(name, self.id.lookup(db).ast_id(db).erased())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Parameter {
    id: ParamId,
}

impl Parameter {
    pub fn name(self, db: &CompilationDB) -> String {
        db.param_data(self.id).name.to_string()
    }

    pub fn default(self, db: &CompilationDB) -> ExprId {
        db.param_exprs(self.id).default
    }

    pub fn bounds(self, db: &CompilationDB) -> Arc<[ParamConstraint]> {
        db.param_exprs(self.id).bounds
    }

    pub fn init(self, db: &CompilationDB) -> Body {
        Body::new(self.id.into(), db)
    }

    pub fn ty(self, db: &CompilationDB) -> Type {
        db.param_ty(self.id)
    }

    pub fn get_attr(&self, db: &CompilationDB, ast: &AstCache, name: &str) -> Option<ast::Attr> {
        ast.resolve_attribute(name, self.id.lookup(db).ast_id(db).erased())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AliasParameter {
    id: AliasParamId,
}

stdx::impl_debug! {
    match AliasParameter{
        AliasParameter{ id } => "{id:?}";
    }
}

impl AliasParameter {
    pub fn name(self, db: &CompilationDB) -> String {
        db.alias_data(self.id).name.to_string()
    }
    pub fn resolve(self, db: &CompilationDB) -> Option<ResolvedAliasParameter> {
        db.resolve_alias(self.id).and_then(|alias| match alias {
            hir_ty::db::Alias::Cycel => None,
            hir_ty::db::Alias::Param(id) => {
                Some(ResolvedAliasParameter::Parameter(Parameter { id }))
            }
            hir_ty::db::Alias::ParamSysFun(param) => {
                Some(ResolvedAliasParameter::SystemParameter(param))
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum ResolvedAliasParameter {
    Parameter(Parameter),
    SystemParameter(ParamSysFun),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BranchKind {
    PortFlow(Node),
    NodeGnd(Node),
    Nodes(Node, Node),
}
impl BranchKind {
    pub fn unwrap_hi_node(self) -> Node {
        match self {
            BranchKind::NodeGnd(hi) | BranchKind::Nodes(hi, _) => hi,
            BranchKind::PortFlow(_) => unreachable!(),
        }
    }

    pub fn lo_node(self) -> Option<Node> {
        match self {
            BranchKind::Nodes(_, lo) => Some(lo),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Branch {
    id: BranchId,
}

stdx::impl_debug! {
    match Branch{
        Branch{ id } => "{id:?}";
    }
}

impl Branch {
    pub fn name(self, db: &CompilationDB) -> String {
        db.branch_data(self.id).name.to_string()
    }

    pub fn discipline(self, db: &CompilationDB) -> Discipline {
        let id = db.branch_info(self.id).unwrap().discipline;
        Discipline { id }
    }

    pub fn kind(self, db: &CompilationDB) -> BranchKind {
        match db.branch_info(self.id).unwrap().kind {
            hir_ty::lower::BranchKind::PortFlow(node) => BranchKind::PortFlow(Node { id: node }),
            hir_ty::lower::BranchKind::NodeGnd(node) => BranchKind::NodeGnd(Node { id: node }),
            hir_ty::lower::BranchKind::Nodes(hi, lo) => {
                BranchKind::Nodes(Node { id: hi }, Node { id: lo })
            }
        }
    }

    pub fn get_attr(&self, db: &CompilationDB, ast: &AstCache, name: &str) -> Option<ast::Attr> {
        ast.resolve_attribute(name, self.id.lookup(db).ast_id(db).erased())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Discipline {
    id: DisciplineId,
}

impl Discipline {
    pub fn name(self, db: &CompilationDB) -> String {
        db.discipline_data(self.id).name.to_string()
    }

    pub fn potential(&self, db: &CompilationDB) -> Option<Nature> {
        db.discipline_info(self.id).potential.map(|id| Nature { id })
    }

    pub fn flow(&self, db: &CompilationDB) -> Option<Nature> {
        db.discipline_info(self.id).flow.map(|id| Nature { id })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Nature {
    id: NatureId,
}

impl Nature {
    pub fn name(self, db: &CompilationDB) -> String {
        db.nature_data(self.id).name.to_string()
    }

    pub fn units(self, db: &CompilationDB) -> String {
        db.nature_data(self.id).units.clone().unwrap_or_default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NatureAttribute {
    id: NatureAttrId,
}

impl NatureAttribute {
    pub fn value(&self, db: &CompilationDB) -> Body {
        Body::new(self.id.into(), db)
    }
}

impl NatureAttribute {
    pub fn name(self, db: &CompilationDB) -> String {
        let loc = self.id.lookup(db);
        db.nature_data(loc.nature).attrs[loc.id].name.to_string()
    }
}

#[non_exhaustive]
#[derive(Debug)]
pub enum ScopeDef {
    Block(Block),
    ModuleInstance(Module),
    Node(Node),
    Variable(Variable),
    Parameter(Parameter),
    AliasParameter(AliasParameter),
    Branch(Branch),
    Function(Function),
}
