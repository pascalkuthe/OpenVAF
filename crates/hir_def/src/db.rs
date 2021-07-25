use std::sync::Arc;

use basedb::{BaseDB, FileId};

use crate::{
    item_tree::{ItemTree, ItemTreeId, Nature},
    nameres::DefMap,
    AstIdMap, BlockId, BlockLoc, BranchId, BranchLoc, DisciplineId, DisciplineLoc, FunctionId,
    FunctionLoc, ModuleId, ModuleLoc, Name, NatureId, NatureLoc, NetId, NetLoc, ParamId, ParamLoc,
    PortId, PortLoc, VarId, VarLoc,
};

#[salsa::query_group(InternDatabase)]
pub trait InternDB: BaseDB {
    #[salsa::interned]
    fn intern_module(&self, loc: ModuleLoc) -> ModuleId;
    #[salsa::interned]
    fn intern_param(&self, loc: ParamLoc) -> ParamId;
    #[salsa::interned]
    fn intern_var(&self, loc: VarLoc) -> VarId;
    #[salsa::interned]
    fn intern_nature(&self, loc: NatureLoc) -> NatureId;
    #[salsa::interned]
    fn intern_discipline(&self, loc: DisciplineLoc) -> DisciplineId;
    #[salsa::interned]
    fn intern_block(&self, loc: BlockLoc) -> BlockId;
    #[salsa::interned]
    fn intern_port(&self, loc: PortLoc) -> PortId;
    #[salsa::interned]
    fn intern_net(&self, loc: NetLoc) -> NetId;
    #[salsa::interned]
    fn intern_branch(&self, loc: BranchLoc) -> BranchId;
    #[salsa::interned]
    fn intern_function(&self, loc: FunctionLoc) -> FunctionId;
}

#[salsa::query_group(HirDefDatabase)]
pub trait HirDefDB: InternDB {
    fn ast_id_map(&self, root_file: FileId) -> Arc<AstIdMap>;
    #[salsa::invoke(ItemTree::file_item_tree_query)]
    fn item_tree(&self, root_file: FileId) -> Arc<ItemTree>;
    // #[salsa::invoke(DefMap::def_map_query)]
    // fn def_map(&self, root_file: FileId) -> Arc<DefMap>;

    // #[doc(hidden)]
    // #[salsa::invoke(NatureDefMap::nature_def_map_query)]
    // fn nature_def_map(&self, root_file: FileId) -> Arc<NatureDefMap>;
    // #[salsa::cycle(NatureDefMap::nature_attr_recover)]
    // #[salsa::invoke(NatureDefMap::nature_attr_query)]
    // fn nature_attrs(&self, root_file: FileId, nature: ItemTreeId<Nature>) -> Arc<NatureAttr>;
}

fn ast_id_map(db: &dyn HirDefDB, root_file: FileId) -> Arc<AstIdMap> {
    let cst = db.parse(root_file).syntax_node();
    let ast_id_map = AstIdMap::from_source(&cst);
    Arc::new(ast_id_map)
}
