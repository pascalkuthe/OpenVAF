use std::sync::Arc;

use basedb::{BaseDB, FileId};

use crate::{AstIdMap, BlockId, BlockLoc, BranchId, BranchLoc, DefWithBehaviourId, DefWithBodyId, DefWithExprId, DisciplineAttrId, DisciplineAttrLoc, DisciplineId, DisciplineLoc, FunctionId, FunctionLoc, ModuleId, ModuleLoc, NatureAttrId, NatureAttrLoc, NatureId, NatureLoc, NodeId, NodeLoc, ParamId, ParamLoc, VarId, VarLoc, body::{AnalogBehaviour, BodySourceMap, ExprBody, ParamBody}, data::{DisciplineData, NatureData, NodeData, ParamData, VarData}, item_tree::ItemTree, nameres::DefMap};

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
    fn intern_branch(&self, loc: BranchLoc) -> BranchId;
    #[salsa::interned]
    fn intern_function(&self, loc: FunctionLoc) -> FunctionId;
    #[salsa::interned]
    fn intern_nature_attr(&self, loc: NatureAttrLoc) -> NatureAttrId;
    #[salsa::interned]
    fn intern_discipline_attr(&self, loc: DisciplineAttrLoc) -> DisciplineAttrId;
    #[salsa::interned]
    fn intern_node(&self, loc: NodeLoc) -> NodeId;
}

#[salsa::query_group(HirDefDatabase)]
pub trait HirDefDB: InternDB {
    fn ast_id_map(&self, root_file: FileId) -> Arc<AstIdMap>;

    #[salsa::invoke(ItemTree::file_item_tree_query)]
    fn item_tree(&self, root_file: FileId) -> Arc<ItemTree>;

    #[salsa::invoke(DefMap::def_map_query)]
    fn def_map(&self, root_file: FileId) -> Arc<DefMap>;

    #[salsa::invoke(DefMap::block_def_map_query)]
    fn block_def_map(&self, block: BlockId) -> Option<Arc<DefMap>>;

    #[salsa::invoke(AnalogBehaviour::body_with_sourcemap_query)]
    fn analog_behaviour_with_sourcemap(
        &self,
        root_file: FileId,
        id: DefWithBehaviourId,
    ) -> (Arc<AnalogBehaviour>, Arc<BodySourceMap>);

    fn analog_behaviour(&self, root_file: FileId, id: DefWithBehaviourId) -> Arc<AnalogBehaviour>;

    #[salsa::invoke(ParamBody::body_with_sourcemap_query)]
    fn param_body_with_sourcemap(
        &self,
        root_file: FileId,
        id: ParamId,
    ) -> (Arc<ParamBody>, Arc<BodySourceMap>);

    fn param_body(&self, root_file: FileId, id: ParamId) -> Arc<ParamBody>;

    #[salsa::invoke(ExprBody::body_with_sourcemap_query)]
    fn expr_body_with_sourcemap(
        &self,
        root_file: FileId,
        def: DefWithExprId,
    ) -> (Arc<ExprBody>, Arc<BodySourceMap>);

    fn expr_body(&self, root_file: FileId, def: DefWithExprId) -> Arc<ExprBody>;

    #[salsa::transparent]
    fn body_source_map(&self, root_file: FileId, def: DefWithBodyId) -> Arc<BodySourceMap>;

    #[salsa::invoke(DisciplineData::discipline_data_query)]
    fn disipline_data(&self, discipline: DisciplineId) -> Arc<DisciplineData>;

    #[salsa::invoke(NatureData::nature_data_query)]
    fn nature_data(&self, nature: NatureId) -> Arc<NatureData>;

    #[salsa::invoke(VarData::var_data_query)]
    fn var_data(&self, var: VarId) -> Arc<VarData>;

    #[salsa::invoke(ParamData::param_data_query)]
    fn param_data(&self, param: ParamId) -> Arc<ParamData>;

    #[salsa::invoke(NodeData::node_data_query)]
    fn node_data(&self, node: NodeId) -> Arc<NodeData>;
}

fn ast_id_map(db: &dyn HirDefDB, root_file: FileId) -> Arc<AstIdMap> {
    let cst = db.parse(root_file).syntax_node();
    let ast_id_map = AstIdMap::from_source(&cst);
    Arc::new(ast_id_map)
}

fn body_source_map(db: &dyn HirDefDB, root_file: FileId, def: DefWithBodyId) -> Arc<BodySourceMap> {
    match def {
        DefWithBodyId::ParamId(param) => db.param_body_with_sourcemap(root_file, param).1,
        DefWithBodyId::ModuleId(module) => {
            db.analog_behaviour_with_sourcemap(root_file, module.into()).1
        }
        DefWithBodyId::FunctionId(fun) => {
            db.analog_behaviour_with_sourcemap(root_file, fun.into()).1
        }
        DefWithBodyId::VarId(var) => db.expr_body_with_sourcemap(root_file, var.into()).1,
        DefWithBodyId::NatureAttrId(attr) => db.expr_body_with_sourcemap(root_file, attr.into()).1,
        DefWithBodyId::DisciplineAttrId(attr) => {
            db.expr_body_with_sourcemap(root_file, attr.into()).1
        }
    }
}

fn expr_body(db: &dyn HirDefDB, root_file: FileId, def: DefWithExprId) -> Arc<ExprBody> {
    db.expr_body_with_sourcemap(root_file, def).0
}

fn param_body(db: &dyn HirDefDB, root_file: FileId, param: ParamId) -> Arc<ParamBody> {
    db.param_body_with_sourcemap(root_file, param).0
}

fn analog_behaviour(
    db: &dyn HirDefDB,
    root_file: FileId,
    module: DefWithBehaviourId,
) -> Arc<AnalogBehaviour> {
    db.analog_behaviour_with_sourcemap(root_file, module).0
}
