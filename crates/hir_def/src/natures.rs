
use std::sync::Arc;

use basedb::FileId;
use data_structures::HashMap;
use syntax::{ast::Expr, AstPtr};

use crate::{
    db::HirDefDB,
    item_tree::{ItemTreeId, Nature},
    name::{kw, AsName},
    Name,
};

pub type NatureAttr = HashMap<Name, AstPtr<Expr>>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct NatureDefMap {
    natures: HashMap<Name, ItemTreeId<Nature>>,
}

impl NatureDefMap {
    pub(crate) fn nature_def_map_query(db: &dyn HirDefDB, file: FileId) -> Arc<NatureDefMap> {
        let tree = db.item_tree(file);
        let natures = tree.data.natures.iter().map(|(id, nature)| (nature.name, id)).collect();
        Arc::new(NatureDefMap { natures })
    }

    pub(crate) fn nature_attr_query(
        db: &dyn HirDefDB,
        file: FileId,
        nature: ItemTreeId<Nature>,
    ) -> Arc<NatureAttr> {
        let def_map = db.nature_def_map(file);

        let ast_id = db.item_tree(file)[nature].ast_id;
        let ast = db.ast_id_map(file).get(ast_id).to_node(&db.parse(file).syntax_node());

        let attrs = ast
            .nature_attrs()
            .filter_map(|attr| Some((attr.name()?.as_name(), AstPtr::new(&attr.val()?))));

        if let Some(parent) = ast.parent().and_then(|name| def_map.natures.get(&name.as_name())) {
            let mut res = db.nature_attrs(file, *parent);
            Arc::make_mut(&mut res).extend(attrs);
            res
        } else {
            Arc::new(attrs.collect())
        }
    }

    /// Cycles are disallowed in Natures. To be able to still handle code with cycles simple return
    /// an empty attr set upon encountering a cycle (all attrs from this point onward had already
    /// been processed anyway)
    pub(crate) fn nature_attr_recover(
        db: &dyn HirDefDB,
        _cycle: &[String],
        _file: FileId,
        _nature: ItemTreeId<Nature>,
    ) -> Arc<NatureAttr> {
        Arc::new(HashMap::new())
    }

    pub(crate) fn nature_access(
        db: &dyn HirDefDB,
        file: FileId,
        nature: ItemTreeId<Nature>,
    ) -> Option<Name> {
        let ast = db.parse(file).syntax_node();
        let ast = db.nature_attrs(file, nature).get(&kw::access)?.to_node(&ast);
        if let Expr::Path(path) = ast {
            if path.parent_path().is_some() {
                return None;
            }
            Some(path.name_ref()?.as_name())
        } else {
            None
        }
    }
}

pub struct Nature{
    access: Name,
    branc

}
