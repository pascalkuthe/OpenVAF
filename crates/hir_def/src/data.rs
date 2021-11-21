use std::sync::Arc;

use crate::{DisciplineId, Lookup, Name, db::HirDefDB, item_tree::Domain};

pub struct DisciplineData{
    pub name: Name,

    pub potential: Option<Name>,
    pub flow: Option<Name>,
    pub domain: Option<Domain>,
}

impl DisciplineData{
    pub fn discipline_data_query(db: &dyn HirDefDB, id: DisciplineId) -> Arc<DisciplineData>{
        let loc = id.lookup(db);
        let discipline =& loc.item_tree(db)[loc.id];

        Arc::new(DisciplineData{
            name: discipline.name.clone(),
            potential: discipline.potential.clone(),
            flow: discipline.flow.clone(),
            domain: discipline.domain,
        })
    }

    pub fn compatible(&self, other: &DisciplineData)->bool{
        if self.domain.is_none() || other.domain.is_none(){
            return true
        }

        if self.potential.is_none() && self.flow.is_some() || other.potential.is_none() && other.flow.is_none(){
            return self.domain == other.domain
        }

        self.potential == other.potential && self.flow == other.flow && self.domain == other.domain
    }
}


// pub struct NatureData{
//     pub name: Name,

//     pub potential: Option<Name>,
//     pub flo
// }
