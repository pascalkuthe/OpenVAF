use std::ops::Index;

use ahash::AHashMap;
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiVec;

#[derive(Default)]
pub struct DeviceParams {
    names: AHashMap<&'static str, ParamId>,
    params: TiVec<ParamId, ParamInfo>,
}

impl DeviceParams {
    pub fn insert_param(
        &mut self,
        name: &'static str,
        ty: Type,
        is_instance_param: bool,
    ) -> ParamId {
        let param = self.params.push_and_get_key(ParamInfo { name, ty, is_instance_param });
        self.names.insert(name, param);
        param
    }

    pub fn insert_model_param(&mut self, name: &'static str, ty: Type) -> ParamId {
        self.insert_param(name, ty, false)
    }

    pub fn insert_instance_param(&mut self, name: &'static str, ty: Type) -> ParamId {
        self.insert_param(name, ty, true)
    }

    pub fn insert_alias(&mut self, name: &'static str, param: ParamId) {
        self.names.insert(name, param);
    }

    pub(crate) fn lookup_param_id(&self, name: &str) -> Option<ParamId> {
        self.names.get(name).copied()
    }

    pub(crate) fn lookup_param(&self, name: &str) -> Option<(ParamId, ParamInfo)> {
        let id = self.lookup_param_id(name)?;
        Some((id, self.params[id]))
    }
}

impl Index<ParamId> for DeviceParams {
    type Output = ParamInfo;

    fn index(&self, index: ParamId) -> &Self::Output {
        &self.params[index]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ParamId(pub u32);
impl_debug_display!(match ParamId{ ParamId(id) => "dev{:?}", id;});
impl_idx_from!(ParamId(u32));

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ParamInfo {
    pub name: &'static str,
    pub ty: Type,
    pub is_instance_param: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Real,
    Int,
    String,
}
