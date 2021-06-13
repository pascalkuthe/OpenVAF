/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::runtime::{ModelId, ModelLayoutId, ModelSizedType};
use crate::ModelInfoStore;
use crate::OsdiModel;
use bincode::ErrorKind;
use index_vec::IndexVec;
use linkme::distributed_slice;
use once_cell::sync::OnceCell;
use std::alloc::Layout;
use std::path::Path;

pub struct OsdiRegistry {
    models: IndexVec<ModelId, OsdiModel>,
}

impl OsdiRegistry {
    pub fn models(&self) -> impl Iterator<Item = &OsdiModel> {
        self.models.iter()
    }

    pub fn models_mut(&mut self) -> impl Iterator<Item = &mut OsdiModel> {
        self.models.iter_mut()
    }

    pub fn models_enumerated(&self) -> impl Iterator<Item = (ModelId, &OsdiModel)> {
        self.models.iter_enumerated()
    }

    pub fn models_enumerated_mut(&mut self) -> impl Iterator<Item = (ModelId, &mut OsdiModel)> {
        self.models.iter_mut_enumerated()
    }

    pub fn load(&mut self, path: impl AsRef<Path>) -> Result<ModelId, Box<ErrorKind>> {
        let mut model = OsdiModel::load(self.models.len_idx(), path)?;
        Ok(self.models.push(model))
    }

    pub fn model_count(&self) -> usize {
        self.models.len()
    }

    pub fn get_model(&self, id: ModelId) -> &OsdiModel {
        &self.models[id]
    }
}
