//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************



use hesr_alloc::{Allocator, SliceId, Immutable, StrId};

/// This is an Ast. Once created is it completely immutable
pub struct  Ast{
    data:Immutable,
    top_nodes:SliceId<TopNode>,
}
impl Ast{
    pub fn new(data:Allocator,top_nodes:SliceId<TopNode>)->Self{
        Self{
            data:Immutable::new(data),
            top_nodes
        }
    }
    pub fn top_nodes(&self)->&[TopNode]{
        self.data.get_slice(*&self.top_nodes)
    }
    pub fn get_data(&self)->&Immutable{
        &self.data
    }
}

pub enum TopNode{
    Module()
}

pub struct Module{
    name:StrId
}

