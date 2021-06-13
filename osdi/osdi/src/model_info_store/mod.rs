/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::id_type;
use crate::string::OsdiStr;
use crate::types::Type;
use index_vec::{Idx, IndexSlice, IndexVec};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::alloc;
use std::alloc::{alloc_zeroed, dealloc};
use std::ops::Deref;
use std::ptr::NonNull;

id_type!(NodeId(u16));

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Node {
    pub name: OsdiStr,
    pub ground: bool,

    // A list of all Jacobian Elements `jacobian[self,x] != 0`
    // The Jacobian in this context refers to the (sparse) jacobian of the Kirchoff Laws used in circuit simulation
    //
    // An entry `x` here can be interpreted as the potential of Node `x`
    // can directly influence the value of a branch that is connected to this
    // pub jacobian_entries: Vec<JacobianId>,
    /// The terminal that this port belogs to
    pub port: Option<PortId>,
}

id_type!(PortId(u16));

#[derive(Clone, Debug, Serialize, Deserialize, Copy)]
pub struct Voltage {
    pub hi: NodeId,
    pub lo: NodeId,
    //pub lim: Option<LimitId>,
}

id_type!(JacobianId(u16));

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct JacobianEntry {
    pub kirchoff_node: NodeId,
    pub derived_by: NodeId,
}

#[cfg(feature = "simulator")]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SerializableIdxSlice<I: Idx, T: 'static>(pub &'static IndexSlice<I, [T]>);

#[cfg(not(feature = "simulator"))]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SerializableIdxSlice<I: Idx, T>(pub Box<IndexSlice<I, [T]>>);

impl<I: Idx, T> Deref for SerializableIdxSlice<I, T> {
    type Target = IndexSlice<I, [T]>;

    fn deref(&self) -> &Self::Target {
        &self.0.deref()
    }
}

impl<I: Idx, T: Serialize> Serialize for SerializableIdxSlice<I, T> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        self.0.as_raw_slice().serialize(serializer)
    }
}

#[cfg(feature = "simulator")]
impl<'de, I: Idx, T: Deserialize<'de>> Deserialize<'de> for SerializableIdxSlice<I, T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = &*Box::leak(Box::<[T]>::deserialize(deserializer)?);
        Ok(Self(IndexSlice::from_slice(raw)))
    }
}

impl<I: Idx, T: 'static> From<IndexVec<I, T>> for SerializableIdxSlice<I, T> {
    fn from(src: IndexVec<I, T>) -> Self {
        Self(Box::leak(src.into_boxed_slice()))
    }
}

id_type!(ParameterId(u16));

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UserInfo {
    pub name: OsdiStr,
    pub description: OsdiStr,
    pub unit: OsdiStr,
    pub obsolete: bool,
    pub ty: Type,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Parameter {
    pub info: UserInfo,
    pub model_data_offset: usize,
    pub id: ParameterId,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct Layout {
    size: usize,
    align: usize,
}

impl Layout {
    pub fn alloc_layout(&self) -> alloc::Layout {
        std::alloc::Layout::from_size_align(self.size, self.align).unwrap()
    }
    pub fn alloc(&self) -> OpaqueData {
        OpaqueData::new(self.alloc_layout())
    }
}

pub struct OpaqueData {
    data: NonNull<u8>,
    layout: alloc::Layout,
}

impl OpaqueData {
    // # Safety
    // This is unsafe because the caller could dealloc the data
    // It is quite clear that the Data is owned by this struct
    pub unsafe fn access(&self) -> NonNull<u8> {
        self.data
    }

    pub unsafe fn frow_raw(data: NonNull<u8>, layout: alloc::Layout) -> Self {
        Self { data, layout }
    }

    pub fn new(layout: alloc::Layout) -> Self {
        let data = if layout.size() == 0 {
            NonNull::dangling() //  zero sized types do not allocate
        } else {
            let raw = unsafe { alloc_zeroed(layout) };
            NonNull::new(raw).unwrap()
        };

        Self { data, layout }
    }
}

impl Drop for OpaqueData {
    fn drop(&mut self) {
        unsafe { dealloc(self.data.as_ptr(), self.layout) }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ModelInfoStore {
    pub name: OsdiStr,
    pub description: OsdiStr,
    pub jacobian: SerializableIdxSlice<JacobianId, JacobianEntry>,
    pub ports: SerializableIdxSlice<PortId, NodeId>,
    pub parameters: SerializableIdxSlice<ParameterId, Parameter>,
    pub state_vec: Layout,
    pub model_data: Layout,
    pub param_given_offset: usize,
    pub instance_data: Layout,
}
