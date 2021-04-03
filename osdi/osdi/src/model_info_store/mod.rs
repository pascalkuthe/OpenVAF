//  * ******************************************************************************************
//  *  Copyright (c) 2020 Pascal Kuthe. This file is part of the OSDI project.
//  *  It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/osdi/blob/master/LICENSE.
//  *  No part of OpenVAF-Types, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************


use crate::id_type;
use crate::string::OsdiStr;
use crate::types::{Type};
use index_vec::{Idx, IndexSlice, IndexVec};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::ops::{Deref};
use std::alloc::Layout;

/// Specifies how to write the derivative of the Current between nodes `i` and `j` by the Voltage between nodes `l` and `m` into the Jacobi matrix.
///
/// ```math
///     \frac{dI_{i,j}}{dV_{l,m}}
/// ```
///
/// # Explanation
///
/// The value of a Jacobi entry is defined by the following equation
///
/// ``` math
///     J_{o,p} = \Sum_q  \frac{dI_{o,q}}{dV_{p}} = \Sum_q \Sum_r \frac{dI_{o,q}}{dV_{p,r}}
/// ```
///
/// Furthermore the symmetries shown below can be used to trivially show that a singular derivative affects **4** Jacobi entries:
///
/// ```math
///     J_{i,l}, J_{i,m}, J_{j,l}, J_{j,m}
/// ```
///
/// ```math
///     \frac{dI_{o,q}}{dV_{p,r}} = -\frac{dI_{o,q}}{dV_{r,p}}
/// ```
///
/// ```math
///     \frac{dI_{o,q}}{dV_{p,r}} = -\frac{dI_{q,o}}{dV_{p,r}}
/// ```
///
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Stamp {
    /// The Jacobi entry `J_{i,l}`
    /// Derivative should be added with **positive** sign
    pub high_by_high: JacobianId,
    /// The Jacobi entry `J_{i,m}`
    /// Derivative should be added with **negative** sign
    pub high_by_lo: JacobianId,

    /// The Jacobi entry `J_{j,l}`
    /// Derivative should be added with **positive** sign
    pub lo_by_lo: JacobianId,
    /// The Jacobi entry `J_{j,m}`
    /// Derivative should be added with **negative** sign
    pub lo_by_high: JacobianId,

    /// The voltage by which this branch was derived.
    /// Necessary for calulating the rhs in SPICE simulators
    ///
    /// ```math
    ///     V_{l,m}
    /// ```
    pub voltage: Voltage,
}

id_type!(NodeId(u16));

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Node {
    pub name: OsdiStr,
    pub ground: bool,

    /// A list of all Jacobian Elements `jacobian[self,x] != 0`
    /// The Jacobian in this context refers to the (sparse) jacobian of the Kirchoff Laws used in circuit simulation
    ///
    /// An entry `x` here can be interpreted as the potential of Node `x`
    /// can directly influence the value of a branch that is connected to this
    // pub jacobian_entries: Vec<JacobianId>,

    /// The terminal that this port belogs to
    pub port: Option<PortId>,
}

id_type!(PortId(u16));


#[derive(Clone, Debug, Serialize, Deserialize, Copy)]
pub struct Voltage {
    pub hi: NodeId,
    pub lo: NodeId,
    pub lim: Option<LimitId>,
}

id_type!(CurrentId(u16));

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Current {
    pub name: OsdiStr,

    /// A list of all Jacobian Elements `jacobian[hi,x]/jacobian[lo,x]`
    /// The Jacobian in this context refers to the (sparse) jacobian of the Kirchoff Laws used in circuit simulation
    ///
    /// This Vector is intended to be used to generate matrix stamps typcially found in SPICE based simulators
    /// Each elements corresponds to one derivative calculated by OpenVAF (see TODO)
    /// This value shall be added to first JacobianId and subtracted from the second
  //  pub stamps: Vec<Stamp>,

    // Voltage over this Branch
    pub voltage: Voltage,

    /// A list of all non linearaties that can affect this branch
    pub non_linearties: Box<[LimitId]>,

    pub pos: usize,
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
    pub mc_offset: usize,
    pub id: ParameterId,
}

id_type!(LimitId(u8));


#[derive(Serialize,Deserialize)]
struct LayoutHelper{
    size: usize,
    align: usize,
}

#[derive(Clone, Debug)]
pub struct SerializableLayout(pub Layout);

impl Serialize for SerializableLayout{
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> where
        S: Serializer {
        LayoutHelper{
            size: self.0.size(),
            align: self.0.align()
        }.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SerializableLayout{
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error> where
        D: Deserializer<'de> {
        let helper = LayoutHelper::deserialize(deserializer)?;
        Ok(Self(Layout::from_size_align(helper.size,helper.align).expect("Deserialized Illegal Layout!")))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ModelInfoStore {
    pub name: OsdiStr,
    pub description: OsdiStr,

    pub branches: SerializableIdxSlice<CurrentId, Current>,
    pub jacobian: SerializableIdxSlice<JacobianId, JacobianEntry>,

    pub nodes: SerializableIdxSlice<NodeId, Node>,
    pub ports: SerializableIdxSlice<PortId, NodeId>,


    pub parameters: SerializableIdxSlice<ParameterId, Parameter>,

    pub non_linear_voltages: SerializableIdxSlice<LimitId, Voltage>,
    pub ddt_count: usize,

    pub current_and_conductance_size: usize,

    pub instance_variable_layout: SerializableLayout,
    pub model_variable_layout: SerializableLayout,
    pub modelcard_layout: SerializableLayout,

}
