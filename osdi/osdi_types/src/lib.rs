//  * ******************************************************************************************
//  *  Copyright (c) 2020 Pascal Kuthe. This file is part of the OSDI project.
//  *  It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/osdi/blob/master/LICENSE.
//  *  No part of OpenVAF-Types, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************


use cfg_if::cfg_if;
use index_vec::define_index_type;
use parking_lot::{const_rwlock, RwLock};
use std::fmt::{Display, Debug};
use std::fmt::Formatter;
use std::mem::size_of;
use std::fmt;

cfg_if! {
    if #[cfg(feature = "serialize")]{
        use serde::{Deserialize, Deserializer, Serialize, Serializer};

        impl Serialize for Type {
            fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
                where
                    S: Serializer,
            {
                self.with_info(|info| info.serialize(serializer))
            }
        }

        impl<'de> Deserialize<'de> for Type {
            fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
                where
                    D: Deserializer<'de>,
            {
                let info = TypeInfo::deserialize(deserializer)?;
                Ok(Self::intern(info))
            }
        }
    }
}

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, PartialEq)]
pub enum SimpleConstVal<S> {
    Integer(i64),
    Real(f64),
    Bool(bool),
    String(S),
}

impl<S: Debug> Display for SimpleConstVal<S>{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self,f)
    }
}

impl<S: Debug> Debug for SimpleConstVal<S>{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self{
            SimpleConstVal::Integer(val) => write!(f, "{}", val),
            SimpleConstVal::Real(val) => write!(f, "{}", val),
            SimpleConstVal::Bool(val) => write!(f, "{}", val),
            SimpleConstVal::String(val) => write!(f, "{:?}", val),
        }
    }
}

impl<S> SimpleConstVal<S> {
    pub fn ty(&self) -> Type {
        match self {
            Self::Integer(_) => Type::INT,
            Self::Real(_) => Type::REAL,
            Self::String(_) => Type::STRING,
            Self::Bool(_) => Type::BOOL,
        }
    }
}

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Clone, PartialEq)]
pub enum ConstVal<S: 'static> {
    Scalar(SimpleConstVal<S>),
    Array(Box<[SimpleConstVal<S>]>, Type),
}

impl<S: 'static + Debug> Display for ConstVal<S>{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self{
            ConstVal::Scalar(val) => write!(f, "{}", val),
            ConstVal::Array(val, _) => write!(f, "{:?}",val)
        }
    }
}

impl<S: 'static + Debug> Debug for ConstVal<S>{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self{
            ConstVal::Scalar(val) => write!(f, "{:?}", val),
            ConstVal::Array(val, _) => write!(f, "{:?}",val)
        }
    }
}

impl<S: 'static> ConstVal<S> {
    pub fn ty(&self) -> Type {
        match self {
            Self::Scalar(val) => val.ty(),

            Self::Array(_, ty) => *ty,
        }
    }
}

impl<S: 'static + Clone> ConstVal<S> {
    pub fn flatten(&self, dst: &mut Vec<SimpleConstVal<S>>) {
        match self {
            Self::Scalar(val) => dst.push(val.clone()),
            Self::Array(vals, _) => dst.extend_from_slice(&vals),
        }
    }
}

impl<S> From<f64> for ConstVal<S> {
    fn from(val: f64) -> Self {
        Self::Scalar(SimpleConstVal::Real(val))
    }
}

impl<S> From<i64> for ConstVal<S> {
    fn from(val: i64) -> Self {
        Self::Scalar(SimpleConstVal::Integer(val))
    }
}

impl<S> From<bool> for ConstVal<S> {
    fn from(val: bool) -> Self {
        Self::Scalar(SimpleConstVal::Bool(val))
    }
}

static TYPE_INTERNER: RwLock<Vec<TypeInfo>> = const_rwlock(Vec::new());

define_index_type! {

    /// A small interned id representing a unique valid type
    pub struct Type = u16;

    DISPLAY_FORMAT = "{}";

    DEBUG_FORMAT = "<Type {}>";

    IMPL_RAW_CONVERSIONS = true;

    // Checks are done when literals are added
    DISABLE_MAX_INDEX_CHECK = ! cfg!(debug_assertions);

}

impl Type {
    pub const REAL: Self = Self::from_raw_unchecked(0);
    pub const INT: Self = Self::from_raw_unchecked(1);
    pub const BOOL: Self = Self::from_raw_unchecked(2);
    pub const STRING: Self = Self::from_raw_unchecked(3);

    pub fn intern(info: TypeInfo) -> Self {
        match info {
            x if x == TypeInfo::REAL => Self::REAL,
            x if x == TypeInfo::INT => Self::INT,
            x if x == TypeInfo::STRING => Self::STRING,
            x if x == TypeInfo::BOOL => Self::BOOL,

            info => {
                let mut interner = TYPE_INTERNER.write();
                let pos = interner.iter().position(|x| x == &info).unwrap_or_else(|| {
                    interner.push(info);
                    interner.len()
                });
                Self::new(pos)
            }
        }
    }

    pub fn with_info<T>(self, f: impl FnOnce(&TypeInfo) -> T) -> T {
        match self {
            Self::REAL => f(&TypeInfo::REAL),
            Self::INT => f(&TypeInfo::INT),
            Self::STRING => f(&TypeInfo::STRING),
            Self::BOOL => f(&TypeInfo::BOOL),
            _ => f(&TYPE_INTERNER.read()[self.index() - 3]),
        }
    }
}
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SimpleType {
    Integer,
    Bool,
    Real,
    String,
}

impl Display for SimpleType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer => f.write_str("int"),
            Self::Real => f.write_str("real"),
            Self::String => f.write_str("string"),
            Self::Bool => f.write_str("bool"),
        }
    }
}

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeInfo {
    pub dimensions: Vec<u32>,
    pub element: SimpleType,
}

impl Display for TypeInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.element, f)?;
        for dimension in &self.dimensions {
            f.write_str("[")?;
            Display::fmt(dimension, f)?;
            f.write_str("]")?;
        }
        Ok(())
    }
}

impl TypeInfo {
    pub const REAL: Self = Self {
        element: SimpleType::Real,
        dimensions: Vec::new(),
    };
    pub const INT: Self = Self {
        element: SimpleType::Integer,
        dimensions: Vec::new(),
    };
    pub const STRING: Self = Self {
        element: SimpleType::String,
        dimensions: Vec::new(),
    };
    pub const BOOL: Self = Self {
        element: SimpleType::Bool,
        dimensions: Vec::new(),
    };

    pub fn has_derviative(&self) -> bool {
        self.dimensions.is_empty() && matches!(self.element, SimpleType::Real | SimpleType::Integer)
    }

    pub fn dim(&self) -> u32 {
        self.dimensions.iter().product()
    }
    pub fn size(&self) -> u32 {
        let base_size = match self.element{
            SimpleType::Real => size_of::<f64>(),
            SimpleType::Integer => size_of::<i64>(),
            SimpleType::Bool => size_of::<u8>(),
            SimpleType::String => size_of::<usize>(),
        };
        // Casting is okay here since the base size is really small and will never exceed 32 bit
        self.dim() * (base_size as u32)
    }
}
