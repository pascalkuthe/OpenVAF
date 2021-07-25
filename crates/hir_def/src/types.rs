use std::iter::successors;

use data_structures::iter::Itertools;
use syntax::ast;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Err,
    Real,
    Integer,
    Bool,
    String,
    Array { ty: Box<Type>, len: u32 },
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Real | Type::Integer)
    }

    pub fn is_convertable_to(&self, dst: &Type) -> bool {
        self == dst
            || matches!((dst, self), (Type::Real, Type::Integer))
            // VerilogA has no boolean type. The Type is only an implementation detail used for
            // better code generation as such these two types are always convertable
            || matches!((dst, self), (Type::Integer, Type::Bool))
            || matches!((dst, self), (Type::Bool, Type::Integer)) 
            || self.dim() == dst.dim() && self.base_type().is_convertable_to(dst.base_type())
    }

    pub fn base_type(&self) -> &Type {
        let mut curr = self;
        while let Type::Array { ty, .. } = self {
            curr = ty
        }
        curr
    }

    pub fn dim(&self) -> Vec<u32> {
        if let Type::Array { ref ty, len } = *self {
            let mut dims = successors(Some((&*ty, len)), |(ty, _)| {
                if let Type::Array { ref ty, len } = ***ty {
                    Some((&*ty, len))
                } else {
                    None
                }
            })
            .map(|(_, len)| len)
            .collect_vec();
            dims.reverse();
            dims
        } else {
            vec![]
        }
    }

    pub fn is_assignable_to(&self, dst: &Type) -> bool {
        self.is_convertable_to(dst)
            || (self.base_type().is_numeric()
                && dst.base_type().is_numeric()
                && self.dim() == dst.dim())
    }
}

pub trait AsType {
    fn as_type(&self) -> Type;
}

impl AsType for ast::Type {
    fn as_type(&self) -> Type {
        if self.integer_token().is_some() {
            Type::Integer
        } else if self.real_token().is_some() {
            Type::Real
        } else {
            Type::Err
        }
    }
}

impl AsType for Option<ast::Type> {
    fn as_type(&self) -> Type {
        self.as_ref().map_or(Type::Err, AsType::as_type)
    }
}
