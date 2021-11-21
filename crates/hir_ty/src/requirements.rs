use hir_def::Type;
use stdx::impl_display;
mod diagnostics;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeRequirement {
    Val(Type),
    ArrayAnyLength { ty: Type },
    Net,
    Port,
    Nature,
    Var(Type),
    Param(Type),
    AnyParam,
    Literal(Type),
}

use TypeRequirement::*;
impl_display! {
    match TypeRequirement{
        Val(ty) => "{} val",ty;
        ArrayAnyLength{ty} => "an array ({})", ty;
        Net => "a net reference";
        Port => "a port reference";
        Nature => "a nature reference";
        Var(ty) => "a {} variable reference", ty;
        Param(ty) => "a {} parameter ref", ty;
        AnyParam => "a parameter reference";
        Literal(ty) => "a {} literal", ty;
    }
}

// impl TypeRequirement{
//     pub fn check(&self,ty: Type) -> Result<Type,Missmatch>{
//         // TODO better errors
//         match self{
//             TypeRequirement::Real => matches!(ty, Type::Real|Type::Bool|Type::Integer),
//             TypeRequirement::Integer|TypeRequirement::Bool => matches!(ty, Type::Integer|Type::Bool),
//             TypeRequirement::String => ty == Type::String,
//             TypeRequirement::Array{ty: required,len: None} => matches!(ty, Type::Array{ty,..} if ty ==required),
//             TypeRequirement::Function(required) => matches!(ty, Type::Function(fun) if fun == required),

//         }
//     }

// }
