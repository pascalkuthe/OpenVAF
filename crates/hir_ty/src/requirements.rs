use std::borrow::Cow;

use hir_def::{BranchId, DisciplineId, NatureId, NodeId, ParamId, Type, VarId};
use stdx::{impl_display, impl_idx_from};

pub use self::diagnostics::{SignatureMissmatch, TypeMissmatch, ArrayTypeMissmatch};
mod diagnostics;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyRequirement {
    Val(Type),
    AnyVal,
    ArrayAnyLength { ty: Type },
    Node,
    Port,
    PortFlow,
    Nature,
    Var(Type),
    Param(Type),
    AnyParam,
    Branch,
    Literal(Type),
}

impl_display! {
    match TyRequirement{
        TyRequirement::Val(ty) => "{} value",ty;
        TyRequirement::AnyVal => "value";
        TyRequirement::ArrayAnyLength{ty} => "array ({})", ty;
        TyRequirement::Node => "net reference";
        TyRequirement::Port => "port reference";
        TyRequirement:: Nature => "nature reference";
        TyRequirement::Var(ty) => "{} variable reference", ty;
        TyRequirement::Param(ty) => "{} parameter ref", ty;
        TyRequirement::AnyParam => "parameter reference";
        TyRequirement::Literal(ty) => "{} literal", ty;
        TyRequirement::PortFlow => "port-flow reference";
        TyRequirement::Branch => "branch reference";
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Val(Type),
    Node(NodeId),
    Port(NodeId),
    PortFlow(NodeId),
    Nature(NatureId),
    Discipline(DisciplineId),
    Var(Type, VarId),
    Param(Type, ParamId),
    Literal(Type),
    InfLiteral,
    Branch(BranchId),
    Scope,
    Function,
}

// TODO coerce tys for nicer errors? Would that even be an improvement?

impl_display! {
    match Ty{
        Ty::Val(ty) => "{} value",ty;
        Ty::Node(_) => "net reference";
        Ty::Port(_) => "port reference";
        Ty::PortFlow(_) => "port-flow reference";
        Ty::Nature(_) => "nature reference";
        Ty::Discipline(_) => "discipline reference";
        Ty::Var(ty,_) => "{} variable reference", ty;
        Ty::Param(ty,_) => "{} parameter ref", ty;
        Ty::Literal(ty) => "{} literal", ty;
        Ty::Branch(_) => "branch reference";
        Ty::Function => "function";
        Ty::Scope => "scope";
        Ty::InfLiteral => "numeric literal";
    }
}

impl Ty {
    pub fn satisfies_exact(&self, requirement: &TyRequirement) -> bool {
        self.satisfies::<false>(requirement)
    }

    pub fn satisfies_with_conversion(&self, requirement: &TyRequirement) -> bool {
        self.satisfies::<true>(requirement)
    }
    fn satisfies<const ALLOW_CONVERSION: bool>(&self, requirement: &TyRequirement) -> bool {
        match (self, requirement) {
            (
                Ty::Val(ty1) | Ty::Literal(ty1) | Ty::Var(ty1, _) | Ty::Param(ty1, _),
                TyRequirement::Val(ty2),
            )
            | (Ty::Literal(ty1), TyRequirement::Literal(ty2)) => {
                if ALLOW_CONVERSION {
                    ty1.is_convertable_to(ty2)
                } else {
                    ty1.is_semantically_equivalent(ty2)
                }
            }

            (
                Ty::Val(_) | Ty::Var(_, _) | Ty::Param(_, _) | Ty::InfLiteral,
                TyRequirement::AnyVal,
            )
            | (Ty::InfLiteral, TyRequirement::Val(Type::Real))
            | (
                Ty::Val(Type::EmptyArray),
                TyRequirement::ArrayAnyLength { .. }
                | TyRequirement::Val(Type::Array { len: 0, .. }),
            ) => true,

            // TODO merge these match arms when there are box/deref patterns (not any time soon)
            (
                Ty::Val(Type::Array { ty: ref ty1, .. }),
                TyRequirement::ArrayAnyLength { ty: ty2 },
            ) => {
                if ALLOW_CONVERSION {
                    ty1.is_convertable_to(ty2)
                } else {
                    ty1.is_semantically_equivalent(ty2)
                }
            }

            // No conversion for explicit references
            (Ty::Var(ty1, _), TyRequirement::Var(ty2))
            | (Ty::Param(ty1, _), TyRequirement::Param(ty2)) => ty1 == ty2,

            (Ty::Node(_) | Ty::Port(_), TyRequirement::Node)
            | (Ty::PortFlow(_), TyRequirement::PortFlow)
            | (Ty::Port(_), TyRequirement::Port)
            | (Ty::Nature(_), TyRequirement::Nature)
            | (Ty::Param(_, _), TyRequirement::AnyParam)
            | (Ty::Branch(_), TyRequirement::Branch) => true,

            _ => false,
        }
    }

    pub fn to_value(&self) -> Option<Type> {
        match self {
            Ty::Val(ty) | Ty::Var(ty, _) | Ty::Param(ty, _) | Ty::Literal(ty) => Some(ty.clone()),
            Ty::InfLiteral => Some(Type::Real),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SignatureData {
    pub args: Cow<'static, [TyRequirement]>,
    pub return_ty: Type,
}

impl SignatureData {
    pub const INT_BIN_OP: SignatureData = SignatureData {
        args: Cow::Borrowed(&[
            TyRequirement::Val(Type::Integer),
            TyRequirement::Val(Type::Integer),
        ]),
        return_ty: Type::Integer,
    };
    pub const REAL_BIN_OP: SignatureData = SignatureData {
        args: Cow::Borrowed(&[TyRequirement::Val(Type::Real), TyRequirement::Val(Type::Real)]),
        return_ty: Type::Real,
    };
    pub const BOOL_BIN_OP: SignatureData = SignatureData {
        args: Cow::Borrowed(&[TyRequirement::Val(Type::Bool), TyRequirement::Val(Type::Bool)]),
        return_ty: Type::Bool,
    };
    pub const REAL_COMPARISON: SignatureData = SignatureData {
        args: Cow::Borrowed(&[TyRequirement::Val(Type::Real), TyRequirement::Val(Type::Real)]),
        return_ty: Type::Bool,
    };
    pub const INT_COMPARISON: SignatureData = SignatureData {
        args: Cow::Borrowed(&[
            TyRequirement::Val(Type::Integer),
            TyRequirement::Val(Type::Integer),
        ]),
        return_ty: Type::Bool,
    };

    pub const STR_COMPARISON: SignatureData = SignatureData {
        args: Cow::Borrowed(&[TyRequirement::Val(Type::String), TyRequirement::Val(Type::String)]),
        return_ty: Type::Bool,
    };

    pub const BOOL_COMPARISON: SignatureData = SignatureData {
        args: Cow::Borrowed(&[TyRequirement::Val(Type::Bool), TyRequirement::Val(Type::Bool)]),
        return_ty: Type::Bool,
    };

    pub const NUMERIC_BIN_OP: &'static [SignatureData] =
        &[SignatureData::REAL_BIN_OP, SignatureData::INT_BIN_OP];
    pub const NUMERIC_COMPARISON: &'static [SignatureData] =
        &[SignatureData::REAL_COMPARISON, SignatureData::INT_COMPARISON];
    pub const ANY_COMPARISON: &'static [SignatureData] = &[
        SignatureData::REAL_COMPARISON,
        SignatureData::INT_COMPARISON,
        SignatureData::BOOL_COMPARISON,
        SignatureData::STR_COMPARISON,
    ];

    pub const SELECT: &'static [SignatureData] =
        &[SignatureData::BOOL_BIN_OP, SignatureData::INT_BIN_OP, SignatureData::REAL_BIN_OP];
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Signature(u8);

impl_idx_from!(Signature(u8));
