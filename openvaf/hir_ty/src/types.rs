use std::borrow::Cow;
use std::ops::Deref;

use hir_def::{
    BranchId, DisciplineId, FunctionId, LocalFunctionArgId, NatureAttrId, NatureId, NodeId,
    ParamId, Type, VarId,
};
use stdx::{impl_display, impl_idx_from, pretty};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyRequirement {
    Val(Type),
    Condition,
    AnyVal,
    ArrayAnyLength { ty: Type },
    Node,
    PortFlow,
    Nature,
    Var(Type),
    Param(Type),
    AnyParam,
    Branch,
    Literal(Type),
    Function,
}

impl TyRequirement {
    pub fn cast(&self, src: &Type) -> Option<Type> {
        match self {
            TyRequirement::Val(ty) if src != ty => Some(ty.to_owned()),
            TyRequirement::Condition if src != &Type::Bool => Some(Type::Bool),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum TyEquivalence {
    Conversion,
    Semantic,
    Exact,
}

impl TyEquivalence {
    fn compare_ty(self, ty1: &Type, ty2: &Type) -> bool {
        match self {
            TyEquivalence::Conversion => ty1.is_convertible_to(ty2),
            TyEquivalence::Semantic => ty1.is_semantically_equivalent(ty2),
            TyEquivalence::Exact => ty1 == ty2,
        }
    }
}

impl_display! {
    match TyRequirement{
        TyRequirement::Val(ty) => "{} value",ty;
        TyRequirement::Condition => "{} value", Type::Bool;
        TyRequirement::AnyVal => "value";
        TyRequirement::ArrayAnyLength{ty} => "array ({})", ty;
        TyRequirement::Node => "net reference";
        TyRequirement:: Nature => "nature reference";
        TyRequirement::Var(ty) => "{} variable reference", ty;
        TyRequirement::Param(ty) => "{} parameter ref", ty;
        TyRequirement::AnyParam => "parameter reference";
        TyRequirement::Literal(ty) => "{} literal", ty;
        TyRequirement::PortFlow => "port-flow reference";
        TyRequirement::Branch => "branch reference";
        TyRequirement::Function => "function";
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Val(Type),
    Node(NodeId),
    PortFlow(NodeId),
    Nature(NatureId),
    Discipline(DisciplineId),
    Var(Type, VarId),
    NatureAttr(Type, NatureAttrId),
    FunctionVar { ty: Type, fun: FunctionId, arg: Option<LocalFunctionArgId> },
    Param(Type, ParamId),
    Literal(Type),
    InfLiteral,
    Branch(BranchId),
    Scope,
    BuiltInFunction,
    UserFunction(FunctionId),
}

// TODO coerce tys for nicer errors? Would that even be an improvement?

impl_display! {
    match Ty{
        Ty::Val(ty) => "{} value",ty;
        Ty::Node(_) => "net reference";
        Ty::PortFlow(_) => "port-flow reference";
        Ty::Nature(_) => "nature reference";
        Ty::Discipline(_) => "discipline reference";
        Ty::Var(ty,_) => "{} variable reference", ty;
        Ty::NatureAttr(ty,_) => "{} nature attriubte reference", ty;
        Ty::FunctionVar{ty,..} => "{} variable reference", ty;
        Ty::Param(ty,_) => "{} parameter ref", ty;
        Ty::Literal(ty) => "{} literal", ty;
        Ty::Branch(_) => "branch reference";
        Ty::BuiltInFunction => "(builtin) function";
        Ty::UserFunction(_) => "(user-defined) function";
        Ty::Scope => "scope";
        Ty::InfLiteral => "numeric literal";
    }
}

impl Ty {
    pub fn unwrap_branch(&self) -> BranchId {
        if let Ty::Branch(id) = *self {
            id
        } else {
            unreachable!("expected branch found {:?}", self)
        }
    }

    pub fn unwrap_port_flow(&self) -> NodeId {
        if let Ty::PortFlow(id) = *self {
            id
        } else {
            unreachable!("expected node found {:?}", self)
        }
    }
    pub fn unwrap_node(&self) -> NodeId {
        if let Ty::Node(id) = *self {
            id
        } else {
            unreachable!("expected node found {:?}", self)
        }
    }

    pub fn unwrap_param(&self) -> ParamId {
        if let Ty::Param(_, id) = *self {
            id
        } else {
            unreachable!("expected node found {:?}", self)
        }
    }

    pub fn satisfies_semantic(&self, requirement: &TyRequirement) -> bool {
        self.satisfies(requirement, TyEquivalence::Semantic)
    }

    pub fn satisfies_with_conversion(&self, requirement: &TyRequirement) -> bool {
        self.satisfies(requirement, TyEquivalence::Conversion)
    }

    pub fn satisfies_exact(&self, requirement: &TyRequirement) -> bool {
        self.satisfies(requirement, TyEquivalence::Exact)
    }

    fn satisfies(&self, requirement: &TyRequirement, equiv: TyEquivalence) -> bool {
        match (self, requirement) {
            (
                Ty::Val(_)
                | Ty::Var(_, _)
                | Ty::NatureAttr(_, _)
                | Ty::Param(_, _)
                | Ty::InfLiteral
                | Ty::Literal(_)
                | Ty::FunctionVar { .. },
                TyRequirement::AnyVal,
            )
            | (Ty::InfLiteral, TyRequirement::Val(Type::Real))
            | (
                Ty::Val(Type::EmptyArray | Type::Array { len: 0, .. }),
                TyRequirement::ArrayAnyLength { .. }
                | TyRequirement::Val(Type::Array { len: 0, .. }),
            )
            | (Ty::Node(_), TyRequirement::Node)
            | (Ty::PortFlow(_), TyRequirement::PortFlow)
            | (Ty::Nature(_), TyRequirement::Nature)
            | (Ty::Param(_, _), TyRequirement::AnyParam)
            | (Ty::UserFunction(_), TyRequirement::Function)
            | (Ty::Branch(_), TyRequirement::Branch) => true,

            (
                Ty::Val(ty1)
                | Ty::Literal(ty1)
                | Ty::Var(ty1, _)
                | Ty::NatureAttr(ty1, _)
                | Ty::Param(ty1, _)
                | Ty::FunctionVar { ty: ty1, .. },
                TyRequirement::Val(ty2),
            )
            | (Ty::Literal(ty1), TyRequirement::Literal(ty2)) => equiv.compare_ty(ty1, ty2),

            (
                Ty::Val(ty)
                | Ty::Var(ty, _)
                | Ty::NatureAttr(ty, _)
                | Ty::Param(ty, _)
                | Ty::Literal(ty)
                | Ty::FunctionVar { ty, .. },
                TyRequirement::Condition,
            ) => ty.is_assignable_to(&Type::Bool),

            // TODO merge these match arms when there are box/deref patterns (not any time soon)
            (
                Ty::Val(Type::Array { ty: ref ty1, .. }),
                TyRequirement::ArrayAnyLength { ty: ty2 },
            ) => equiv.compare_ty(ty1, ty2),

            // No conversion for explicit references
            (
                Ty::Var(ty1, _) | Ty::NatureAttr(ty1, _) | Ty::FunctionVar { ty: ty1, .. },
                TyRequirement::Var(ty2),
            )
            | (Ty::Param(ty1, _), TyRequirement::Param(ty2)) => ty1 == ty2,

            _ => false,
        }
    }

    pub fn unwrap_func(&self) -> FunctionId {
        if let Ty::UserFunction(func) = *self {
            func
        } else {
            unreachable!("called unwrap_func on {self:?}")
        }
    }

    pub fn to_value(&self) -> Option<Type> {
        match self {
            Ty::Val(ty)
            | Ty::Var(ty, _)
            | Ty::NatureAttr(ty, _)
            | Ty::Param(ty, _)
            | Ty::Literal(ty)
            | Ty::FunctionVar { ty, .. } => Some(ty.clone()),
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

impl_display! {
    match SignatureData{
        SignatureData{ args, return_ty } => "({}) -> {}", pretty::List::new(args.deref()).with_final_separator(", "), return_ty;
    }
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

    pub const CONDITIONAL_BIN_OP: SignatureData = SignatureData {
        args: Cow::Borrowed(&[TyRequirement::Condition, TyRequirement::Condition]),
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
        &[SignatureData::INT_BIN_OP, SignatureData::REAL_BIN_OP];
    pub const NUMERIC_COMPARISON: &'static [SignatureData] =
        &[SignatureData::INT_COMPARISON, SignatureData::REAL_COMPARISON];
    pub const ANY_COMPARISON: &'static [SignatureData] = &[
        SignatureData::BOOL_COMPARISON,
        SignatureData::INT_COMPARISON,
        SignatureData::REAL_COMPARISON,
        SignatureData::STR_COMPARISON,
    ];

    pub const SELECT: &'static [SignatureData] =
        &[SignatureData::BOOL_BIN_OP, SignatureData::REAL_BIN_OP, SignatureData::INT_BIN_OP];
}

pub const BOOL_EQ: Signature = Signature(0);
pub const INT_EQ: Signature = Signature(1);
pub const REAL_EQ: Signature = Signature(2);
pub const STR_EQ: Signature = Signature(3);

pub const INT_OP: Signature = Signature(0);
pub const REAL_OP: Signature = Signature(1);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Signature(pub u32);

impl_idx_from!(Signature(u32));

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) struct BuiltinInfo {
    pub signatures: &'static [SignatureData],
    pub min_args: usize,
    pub max_args: Option<usize>,
    pub has_side_effects: bool,
}

pub fn default_return_ty(signatures: &[SignatureData]) -> Option<Ty> {
    let ty = &signatures.get(0)?.return_ty;
    signatures.iter().all(|sig| &sig.return_ty == ty).then(|| Ty::Val(ty.clone()))
}
