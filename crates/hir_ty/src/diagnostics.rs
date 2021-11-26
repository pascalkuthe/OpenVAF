use std::borrow::Cow;

use hir_def::{ExprId, FunctionId, Type};
use typed_index_collections::TiSlice;

use crate::types::{Signature, SignatureData, Ty, TyRequirement};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeMissmatch {
    pub expected: Cow<'static, [TyRequirement]>,
    pub found_ty: Ty,
    pub expr: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayTypeMissmatch {
    pub expected: Type,
    pub found_ty: Type,
    pub found_expr: ExprId,
    pub expected_expr: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureMissmatch {
    pub type_missmatches: Box<[TypeMissmatch]>,
    pub signatures: Cow<'static, TiSlice<Signature, SignatureData>>,
    pub src: Option<FunctionId>,
}
