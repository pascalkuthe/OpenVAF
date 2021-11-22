use std::borrow::Cow;

use hir_def::{ExprId, Type};
use typed_index_collections::TiSlice;

use super::{Signature, SignatureData, Ty, TyRequirement};

pub struct TypeMissmatch {
    expected: Cow<'static, [TyRequirement]>,
    found_ty: Ty,
    expr: ExprId,
}

pub struct ArrayTypeMissmatch {
    expected: Type,
    found_ty: Type,
    found_expr: ExprId,
    expected_expr: ExprId,
}

pub struct SignatureMissmatch {
    type_missmatches: Box<[TypeMissmatch]>,
    signatures: Cow<'static, TiSlice<Signature, SignatureData>>,
}
