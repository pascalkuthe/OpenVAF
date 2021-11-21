use hir_def::Type;
use text_size::TextRange;

use super::TypeRequirement;

pub struct Found {
    pos: TextRange,
    ty: Type,
}

pub enum TypeMissmatch {
    UnexpectedType {
        expected: TypeRequirement,
        found_ty: Type,
        found_decl: TextRange,
        range: TextRange,
    },

}
