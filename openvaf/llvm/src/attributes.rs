use libc::c_char;

/**
 * Create a string attribute.
 */
use crate::{Attribute, Context, Value};

extern "C" {
    fn LLVMCreateStringAttribute(
        ctx: &Context,
        key: *const c_char,
        key_len: u32,
        val: *const c_char,
        val_len: u32,
    ) -> &Attribute;

    pub fn LLVMPurgeAttrs(val: &Value);
}

pub fn create_attr_string_value<'ll>(
    llcx: &'ll Context,
    attr: &str,
    value: &str,
) -> &'ll Attribute {
    unsafe {
        LLVMCreateStringAttribute(
            llcx,
            attr.as_ptr().cast(),
            attr.len().try_into().unwrap(),
            value.as_ptr().cast(),
            value.len().try_into().unwrap(),
        )
    }
}
