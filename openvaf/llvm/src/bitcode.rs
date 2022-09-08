use libc::{c_char, size_t};

use crate::{Bool, Context, MemoryBuffer, Module, Value};

extern "C" {
    pub fn LLVMCreateMemoryBufferWithMemoryRange(
        input: *const c_char,
        input_len: size_t,
        name: *const c_char,
        requires_null_term: Bool,
    ) -> &'static MemoryBuffer;
    pub fn LLVMParseBitcodeInContext2<'a>(
        ctx: &'a Context,
        buf: &MemoryBuffer,
        dst_module: &mut Option<&'a Module>,
    ) -> Bool;

    pub fn LLVMGetNamedFunction<'a>(module: &'a Module, name: *const c_char) -> Option<&'a Value>;
}
