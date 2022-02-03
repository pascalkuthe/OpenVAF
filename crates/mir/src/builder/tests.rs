use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use sourcegen::{add_preamble, ensure_file_contents, project_root, reformat};

use crate::builder::InstBuilder;
use crate::cursor::{Cursor, FuncCursor};
use crate::instructions::tests::{OpcodeInfo, INSTRUCTION_FORMATS};
use crate::{Function, ValueDef};

#[test]
fn reuse_results() {
    let mut func = Function::new();
    let block0 = func.dfg.make_block();
    let arg0 = func.dfg.append_block_param(block0);
    let mut pos = FuncCursor::new(&mut func);
    pos.insert_block(block0);

    let c0 = pos.ins().iconst(17);
    let v0 = pos.ins().iadd(arg0, c0);
    let iadd = pos.prev_inst().unwrap();

    // Detach v0 and reuse it for a different instruction.
    pos.func.dfg.clear_results(iadd);
    pos.ins().with_result(v0).iconst(3);
    assert_eq!(pos.current_inst(), Some(iadd));
    let iconst = pos.prev_inst().unwrap();
    assert!(iadd != iconst);
    assert_eq!(pos.func.dfg.value_def(v0), ValueDef::Result(iconst, 0));
}

#[test]
fn gen_instr_builder() {
    struct InstrBuilderFunc {
        format: &'static str,
        opcode: &'static OpcodeInfo,
    }

    impl ToTokens for InstrBuilderFunc {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let implementation = format_ident!("{}", self.format.to_lowercase());
            let func_ident = format_ident!("{}", self.opcode.name.to_lowercase());
            let opcode = format_ident!("{}", self.opcode.name);

            let args_def = (0..self.opcode.args).map(|arg| format_ident!("arg{}", arg));
            let args_ref = (0..self.opcode.args).map(|arg| format_ident!("arg{}", arg));
            let ret_val = if self.opcode.returns == 1 {
                quote! {
                    dfg.first_result(inst)
                }
            } else {
                let args_index = 0..(self.opcode.returns as usize);
                quote! {
                    let res = dfg.inst_results(inst);
                    (#(res[#args_index]),*)
                }
            };
            let value = format_ident!("Value");
            let returns = if self.opcode.returns == 1 {
                value.to_token_stream()
            } else {
                let returns = (0..self.opcode.returns).map(|_| value.clone());
                quote! { (#(#returns),*)}
            };

            quote! {
                fn #func_ident(self,  #(#args_def: Value),*) -> #returns{
                    let (inst, dfg) = self.#implementation(Opcode::#opcode, #(#args_ref),*);
                    #ret_val
                }
            }
            .to_tokens(tokens)
        }
    }

    let opcode_funcs = INSTRUCTION_FORMATS
        .iter()
        .filter(|format| format.name == "Binary" || format.name == "Unary")
        .map(|format| {
            format.opcodes.iter().map(|opcode| InstrBuilderFunc { format: format.name, opcode })
        })
        .flatten();

    let builder = quote! {
        /// Convenience methods for building instructions.
        ///
        /// The `InstBuilder` trait has one method per instruction opcode for
        /// conveniently constructing the instruction with minimum arguments.
        /// Polymorphic instructions infer their result types from the input
        /// arguments when possible. In some cases, an explicit `ctrl_typevar`
        /// argument is required.
        ///
        /// The opcode methods return the new instruction's result values, or
        /// the `Inst` itself for instructions that don't have any results.
        ///
        /// There is also a method per instruction format. These methods all
        /// return an `Inst`.
        pub trait InstBuilder<'f>: InstBuilderBase<'f> {
            fn unary(self, op: Opcode, arg: Value) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::Unary { op, arg };
                self.build(data)
            }

            fn binary(self, op: Opcode, arg1: Value, arg2: Value) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::Binary { op, args: [arg1, arg2] };
                self.build(data)
            }

            fn branch(
                self,
                op: Opcode,
                args: ValueList,
                destination: Block,
                loop_tag: LoopTag,
            ) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::Branch { op, args, destination, loop_tag };
                self.build(data)
            }

            fn jump(self, args: ValueList, destination: Block) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::Jump { args, destination };
                self.build(data)
            }

            fn build_call(self, args: ValueList, func_ref: FuncRef) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::Call { args, func_ref };
                self.build(data)
            }

            fn call1(self, args: ValueList, func_ref: FuncRef) -> Value {
                let (inst, dfg) = self.build_call(args, func_ref);
                dfg.first_result(inst)
            }

            fn build_fconst(self, imm: Ieee64) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::UnaryIeee64 { imm };
                self.build(data)
            }

            fn build_iconst(self, imm: i32) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::UnaryInt { imm };
                self.build(data)
            }

            fn build_bconst(self, imm: bool) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::UnaryBool { imm };
                self.build(data)
            }

            fn build_sconst(self, imm: Spur) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::UnaryStr { imm };
                self.build(data)
            }

            fn fconst(self, imm: Ieee64) -> Value {
                let (inst, dfg) = self.build_fconst(imm);
                dfg.first_result(inst)
            }

            fn iconst(self, imm: i32) -> Value {
                let (inst, dfg) = self.build_iconst(imm);
                dfg.first_result(inst)
            }

            fn bconst(self, imm: bool) -> Value {
                let (inst, dfg) = self.build_bconst(imm);
                dfg.first_result(inst)
            }

            fn sconst(self, imm: Spur) -> Value {
                let (inst, dfg) = self.build_sconst(imm);
                dfg.first_result(inst)
            }

            #(#opcode_funcs)*
        }
    };

    let header = "use super::*;";

    let file =
        project_root().join("crates").join("mir").join("src").join("builder").join("generated.rs");

    let file_string = format!("{}\n{}", header, builder);
    let file_string = add_preamble("gen_instr_builder", reformat(file_string));
    ensure_file_contents(&file, &file_string);
}
