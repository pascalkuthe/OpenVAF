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
    let block0 = func.layout.make_block();
    let arg0 = func.dfg.make_param(0u32.into());
    let mut pos = FuncCursor::new(&mut func);
    pos.insert_block(block0);

    let c0 = pos.func.dfg.iconst(17);
    let v0 = pos.ins().iadd(arg0, c0);
    let v1 = pos.ins().imul(v0, c0);
    let imul = pos.prev_inst().unwrap();

    // Detach v0 and reuse it for a different instruction.
    pos.func.dfg.clear_results(imul);
    pos.ins().with_result(v1).iadd(arg0, c0);
    assert_eq!(pos.current_inst(), Some(imul));
    let iadd = pos.prev_inst().unwrap();
    assert!(imul != iadd);
    assert_eq!(pos.func.dfg.value_def(v1), ValueDef::Result(iadd, 0));
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
        .flat_map(|format| {
            format.opcodes.iter().map(|opcode| InstrBuilderFunc { format: format.name, opcode })
        });

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
            fn unary(self, opcode: Opcode, arg: Value) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::Unary { opcode, arg };
                self.build(data)
            }

            fn binary(self, opcode: Opcode, arg1: Value, arg2: Value) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::Binary { opcode, args: [arg1, arg2] };
                self.build(data)
            }


            fn binary1(self, opcode: Opcode, arg1: Value, arg2: Value) -> Value {
                let (inst, dfg) = self.binary(opcode, arg1, arg2);
                dfg.first_result(inst)
            }

            fn branch(
                self,
                cond: Value,
                then_dst: Block,
                else_dst: Block,
                loop_entry: bool
            ) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::Branch { cond, then_dst, else_dst, loop_entry };
                self.build(data)
            }

            fn br(
                self,
                cond: Value,
                then_dst: Block,
                else_dst: Block,
            ) -> Inst  {
                self.branch(cond, then_dst, else_dst, false).0
            }

            fn br_loop(
                self,
                cond: Value,
                then_dst: Block,
                else_dst: Block,
            ) -> Inst  {
                self.branch(cond, then_dst, else_dst, true).0
            }

            fn jump(self, destination: Block) -> Inst {
                let data = InstructionData::Jump { destination };
                self.build(data).0
            }

            fn build_call(self, func_ref: FuncRef, args: ValueList) -> (Inst, &'f mut DataFlowGraph) {
                let data = InstructionData::Call { args, func_ref };
                self.build(data)
            }

            fn call(mut self, func_ref: FuncRef,  args: &[Value]) -> Inst {
                let pool = &mut self.data_flow_graph_mut().insts.value_lists;
                let args = ValueList::from_slice(args, pool);
                self.build_call(func_ref, args).0
            }


            fn call1(mut self, func_ref: FuncRef, args: &[Value]) -> Value {
                let pool = &mut self.data_flow_graph_mut().insts.value_lists;
                let args = ValueList::from_slice(args, pool);
                let (inst, dfg) = self.build_call(func_ref, args);
                dfg.first_result(inst)
            }

            #[inline]
            fn phi(mut self, edges: &[(Block, Value)]) -> Value {
                let mut args = ValueList::new();
                let mut blocks = PhiMap::new();
                let dfg = self.data_flow_graph_mut();
                for (i, (block, val)) in edges.iter().enumerate() {
                    args.push(*val, &mut dfg.insts.value_lists);
                    blocks.insert(*block, i as u32, &mut dfg.phi_forest, &());
                }
                let (inst, dfg) = self.build(PhiNode { args, blocks }.into());
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
