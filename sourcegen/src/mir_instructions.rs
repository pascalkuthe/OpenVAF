use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use stdx::iter::zip;

use crate::{add_preamble, ensure_file_contents, project_root, reformat};

pub(crate) struct OpcodeInfo {
    pub name: &'static str,
    pub args: u8,
    pub returns: u8,
}

pub(crate) struct InstructionFormatData {
    pub name: &'static str,
    pub opcodes: &'static [OpcodeInfo],
}

macro_rules! opcodes_vals {
    ($format:ident ($args: literal) -> $returns: literal {
        $($opcodes: ident $(-> $op_returns: literal)?)*
    }) => {
        InstructionFormatData{
            name: stringify!($format),
            opcodes: &[
                $(
                    OpcodeInfo{
                        name: stringify!($opcodes),
                        args: $args,
                        returns: $returns $(-$returns + $op_returns)?,
                    }
                ),*
            ]
        }
    };

    (@varargs $format:ident{
        $($opcodes: ident ($args: literal) -> $returns: literal)*
    }) => {
        InstructionFormatData{
            name: stringify!($format),
            opcodes: &[
                $(
                    OpcodeInfo{
                        name: stringify!($opcodes),
                        args: $args,
                        returns: $returns,
                    }
                ),*
            ]
        }
    };
}

macro_rules! opcodes {
    ($($(@$opt: ident)? $format: ident $(($args: literal) -> $returns: literal)? {$($opcodes: tt)*})*) => {
        const INSTRUCTION_FORMAT_COUNT: usize = [$(stringify!($format)),*].len();
        pub(crate) const INSTRUCTION_FORMATS: [InstructionFormatData; INSTRUCTION_FORMAT_COUNT] = [$(opcodes_vals!($(@$opt)? $format $(($args) -> $returns)? {$($opcodes)*})),*];
    };
}

opcodes! {
    Unary(1) -> 1 {
        Inot
        Bnot
        Fneg
        Ineg

        FIcast
        IFcast

        BIcast
        IBcast

        FBcast
        BFcast
        OptBarrier

        Sqrt
        Exp
        Ln
        Log
        Clog2
        Floor
        Ceil
        Sin
        Cos
        Tan
        Asin
        Acos
        Atan
        Sinh
        Cosh
        Tanh
        Asinh
        Acosh
        Atanh
    }

    Binary(2) -> 1{
        Iadd
        Isub
        Imul
        Idiv
        Irem

        Ishl
        Ishr
        Ixor
        Iand
        Ior

        Fadd
        Fsub
        Fmul
        Fdiv
        Frem

        Ilt
        Igt
        Ige
        Ile

        Flt
        Fgt
        Fge
        Fle

        Ieq
        Feq
        Seq
        Beq

        Ine
        Fne
        Sne
        Bne

        Hypot
        Atan2
        Pow
    }

    Branch(1) -> 0 {
        Br
    }

    Jump(0) -> 0 {
        Jmp
    }

    @varargs Call {
        Call(0) -> 0
    }

    @varargs PhiNode {
        Phi(0) -> 1
    }
}

#[test]
fn gen_opcodes() {
    let opcodes = INSTRUCTION_FORMATS.iter().flat_map(|format| format.opcodes.iter());

    let formats: Vec<_> =
        INSTRUCTION_FORMATS.iter().map(|format| format_ident!("{}", format.name)).collect();

    let opcode_formats = zip(INSTRUCTION_FORMATS, &formats)
        .flat_map(|(format, ident)| format.opcodes.iter().map(move |_| ident));

    let opcode_idents = opcodes.clone().map(|code| format_ident!("{}", code.name));
    let opcode_print = opcodes.clone().map(|code| code.name.to_lowercase());
    let opcode_args = opcodes.clone().map(|code| code.args);
    let opcode_returns = opcodes.clone().map(|code| code.returns);

    let opcode_cnt = opcodes.count();
    let opcode_idencies = 1u8..=(opcode_cnt as u8);
    let opcode_print2 = opcode_print.clone();
    let opcode_idents2 = opcode_idents.clone();

    let opcodes = quote! {
        #[derive(Clone, PartialEq, Eq, Copy, Hash)]
        pub enum InstructionFormat {
            #(#formats),*
        }


        #[repr(u8)]
        #[derive(Clone, PartialEq, Eq, Copy, Hash)]
        pub enum Opcode {
            #(#opcode_idents = #opcode_idencies),*
        }

        pub(super) const OPCODE_CONSTRAINTS: [OpcodeConstraints; #opcode_cnt + 1] = [
                // sential value so that no additional subtraction is required
                OpcodeConstraints::new(0,0),
                #(
                    OpcodeConstraints::new(#opcode_args,#opcode_returns)
                ),*
        ];

        pub(super) const OPCODE_NAMES: [&str; #opcode_cnt + 1] = [
                // sential value so that no additional subtraction is required
                "",
                #(#opcode_print),*
        ];

        pub(super) const OPCODE_FORMAT: [InstructionFormat; #opcode_cnt + 1] = [
                // sential value so that no additional subtraction is required
                InstructionFormat::Binary,
                #(InstructionFormat::#opcode_formats),*
        ];

        impl std::str::FromStr for Opcode{
            type Err = &'static str;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s{
                    #(#opcode_print2 => Ok(Opcode::#opcode_idents2),)*
                    _ => Err("Unknown opcode")
                }
            }
        }


    };

    let header = "use super::*;";

    let file = project_root()
        .join("openvaf")
        .join("mir")
        .join("src")
        .join("instructions")
        .join("generated.rs");

    let file_string = format!("{}\n{}", header, opcodes);
    let file_string = add_preamble("gen_opcodes", reformat(file_string));
    ensure_file_contents(&file, &file_string);
}

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

#[test]
fn gen_instr_builder() {
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
        project_root().join("openvaf").join("mir").join("src").join("builder").join("generated.rs");

    let file_string = format!("{}\n{}", header, builder);
    let file_string = add_preamble("gen_instr_builder", reformat(file_string));
    ensure_file_contents(&file, &file_string);
}
