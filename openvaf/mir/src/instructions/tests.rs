use quote::{format_ident, quote};
use sourcegen::{add_preamble, ensure_file_contents, project_root, reformat};
use stdx::iter::zip;
use stdx::SKIP_HOST_TESTS;

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
    if SKIP_HOST_TESTS {
        return;
    }

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
                    _ => Err("Unkown opcode")
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
