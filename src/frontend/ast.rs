use indextree::NodeId;

use Node_Types::*;

#[derive(Clone, Debug)]
pub enum Node {
    TOP,
    ATTRIBUTE(String),
    MODULE(String),
    TASK,
    FUNCTION,
    WIRE(String, VARIABLE),
    BRANCH_DECL(String, bool),
    PARAMETER(String, bool, PARAMETER),
    ALIAS_PARAMETER,
    DEFPARAM,
    PARASET,
    ARGUMENT,
    RANGE,
    MULTIRANGE,
    CONSTANT_EXPRESSION,

    STRINGVALUE(String),
    PREFIX,
    ASSERT,

    //name and whether a system function was called
    FCALL(Vec<String>, bool),
    TO_BITS,
    TO_SIGNED,
    TO_UNSIGNED,

    TERNARY,
    MEMRD,
    MEMWR,
    MEMINIT,

    TCALL,
    ASSIGN,
    CELL,
    CELLARRAY,
    ALWAYS,
    ANALOG(bool),
    BLOCK(Option<String>),
    ASSIGN_EQ,
    ASSIGN_LE,
    CONTRIBUTE,
    CONTRIBUTE_INDIREKT,
    CASE,
    COND,
    CONDX,
    CONDZ,
    DEFAULT,
    FOR,
    WHILE,
    REPEAT,

    GENVAR,
    GENFOR,
    GENIF,
    GENCASE,
    GENBLOCK,
    TECALL,

    POSEDGE,
    NEGEDGE,
    EDGE,

    INTERFACE,
    INTERFACEPORT,
    INTERFACEPORTTYPE,
    MODPORT(String, PORT),
    MODPORTMEMBER(String),
    PACKAGE,
    WIRETYPE,

    //PRIMITVES
    INTEGER_VALUE(i64),
    REAL_VALUE(f64),
    REFERENCE(Vec<String>),
    BRANCH_REF(NATURE_ACCESS, bool),
    //OPERATORS
    CONCAT,
    REPLICATE,
    BIT_NOT,
    BIT_AND,
    BIT_OR,
    BIT_XOR,
    BIT_EQ,
    REDUCE_AND,
    REDUCE_OR,
    REDUCE_XOR,
    REDUCE_XNOR,
    REDUCE_BOOL,
    SHIFT_LEFT,
    SHIFT_RIGHT,
    SHIFT_SLEFT,
    SHIFT_SRIGHT,
    LT,
    LE,
    EQ,
    NE,
    EQX,
    NEX,
    GE,
    GT,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    POW,
    POS,
    NEG,
    LOGIC_AND,
    LOGIC_OR,
    LOGIC_NOT,
}

pub mod Node_Types {
    #[derive(Clone, Debug)]
    pub struct PORT {
        pub is_signed: bool,
        pub is_input: bool,
        pub is_output: bool,
        pub verilog_type: VERILOG_TYPE,
        pub discipline: String,
    }

    impl PORT {
        pub fn new() -> Self {
            PORT {
                is_signed: false,
                is_input: false,
                is_output: false,
                verilog_type: VERILOG_TYPE::UNDECLARED,
                discipline: String::from(""),
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct PARAMETER {
        pub verilog_type: VERILOG_TYPE,
        pub signed: bool,
    }

    #[derive(Clone, Debug)]
    pub struct VARIABLE {
        pub verilog_type: VERILOG_TYPE,
        pub is_signed: bool,
        pub discipline: String,
    }

    #[derive(Clone, Debug)]
    pub enum VERILOG_TYPE {
        UNDECLARED,
        REG,
        WREAL,
        SUPPLY0,
        SUPPLY1,
        TRI,
        TRIAND,
        TRIOR,
        TRI0,
        TRI1,
        WIRE,
        UWIRE,
        WAND,
        WOR,
        TIME,
        INTEGER,
        REAL,
        REALTIME,
    }

    #[derive(Clone, Debug)]
    pub enum NATURE_ACCESS {
        POTENTIAL,
        FLOW,
        UNRESOLVED(String),
    }
}





