//! Parser for .clif files.

use std::fmt::{self, Display, Formatter};
use std::ops::{Deref, DerefMut};

use crate::error::{Location, ParseResult};
use crate::lexer::{LexError, Lexer, LocatedError, LocatedToken, Token};
use crate::ParseError;

use bforest::Map;
use lasso::{Rodeo, Spur};
use mir::{
    Block, FuncRef, Function, FunctionSignature, Ieee64, InstructionData, InstructionFormat,
    Opcode, Param, PhiNode, SourceLoc, Value, ValueList, ValueListPool,
};

#[cfg(test)]
mod tests;

macro_rules! match_imm {
    ($signed:ty, $unsigned:ty, $parser:expr, $err_msg:expr) => {{
        if let Some(Token::Integer(text)) = $parser.token() {
            $parser.consume();
            let negative = text.starts_with('-');
            let positive = text.starts_with('+');
            let text = if negative || positive {
                // Strip sign prefix.
                &text[1..]
            } else {
                text
            };

            // Parse the text value; the lexer gives us raw text that looks like an integer.
            let value = if text.starts_with("0x") {
                // Skip underscores.
                let text = text.replace("_", "");
                // Parse it in hexadecimal form.
                <$unsigned>::from_str_radix(&text[2..], 16).map_err(|_| {
                    $parser.error("unable to parse value as a hexadecimal immediate")
                })?
            } else {
                // Parse it as a signed type to check for overflow and other issues.
                text.parse().map_err(|_| $parser.error("expected decimal immediate"))?
            };

            // Apply sign if necessary.
            let signed = if negative {
                let value = value.wrapping_neg() as $signed;
                if value > 0 {
                    return Err($parser.error("negative number too small"));
                }
                value
            } else {
                value as $signed
            };

            Ok(signed)
        } else {
            err!($parser.loc, $err_msg)
        }
    }};
}

/// A variable list of `Value` operands used for function call arguments and passing arguments to
/// basic blocks.
#[derive(Clone, Debug)]
pub struct VariableArgs(Vec<Value>);

impl VariableArgs {
    /// Create an empty argument list.
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Add an argument to the end.
    pub fn push(&mut self, v: Value) {
        self.0.push(v)
    }

    /// Convert this to a value list in `pool` with `fixed` prepended.
    pub fn into_value_list(self, fixed: &[Value], pool: &mut ValueListPool) -> ValueList {
        let mut vlist = ValueList::default();
        vlist.extend(fixed.iter().cloned(), pool);
        vlist.extend(self.0, pool);
        vlist
    }
}

// Coerce `VariableArgs` into a `&[Value]` slice.
impl Deref for VariableArgs {
    type Target = [Value];

    fn deref(&self) -> &[Value] {
        &self.0
    }
}

impl DerefMut for VariableArgs {
    fn deref_mut(&mut self) -> &mut [Value] {
        &mut self.0
    }
}

impl Display for VariableArgs {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        for (i, val) in self.0.iter().enumerate() {
            if i == 0 {
                write!(fmt, "{}", val)?;
            } else {
                write!(fmt, ", {}", val)?;
            }
        }
        Ok(())
    }
}

impl Default for VariableArgs {
    fn default() -> Self {
        Self::new()
    }
}

/// After some quick benchmarks a program should never have more than 100,000 blocks.
const MAX_BLOCKS_IN_A_FUNCTION: u32 = 100_000;

/// Parse the entire `text` into a list of functions.
///
/// Any test commands or target declarations are ignored.
pub fn parse_functions(text: &str) -> ParseResult<(Vec<Function>, Rodeo)> {
    let mut parser = Parser::new(text);
    Ok((parser.parse_function_list()?, parser.interner))
}

/// Parse the entire `text` into a function
pub fn parse_function(text: &str) -> ParseResult<(Function, Rodeo)> {
    let mut parser = Parser::new(text);
    Ok((parser.parse_function()?, parser.interner))
}

pub struct Parser<'a> {
    lex: Lexer<'a>,

    lex_error: Option<LexError>,

    /// Current lookahead token.
    lookahead: Option<Token<'a>>,

    /// Location of lookahead.
    loc: Location,

    interner: Rodeo,
}

/// Context for resolving references when parsing a single function.
struct Context {
    function: Function,
}

impl Context {
    fn new(f: Function) -> Self {
        Self { function: f }
    }

    // Allocate a new signature.
    fn add_sig(&mut self, sig: FuncRef, data: FunctionSignature) -> ParseResult<()> {
        while usize::from(self.function.dfg.signatures.next_key()) <= usize::from(sig) {
            self.function.import_function(FunctionSignature::default());
        }
        self.function.dfg.signatures[sig] = data;
        Ok(())
    }

    // Allocate a new block.
    fn add_block(&mut self, block: Block) -> ParseResult<Block> {
        while self.function.layout.num_blocks() <= usize::from(block) {
            self.function.layout.make_block();
        }
        self.function.layout.append_block(block);
        Ok(block)
    }
}

impl<'a> Parser<'a> {
    /// Create a new `Parser` which reads `text`. The referenced text must outlive the parser.
    pub fn new(text: &'a str) -> Self {
        Self {
            lex: Lexer::new(text),
            lex_error: None,
            lookahead: None,
            loc: Location { line_number: 0 },
            interner: Rodeo::default(),
        }
    }

    // Consume the current lookahead token and return it.
    fn consume(&mut self) -> Token<'a> {
        self.lookahead.take().expect("No token to consume")
    }

    // Get the current lookahead token, after making sure there is one.
    fn token(&mut self) -> Option<Token<'a>> {
        while self.lookahead.is_none() {
            match self.lex.next() {
                Some(Ok(LocatedToken { token, location })) => {
                    self.lookahead = Some(token);
                    self.loc = location;
                }
                Some(Err(LocatedError { error, location })) => {
                    self.lex_error = Some(error);
                    self.loc = location;
                    break;
                }
                None => break,
            }
        }
        self.lookahead
    }

    // Match and consume a token without payload.
    fn match_token(&mut self, want: Token<'a>, err_msg: &str) -> ParseResult<Token<'a>> {
        if self.token() == Some(want) {
            Ok(self.consume())
        } else {
            err!(self.loc, err_msg)
        }
    }

    // If the next token is a `want`, consume it, otherwise do nothing.
    fn optional(&mut self, want: Token<'a>) -> bool {
        if self.token() == Some(want) {
            self.consume();
            true
        } else {
            false
        }
    }

    // Match and consume a specific identifier string.
    // Used for pseudo-keywords like "stack_slot" that only appear in certain contexts.
    fn match_identifier(&mut self, want: &'static str, err_msg: &str) -> ParseResult<Token<'a>> {
        if self.token() == Some(Token::Identifier(want)) {
            Ok(self.consume())
        } else {
            err!(self.loc, err_msg)
        }
    }
    // Match and consume a function reference.
    fn match_fn(&mut self, err_msg: &str) -> ParseResult<FuncRef> {
        if let Some(Token::FuncRef(fnref)) = self.token() {
            self.consume();
            if let Some(fnref) = FuncRef::with_number(fnref) {
                return Ok(fnref);
            }
        }
        err!(self.loc, err_msg)
    }

    // Match and consume a block reference.
    fn match_block(&mut self, err_msg: &str) -> ParseResult<Block> {
        if let Some(Token::Block(block)) = self.token() {
            self.consume();
            Ok(block)
        } else {
            err!(self.loc, err_msg)
        }
    }

    // Match and consume a value reference.
    fn match_value(&mut self, ctx: &mut Context, err_msg: &str) -> ParseResult<Value> {
        if let Some(Token::Value(v)) = self.token() {
            while ctx.function.dfg.num_values() <= usize::from(v) {
                ctx.function.dfg.make_invalid_value();
            }

            self.consume();
            Ok(v)
        } else {
            err!(self.loc, err_msg)
        }
    }

    fn error(&self, message: &str) -> ParseError {
        ParseError { location: self.loc, message: message.to_string(), is_warning: false }
    }

    // Match and consume an Imm64 immediate.
    fn match_imm(&mut self, err_msg: &str) -> ParseResult<i32> {
        match_imm!(i32, u32, self, err_msg)
    }

    // Match and consume an Ieee64 immediate.
    fn match_ieee64(&mut self, err_msg: &str) -> ParseResult<Ieee64> {
        if let Some(Token::Float(text)) = self.token() {
            self.consume();
            // Lexer just gives us raw text that looks like a float.
            // Parse it as an Ieee64 to check for the right number of digits and other issues.
            text.parse().map_err(|e| self.error(e))
        } else {
            err!(self.loc, err_msg)
        }
    }

    // Match and consume a boolean immediate.
    fn match_str(&mut self, err_msg: &str) -> ParseResult<Spur> {
        if let Some(Token::String(text)) = self.token() {
            self.consume();
            Ok(self.interner.get_or_intern(text))
        } else {
            err!(self.loc, err_msg)
        }
    }

    // Match and consume an identifier.
    fn match_any_identifier(&mut self, err_msg: &str) -> ParseResult<&'a str> {
        if let Some(Token::Identifier(text)) = self.token() {
            self.consume();
            Ok(text)
        } else {
            err!(self.loc, err_msg)
        }
    }

    /// Parse an optional source location.
    ///
    /// Return an optional source location if no real location is present.
    fn optional_srcloc(&mut self) -> ParseResult<SourceLoc> {
        if let Some(Token::SourceLoc(text)) = self.token() {
            match i32::from_str_radix(text, 16) {
                Ok(num) => {
                    self.consume();
                    Ok(SourceLoc::new(num))
                }
                Err(_) => err!(self.loc, "invalid source location: {}", text),
            }
        } else {
            Ok(Default::default())
        }
    }

    /// Parse a list of function definitions.
    ///
    /// This is the top-level parse function matching the whole contents of a file.
    pub fn parse_function_list(&mut self) -> ParseResult<Vec<Function>> {
        let mut list = Vec::new();
        while self.token().is_some() {
            list.push(self.parse_function()?);
        }
        if let Some(err) = self.lex_error {
            return match err {
                LexError::InvalidChar => err!(self.loc, "invalid character"),
                LexError::InvalidEscapeSequence => err!(self.loc, "invalid escape sequence"),
            };
        }
        Ok(list)
    }

    // Parse a whole function definition.
    //
    // function ::= * "function" name signature "{" preamble function-body "}"
    //
    fn parse_function(&mut self) -> ParseResult<Function> {
        // Begin gathering comments.
        // Make sure we don't include any comments before the `function` keyword.
        self.token();

        self.match_identifier("function", "expected 'function'")?;

        // function ::= "function" * name signature "{" preamble function-body "}"
        let name = self.parse_external_name()?;

        let mut ctx = Context::new(Function::with_name(name));
        self.parse_func_params(&mut ctx)?;

        // function ::= "function" name signature * "{" preamble function-body "}"
        self.match_token(Token::LBrace, "expected '{' before function body")?;

        self.token();

        // function ::= "function" name signature "{" * preamble function-body "}"
        self.parse_preamble(&mut ctx)?;
        // function ::= "function" name signature "{"  preamble * function-body "}"
        self.parse_function_body(&mut ctx)?;
        // function ::= "function" name signature "{" preamble function-body * "}"
        self.match_token(Token::RBrace, "expected '}' after function body")?;

        // Collect any comments following the end of the function, then stop gathering comments.
        self.token();

        Ok(ctx.function)
    }

    // Parse the function preamble.
    //
    // preamble      ::= * { preamble-decl }
    // preamble-decl ::= * stack-slot-decl
    //                   * function-decl
    //                   * signature-decl
    //                   * jump-table-decl
    //                   * stack-limit-decl
    //
    // The parsed decls are added to `ctx` rather than returned.
    fn parse_preamble(&mut self, ctx: &mut Context) -> ParseResult<()> {
        loop {
            match self.token() {
                Some(Token::FuncRef(..)) => {
                    self.parse_signature_decl().and_then(|(fn_, dat)| ctx.add_sig(fn_, dat))
                }
                Some(Token::Value(dst)) => {
                    while ctx.function.dfg.num_values() <= usize::from(dst) {
                        ctx.function.dfg.make_invalid_value();
                    }
                    self.consume();
                    self.match_token(
                        Token::Equal,
                        "expected '=' between constant definition and value",
                    )?;

                    while ctx.function.dfg.num_values() <= usize::from(dst) {
                        ctx.function.dfg.make_invalid_value();
                    }
                    match self.token() {
                        Some(Token::Identifier("fconst")) => {
                            self.consume();
                            let val = self.match_ieee64("expected float value")?;
                            ctx.function.dfg.values.fconst_at(val, dst)
                        }
                        Some(Token::Identifier("iconst")) => {
                            self.consume();
                            let val = self.match_imm("expected int value")?;
                            ctx.function.dfg.values.iconst_at(val, dst)
                        }
                        Some(Token::Identifier("sconst")) => {
                            self.consume();
                            let val = self.match_str("expected str value")?;
                            ctx.function.dfg.values.sconst_at(val, dst)
                        }
                        _ => {
                            return err!(
                                self.loc,
                                "expected 'fconst', 'sconst', 'iconst' or function signature"
                            )
                        }
                    }

                    Ok(())
                }
                // More to come..
                _ => return Ok(()),
            }?;
        }
    }

    // Parse an external name.
    //
    // For example, in a function decl, the parser would be in this state:
    //
    // function ::= "function" * name signature { ... }
    //
    fn parse_external_name(&mut self) -> ParseResult<String> {
        match self.token() {
            Some(Token::Name(s)) => {
                self.consume();
                s.parse().map_err(|_| self.error("invalid test case or libcall name"))
            }

            _ => err!(self.loc, "expected external name"),
        }
    }

    // Parse a function signature.
    //
    // signature ::=  * "%"name "(" argcount ")" "->" retcount
    //
    fn parse_signature(&mut self) -> ParseResult<FunctionSignature> {
        let has_sideeffects = !self.optional(Token::Identifier("const"));
        self.match_identifier("fn", "expected 'fn'")?;

        let mut sig = FunctionSignature {
            name: self.parse_external_name()?,
            params: 0,
            returns: 0,
            has_sideeffects,
        };

        self.match_token(Token::LPar, "expected function signature: ( args... )")?;
        // signature ::=  * "(" argcount ")" "->" retcount
        sig.params = self.match_imm("expected integer")? as u16;
        self.match_token(Token::RPar, "expected ')' after function arguments")?;
        if self.optional(Token::Arrow) {
            sig.returns = self.match_imm("expected integer")? as u16;
        }

        Ok(sig)
    }

    // Parse a signature decl.
    //
    // signature-decl ::= SigRef(sigref) "=" signature
    //
    fn parse_signature_decl(&mut self) -> ParseResult<(FuncRef, FunctionSignature)> {
        let sig = self.match_fn("expected signature number: sig«n»")?;
        self.match_token(Token::Equal, "expected '=' in signature decl")?;
        let data = self.parse_signature()?;

        // Collect any trailing comments.
        self.token();

        Ok((sig, data))
    }

    // Parse a function body, add contents to `ctx`.
    //
    // function-body ::= * { extended-basic-block }
    //
    fn parse_function_body(&mut self, ctx: &mut Context) -> ParseResult<()> {
        while self.token() != Some(Token::RBrace) {
            self.parse_basic_block(ctx)?;
        }

        Ok(())
    }

    // Parse a basic block, add contents to `ctx`.
    //
    // extended-basic-block ::= * block-header { instruction }
    // block-header         ::= Block(block) [block-params] [block-flags] ":"
    // block-flags          ::= [Cold]
    //
    fn parse_basic_block(&mut self, ctx: &mut Context) -> ParseResult<()> {
        let block_num = self.match_block("expected block header")?;
        let block = ctx.add_block(block_num)?;

        if u32::from(block_num) >= MAX_BLOCKS_IN_A_FUNCTION {
            return Err(self.error("too many blocks"));
        }

        self.match_token(Token::Colon, "expected ':' after block parameters")?;

        // Collect any trailing comments.
        self.token();

        // extended-basic-block ::= block-header * { instruction }
        while matches!(
            self.token(),
            Some(Token::Value(_) | Token::Identifier(_) | Token::LBracket | Token::SourceLoc(_),)
        ) {
            let srcloc = self.optional_srcloc()?;

            // We need to parse instruction results here because they are shared
            // between the parsing of value aliases and the parsing of instructions.
            //
            // inst-results ::= Value(v) { "," Value(v) }
            let results = self.parse_inst_results(ctx)?;

            match self.token() {
                Some(Token::Equal) => {
                    self.consume();
                    self.parse_instruction(&results, srcloc, ctx, block)?;
                }
                _ if !results.is_empty() => return err!(self.loc, "expected -> or ="),
                _ => self.parse_instruction(&results, srcloc, ctx, block)?,
            }
        }

        Ok(())
    }

    // Parse parenthesized list of block parameters. Returns a vector of (u32, Type) pairs with the
    // value numbers of the defined values and the defined types.
    //
    // block-params ::= * "(" block-param { "," block-param } ")"
    fn parse_func_params(&mut self, ctx: &mut Context) -> ParseResult<()> {
        self.match_token(Token::LPar, "expected '('")?;
        let mut i = 0u32;
        if self.token() != Some(Token::RPar) {
            loop {
                self.parse_function_param(ctx, i.into())?;
                i += 1;

                if !self.optional(Token::Comma) {
                    break;
                }
            }
        }

        self.match_token(Token::RPar, "expected ')'")?;

        Ok(())
    }

    // Parse a single block parameter declaration, and append it to `block`.
    //
    // block-param ::= * Value(v) ":" Type(t) arg-loc?
    // arg-loc ::= "[" value-location "]"
    //
    fn parse_function_param(&mut self, ctx: &mut Context, param: Param) -> ParseResult<()> {
        // block-param ::= * Value(v) ":" Type(t) arg-loc?
        let v = self.match_value(ctx, "function argument must be a value")?;

        while ctx.function.dfg.num_values() <= usize::from(v) {
            ctx.function.dfg.make_invalid_value();
        }

        // Allocate the block argument.
        ctx.function.dfg.values.make_param_at(param, v);

        Ok(())
    }

    // Parse instruction results and return them.
    //
    // inst-results ::= Value(v) { "," Value(v) }
    //
    fn parse_inst_results(&mut self, ctx: &mut Context) -> ParseResult<Vec<Value>> {
        // Result value numbers.
        let mut results = Vec::new();

        // instruction  ::=  * [inst-results "="] Opcode(opc) ["." Type] ...
        // inst-results ::= * Value(v) { "," Value(v) }
        if let Ok(v) = self.match_value(ctx, "") {
            results.push(v);

            // inst-results ::= Value(v) * { "," Value(v) }
            while self.optional(Token::Comma) {
                // inst-results ::= Value(v) { "," * Value(v) }
                results.push(self.match_value(ctx, "expected result value")?);
            }
        }

        Ok(results)
    }

    // Parse an instruction, append it to `block`.
    //
    // instruction ::= [inst-results "="] Opcode(opc) ["." Type] ...
    //
    fn parse_instruction(
        &mut self,
        results: &[Value],
        srcloc: SourceLoc,
        ctx: &mut Context,
        block: Block,
    ) -> ParseResult<()> {
        // instruction ::=  [inst-results "="] * Opcode(opc) ["." Type] ...
        let opcode = if let Some(Token::Identifier(text)) = self.token() {
            match text.parse() {
                Ok(opc) => opc,
                Err(msg) => return err!(self.loc, "{}: '{}'", msg, text),
            }
        } else {
            return err!(self.loc, "expected instruction opcode");
        };
        self.consume();

        // instruction ::=  [inst-results "="] Opcode(opc) ["." Type] * ...
        let inst_data = self.parse_inst_operands(ctx, opcode)?;

        // We're done parsing the instruction now.
        //
        // We still need to check that the number of result values in the source matches the opcode
        // or function call signature. We also need to create values with the right type for all
        // the instruction results.
        let inst = ctx.function.dfg.make_inst(inst_data);
        let num_results =
            ctx.function.dfg.make_inst_results_reusing(inst, results.iter().map(|x| Some(*x)));
        ctx.function.layout.append_inst_to_bb(inst, block);

        if !srcloc.is_default() {
            ctx.function.srclocs[inst] = srcloc;
        }

        if results.len() != num_results {
            return err!(
                self.loc,
                "instruction produces {} result values, {} given",
                num_results,
                results.len()
            );
        }

        // Collect any trailing comments.
        self.token();

        Ok(())
    }

    // Parse comma-separated value list into a VariableArgs struct.
    //
    // value_list ::= [ value { "," value } ]
    //
    fn parse_value_list(&mut self, ctx: &mut Context) -> ParseResult<VariableArgs> {
        let mut args = VariableArgs::new();

        if let Ok(v) = self.match_value(ctx, "") {
            args.push(v);
        } else {
            return Ok(args);
        }

        while self.optional(Token::Comma) {
            args.push(self.match_value(ctx, "expected value in argument list")?);
        }

        Ok(args)
    }

    // Parse the operands following the instruction opcode.
    // This depends on the format of the opcode.
    fn parse_inst_operands(
        &mut self,
        ctx: &mut Context,
        opcode: Opcode,
    ) -> ParseResult<InstructionData> {
        let idata = match opcode.format() {
            InstructionFormat::Unary => InstructionData::Unary {
                opcode,
                arg: self.match_value(ctx, "expected SSA value operand")?,
            },
            InstructionFormat::Binary => {
                let lhs = self.match_value(ctx, "expected SSA value first operand")?;
                self.match_token(Token::Comma, "expected ',' between operands")?;
                let rhs = self.match_value(ctx, "expected SSA value second operand")?;
                InstructionData::Binary { opcode, args: [lhs, rhs] }
            }
            InstructionFormat::Jump => {
                // Parse the destination block number.
                let block_num = self.match_block("expected jump destination block")?;
                InstructionData::Jump { destination: block_num }
            }

            // br <COND>, <THEN_DST>[tag]?, <ELSE_DST>
            InstructionFormat::Branch => {
                let cond = self.match_value(ctx, "expected SSA value control operand")?;
                self.match_token(Token::Comma, "expected ',' between operands")?;
                let then_dst = self.match_block("expected branch destination block")?;
                let loop_entry = if self.optional(Token::LBracket) {
                    let res = match self.match_any_identifier("expected looptag")? {
                        "loop" => true,
                        _ => return err!(self.loc, "expected 'loop'"),
                    };
                    self.match_token(Token::RBracket, "expected ']'")?;
                    res
                } else {
                    false
                };
                self.match_token(Token::Comma, "expected ',' between operands")?;
                let else_dst = self.match_block("expected branch destination block")?;
                InstructionData::Branch { loop_entry, cond, then_dst, else_dst }
            }
            InstructionFormat::Call => {
                let func_ref = self.match_fn("expected function reference")?;
                self.match_token(Token::LPar, "expected '(' before arguments")?;
                let args = self.parse_value_list(ctx)?;
                self.match_token(Token::RPar, "expected ')' after arguments")?;
                InstructionData::Call {
                    func_ref,
                    args: args.into_value_list(&[], &mut ctx.function.dfg.insts.value_lists),
                }
            }
            InstructionFormat::PhiNode => {
                let mut args = ValueList::new();
                let mut blocks = Map::new();
                loop {
                    let (val, block) = self.parse_phi_edge(ctx)?;
                    let pos = args.push(val, &mut ctx.function.dfg.insts.value_lists);
                    blocks.insert(block, pos as u32, &mut ctx.function.dfg.phi_forest, &());
                    if !self.optional(Token::Comma) {
                        break;
                    }
                }

                InstructionData::PhiNode(PhiNode { args, blocks })
            }
        };
        Ok(idata)
    }

    fn parse_phi_edge(&mut self, ctx: &mut Context) -> ParseResult<(Value, Block)> {
        self.match_token(Token::LBracket, "expected '['")?;
        let val = self.match_value(ctx, "expected phi value")?;
        self.match_token(Token::Comma, "expected ',' between phi value and block")?;
        let block = self.match_block("expected phi block")?;
        self.match_token(Token::RBracket, "expected ']' to end phi edge")?;
        Ok((val, block))
    }
}
