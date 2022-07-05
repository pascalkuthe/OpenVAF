use std::str::CharIndices;

use hir_def::ExprId;
use syntax::{TextRange, TextSize};

use crate::inference::InferenceDiagnostic;

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
enum ParserState {
    Flags,
    FixedFmtLit,
    DynamicFmtLit,
    AnyPrecison,
    FixedPrecison,
    DynamicPrecsion,
}

impl ParserState {
    fn start_precision(self) -> bool {
        self < Self::AnyPrecison
    }

    fn eat_number(self) -> bool {
        matches!(self, Self::FixedPrecison | Self::FixedFmtLit)
    }
    fn candidates(self) -> &'static [char] {
        match self {
            ParserState::Flags => &[
                '-', '+', ' ', '#', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '*', '.',
                'e', 'E', 'f', 'F', 'g', 'G', 'r', 'R', '%', 'm', 'M', 'l', 'L', 'd', 'D', 'h',
                'H', 'o', 'O', 'b', 'B', 'c', 'C', 's', 'S',
            ],
            ParserState::FixedFmtLit => &[
                '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', 'f', 'F', 'g',
                'G', 'r', 'R',
            ],
            ParserState::DynamicFmtLit => &['.', 'e', 'E', 'f', 'F', 'g', 'G', 'r', 'R'],
            ParserState::AnyPrecison => &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '*'],
            ParserState::FixedPrecison => &[
                '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'e', 'E', 'f', 'F', 'g', 'G',
                'r', 'R',
            ],
            ParserState::DynamicPrecsion => &['e', 'E', 'f', 'F', 'g', 'G', 'r', 'R'],
        }
    }
}

pub struct ParseResult {
    pub dynamic_args: Vec<TextSize>,
    pub err: Option<InferenceDiagnostic>,
    pub end: TextSize,
}

pub fn parse_real_fmt_spec(
    start: u32,
    fmt_expr: ExprId,
    mut pos: Option<(usize, char)>,
    chars: &mut CharIndices,
) -> ParseResult {
    let mut state = ParserState::Flags;
    let mut end = start + 1;
    let mut dynamic_args = Vec::new();
    let mut err = None;
    loop {
        if let Some((off, c)) = pos {
            end = (off + c.len_utf8()) as u32;
            match c {
                // flags
                '-' | '+' | ' ' | '#' if state == ParserState::Flags => {}
                '0'..='9' if state == ParserState::Flags => {
                    state = ParserState::FixedFmtLit;
                }
                '*' if state == ParserState::Flags => {
                    dynamic_args.push(off.try_into().unwrap());
                    state = ParserState::DynamicFmtLit;
                }
                '.' if state.start_precision() => {
                    state = ParserState::AnyPrecison;
                }

                '*' if state == ParserState::AnyPrecison => {
                    dynamic_args.push(off.try_into().unwrap());
                    state = ParserState::DynamicPrecsion
                }
                '0'..='9' if state == ParserState::AnyPrecison => {
                    state = ParserState::FixedPrecison
                }
                '0'..='9' if state.eat_number() => (),
                'e'..='g' | 'E'..='G' | 'r' | 'R' if state != ParserState::AnyPrecison => {
                    break;
                }
                _ => {
                    err = Some(InferenceDiagnostic::InvalidFmtSpecifierChar {
                        fmt_lit: fmt_expr,
                        lit_range: TextRange::new(off.try_into().unwrap(), end.try_into().unwrap()),
                        err_char: c,
                        candidates: state.candidates(),
                    });
                    break;
                }
            }

            pos = chars.next();
        } else {
            err = Some(InferenceDiagnostic::InvalidFmtSpecifierEnd {
                fmt_lit: fmt_expr,
                lit_range: TextRange::new(start.try_into().unwrap(), end.try_into().unwrap()),
            });
            break;
        }
    }

    ParseResult { dynamic_args, err, end: end.into() }
}
