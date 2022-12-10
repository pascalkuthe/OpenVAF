use tokens::SyntaxKind;

use crate::SyntaxError;

#[derive(Default)]
pub struct Output {
    /// 32-bit encoding of events. If LSB is zero, then that's an index into the
    /// error vector. Otherwise, it's one of the thee other variants, with data encoded as
    ///
    ///     |16 bit kind|8 bit leftovers|4 bit tag|4 bit leftover|
    ///
    event: Vec<u32>,
    error: Vec<SyntaxError>,
}

#[derive(Debug)]
pub enum Step<'a> {
    Token { kind: SyntaxKind },
    Enter { kind: SyntaxKind },
    Exit,
    Error { err: &'a SyntaxError },
}

impl Output {
    pub fn iter(&self) -> impl Iterator<Item = Step<'_>> {
        self.event.iter().map(|&event| {
            if event & 0b1 == 0 {
                return Step::Error { err: &self.error[(event as usize) >> 1] };
            }
            let tag = ((event & 0x0000_00F0) >> 4) as u8;
            match tag {
                0 => {
                    let kind: SyntaxKind = (((event & 0xFFFF_0000) >> 16) as u16).into();
                    Step::Token { kind }
                }
                1 => {
                    let kind: SyntaxKind = (((event & 0xFFFF_0000) >> 16) as u16).into();
                    Step::Enter { kind }
                }
                2 => Step::Exit,
                _ => unreachable!(),
            }
        })
    }

    pub(crate) fn token(&mut self, kind: SyntaxKind) {
        let e = ((kind as u16 as u32) << 16) | 1;
        self.event.push(e)
    }

    pub(crate) fn enter_node(&mut self, kind: SyntaxKind) {
        let e = ((kind as u16 as u32) << 16) | (1 << 4) | 1;
        self.event.push(e)
    }

    pub(crate) fn leave_node(&mut self) {
        let e = 2 << 4 | 1;
        self.event.push(e)
    }

    pub(crate) fn error(&mut self, error: SyntaxError) {
        let idx = self.error.len();
        self.error.push(error);
        let e = (idx as u32) << 1;
        self.event.push(e);
    }
}
