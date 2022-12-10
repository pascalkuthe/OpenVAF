//! See [`Parser`].

use std::cell::Cell;

use drop_bomb::DropBomb;
use stdx::pretty::List;

use crate::event::Event;
use crate::token_set::TokenSet;
use crate::SyntaxError;
use crate::SyntaxKind::{self, EOF, ERROR, TOMBSTONE};

/// `Parser` struct provides the low-level API for
/// navigating through the stream of tokens and
/// constructing the parse tree. The actual parsing
/// happens in the `grammar` module.
///
/// However, the result of this `Parser` is not a real
/// tree, but rather a flat stream of events of the form
/// "start expression, consume number literal,
/// finish expression". See `Event` docs for more.
pub(crate) struct Parser<'t> {
    tokens: &'t [SyntaxKind],
    pos: u32,
    events: Vec<Event>,
    steps: Cell<u32>,
}

impl<'t> Parser<'t> {
    pub(super) fn new(tokens: &'t [SyntaxKind]) -> Parser<'t> {
        Parser { tokens, events: Vec::new(), steps: Cell::new(0), pos: 0 }
    }

    pub(crate) fn finish(self) -> Vec<Event> {
        self.events
    }

    /// Returns the kind of the current token.
    /// If parser has already reached the end of input,
    /// the special `EOF` kind is returned.
    pub(crate) fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    /// Lookahead operation: returns the kind of the next nth
    /// token.
    pub(crate) fn nth(&self, n: usize) -> SyntaxKind {
        assert!(n <= 3);

        let steps = self.steps.get();
        assert!(steps <= 10_000_000, "the parser seems stuck");
        self.steps.set(steps + 1);

        self.tokens.get(self.pos as usize + n).copied().unwrap_or(SyntaxKind::EOF)
    }

    pub(crate) fn nth_at_ts(&self, n: usize, ts: TokenSet) -> bool {
        ts.contains(self.nth(n))
    }
    /// Checks if the current token is `kind`.
    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    pub(crate) fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.nth(n) == kind
    }

    /// Consume the next token if `kind` matches.
    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        self.do_bump(kind);
        true
    }

    /// Checks if the current token is in `kinds`.
    pub(crate) fn at_ts(&self, kinds: TokenSet) -> bool {
        kinds.contains(self.current())
    }
    pub(crate) fn eat_ts(&mut self, kinds: TokenSet) -> bool {
        if self.at_ts(kinds) {
            self.bump_any();
            true
        } else {
            false
        }
    }

    pub(crate) fn bump_ts(&mut self, kinds: TokenSet) {
        assert!(self.eat_ts(kinds));
    }

    /// Starts a new node in the syntax tree. All nodes and tokens
    /// consumed between the `start` and the corresponding `Marker::complete`
    /// belong to the same node.
    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len() as u32;
        self.push_event(Event::tombstone());
        Marker::new(pos)
    }

    /// Consume the next token if `kind` matches.
    pub(crate) fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind), "expected {} found {}", kind, self.current());
    }

    /// Advances the parser by one token
    pub(crate) fn bump_any(&mut self) {
        let kind = self.nth(0);
        if kind == EOF {
            return;
        }
        self.do_bump(kind)
    }

    pub(crate) fn error(&mut self, err: SyntaxError) {
        let m = self.start();
        self.push_event(Event::Error { err });
        m.complete(self, ERROR);
    }

    /// Consume the next token if it is `kind` or emit an error
    /// otherwise.
    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.error(self.unexpected_token_msg(kind));
        false
    }

    pub(crate) fn expect_ts(&mut self, kinds: TokenSet) -> bool {
        if self.eat_ts(kinds) {
            return true;
        }
        self.error(self.unexpected_tokens_msg(kinds.iter().collect()));
        false
    }

    pub(crate) fn expect_ts_r(&mut self, kinds: TokenSet, recover: TokenSet) -> bool {
        if !self.expect_ts(kinds) {
            if !self.at_ts(recover) {
                self.bump_any();
            }
            false
        } else {
            true
        }
    }

    pub(crate) fn expect_with(&mut self, kind: SyntaxKind, msg: &[SyntaxKind]) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.error(self.unexpected_tokens_msg(msg.to_owned()));
        false
    }

    pub(crate) fn unexpected_token_msg(&self, kind: SyntaxKind) -> SyntaxError {
        SyntaxError::UnexpectedToken { expected: List::new(vec![kind]), found: self.current() }
    }

    pub(crate) fn unexpected_tokens_msg(&self, kinds: Vec<SyntaxKind>) -> SyntaxError {
        SyntaxError::UnexpectedToken { expected: List::new(kinds), found: self.current() }
    }

    // Create an error node and consume the next token.
    // pub(crate) fn err_and_bump(&mut self, err: SyntaxError) {
    //     self.err_recover(err, TokenSet::EMPTY);
    // }

    /// Create an error node and consume the next token.
    pub(crate) fn err_recover(&mut self, err: SyntaxError, recovery: TokenSet) -> bool {
        if self.at_ts(recovery) {
            self.error(err);
            return true;
        }

        let m = self.start();
        self.push_event(Event::Error { err });
        self.bump_any();
        m.complete(self, ERROR);
        false
    }

    fn do_bump(&mut self, kind: SyntaxKind) {
        self.pos += 1;

        self.push_event(Event::Token(kind));
    }

    fn push_event(&mut self, event: Event) {
        self.events.push(event)
    }
}

/// See `Parser::start`.
pub(crate) struct Marker {
    pos: u32,
    bomb: DropBomb,
}

impl Marker {
    fn new(pos: u32) -> Marker {
        Marker { pos, bomb: DropBomb::new("Marker must be either completed or abandoned") }
    }

    /// Finishes the syntax tree node and assigns `kind` to it,
    /// and mark the create a `CompletedMarker` for possible future
    /// operation like `.precede()` to deal with forward_parent.
    pub(crate) fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();
        let idx = self.pos as usize;
        match &mut p.events[idx] {
            Event::Start { kind: slot, .. } => {
                *slot = kind;
            }
            _ => unreachable!(),
        }
        let finish_pos = p.events.len() as u32;
        p.push_event(Event::Finish);
        CompletedMarker::new(self.pos, finish_pos)
    }

    /// Abandons the syntax tree node. All its children
    /// are attached to its parent instead.
    pub(crate) fn abandon(mut self, p: &mut Parser) {
        self.bomb.defuse();
        let idx = self.pos as usize;
        if idx == p.events.len() - 1 {
            match p.events.pop() {
                Some(Event::Start { kind: TOMBSTONE, forward_parent: None }) => (),
                _ => unreachable!(),
            }
        }
    }
}

pub(crate) struct CompletedMarker {
    start_pos: u32,
    finish_pos: u32,
}

impl CompletedMarker {
    fn new(start_pos: u32, finish_pos: u32) -> Self {
        CompletedMarker { start_pos, finish_pos }
    }

    /// This method allows to create a new node which starts
    /// *before* the current one. That is, parser could start
    /// node `A`, then complete it, and then after parsing the
    /// whole `A`, decide that it should have started some node
    /// `B` before starting `A`. `precede` allows to do exactly
    /// that. See also docs about `forward_parent` in `Event::Start`.
    ///
    /// Given completed events `[START, FINISH]` and its corresponding
    /// `CompletedMarker(pos: 0, _)`.
    /// Append a new `START` events as `[START, FINISH, NEWSTART]`,
    /// then mark `NEWSTART` as `START`'s parent with saving its relative
    /// distance to `NEWSTART` into forward_parent(=2 in this case);
    pub(crate) fn precede(self, p: &mut Parser) -> Marker {
        let new_pos = p.start();
        let idx = self.start_pos as usize;
        match &mut p.events[idx] {
            Event::Start { forward_parent, .. } => {
                *forward_parent = Some(new_pos.pos - self.start_pos);
            }
            _ => unreachable!(),
        }
        new_pos
    }

    /// Undo this completion and turns into a `Marker`
    pub(crate) fn undo_completion(self, p: &mut Parser) -> Marker {
        let start_idx = self.start_pos as usize;
        let finish_idx = self.finish_pos as usize;
        match &mut p.events[start_idx] {
            Event::Start { kind, forward_parent: None } => *kind = TOMBSTONE,
            _ => unreachable!(),
        }
        match &mut p.events[finish_idx] {
            slot @ Event::Finish => *slot = Event::tombstone(),
            _ => unreachable!(),
        }
        Marker::new(self.start_pos)
    }
}
