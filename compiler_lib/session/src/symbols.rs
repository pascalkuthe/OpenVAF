/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use core::fmt;
use more_asserts::assert_le;
use std::clone::Clone;
use std::hash::{Hash, Hasher};
use std::iter::Iterator;

use bumpalo::Bump;

use crate::sourcemap::span::DUMMY_SP;
use crate::sourcemap::Span;
use core::fmt::Formatter;
use core::ptr::NonNull;
use openvaf_data_structures::index_vec::{define_index_type, IndexVec};
use openvaf_data_structures::sync::Lock;
use openvaf_data_structures::HashMap;
use openvaf_macros::symbols;

use std::fmt::{Debug, Display};
use std::ops::Deref;

#[derive(Copy, Clone, Debug)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    #[inline]
    #[must_use]
    /// Constructs a new identifier from a symbol and a span.
    pub const fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }

    pub const DUMMY: Self = Self {
        name: sym::EMPTY,
        span: DUMMY_SP,
    };

    #[must_use]
    pub const fn spanned_empty(span: Span) -> Self {
        Self {
            name: sym::EMPTY,
            span,
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    /// Maps a string to an identifier with a dummy span.
    pub fn from_str(string: &str) -> Self {
        Self::new(Symbol::intern(string), DUMMY_SP)
    }

    /// Maps a string and a span to an identifier.
    #[must_use]
    pub fn from_str_and_span(string: &str, span: Span) -> Self {
        Self::new(Symbol::intern(string), span)
    }

    #[must_use]
    pub fn without_first_quote(self) -> Self {
        Self::new(
            Symbol::intern(self.as_str().trim_start_matches('\'')),
            self.span,
        )
    }

    #[must_use]
    pub fn is_reserved(self) -> bool {
        self.name.is_reserved()
    }

    #[must_use]
    pub fn is_valid_system_function_call(self) -> bool {
        self.name.is_system_function_call()
    }

    #[must_use]
    pub fn is_system_function_call(self) -> bool {
        self.name.is_system_function_call()
    }

    #[must_use]
    /// Convert the name to a `SymbolStr`. This is a slowish operation because
    /// it requires locking the symbol interner.
    pub fn as_str(self) -> SymbolStr {
        self.name.as_str()
    }
}

impl PartialEq for Ident {
    fn eq(&self, rhs: &Self) -> bool {
        self.name == rhs.name
    }
}

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.name, f)
    }
}

define_index_type! {
            /// An interned string.
            ///
            /// Internally, a `Symbol` is implemented as an index, and all operations
            /// (including hashing, equality, and ordering) operate on that index.
            pub struct Symbol = u32;

            DEBUG_FORMAT = "sym{}";

            IMPL_RAW_CONVERSIONS = true;

            DISABLE_MAX_INDEX_CHECK = !cfg!(debug_assertions);
}

impl Symbol {
    #[must_use]
    /// Maps a string to its interned representation.
    pub fn intern(string: &str) -> Self {
        with_interner(|interner| interner.intern(string))
    }

    /// Access the symbol's chars. This is a slowish operation because it
    /// requires locking the symbol interner.
    pub fn with<F: FnOnce(&str) -> R, R>(self, f: F) -> R {
        with_interner(|interner| f(interner.get(self)))
    }

    #[must_use]
    /// Convert to a `SymbolStr`. This is a slowish operation because it
    /// requires locking the symbol interner.
    ///
    /// # Safety
    pub fn as_str(self) -> SymbolStr {
        with_interner(|interner| SymbolStr::new(interner.get(self)))
    }

    #[inline]
    #[must_use]
    pub fn is_reserved(self) -> bool {
        (kw::START..kw::END).contains(&self.raw())
    }

    #[inline]
    #[must_use]
    pub fn is_valid_system_function_call(self) -> bool {
        (sysfun::START..sysfun::END).contains(&self.raw())
    }

    #[inline]
    #[must_use]
    pub fn is_system_function_call(self) -> bool {
        self.with(|contents| contents.starts_with('$'))
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.with(|contents| f.write_str(contents))
    }
}

// The `&'static str`s in this type actually point into the arena.
#[derive(Default)]
pub struct Interner {
    arena: Bump,
    names: HashMap<&'static str, Symbol>,
    strings: IndexVec<Symbol, &'static str>,
}

impl Interner {
    fn prefill(init: &[&'static str]) -> Self {
        Self {
            strings: init.iter().copied().collect(),
            names: init.iter().copied().zip((0..).map(Symbol::new)).collect(),
            arena: Bump::with_capacity(2048),
        }
    }

    pub fn intern(&mut self, string: &str) -> Symbol {
        if let Some(&name) = self.names.get(string) {
            return name;
        }

        assert_le!(self.strings.len(), u32::MAX as usize);
        #[allow(clippy::cast_possible_truncation)]
        let string = self.arena.alloc_str(string);
        // It is safe to extend the arena allocation to `'static` because we only access
        // these while the arena is still alive.
        let string: &'static str = unsafe { &*(string as *const str) };
        let symbol = self.strings.push(string);
        self.names.insert(string, symbol);
        symbol
    }

    // Get the symbol as a string. `Symbol::as_str()` should be used in
    // preference to this function.
    pub fn get(&self, symbol: Symbol) -> &str {
        self.strings[symbol]
    }
}

session_data!(static interner_lock: Lock<Interner> = Lock::new(Interner::fresh()));
#[inline]
fn with_interner<T, F: FnOnce(&mut Interner) -> T>(f: F) -> T {
    with_interner_lock(|interner_lock| f(&mut *interner_lock.lock()))
}

/// An alternative to `Symbol`, useful when the chars within the symbol need to
/// be accessed. It deliberately has limited functionality and should only be
/// used for temporary values.
///
/// Because the interner outlives any thread which uses this type, we can
/// safely treat `string` which points to interner data, as an immortal string,
/// as long as this type never crosses between threads.
//
// by creating a new thread right after constructing the interner.
#[derive(Clone, Eq, PartialOrd, Ord)]
pub struct SymbolStr {
    string: NonNull<str>,
}
impl SymbolStr {
    fn new(interner_str: &str) -> Self {
        unsafe {
            // this is save since we only use NonNull to indicate that SymbolStr is not send
            // &str is always non null anyway and the const to mut case is also save since self.string() is only ever used in deref as reading
            let raw = interner_str as *const str as *mut str;
            Self {
                string: NonNull::new_unchecked(raw),
            }
        }
    }
}

// This impl allows a `SymbolStr` to be directly equated with a `String` or
// `&str`.
impl<T: std::ops::Deref<Target = str>> std::cmp::PartialEq<T> for SymbolStr {
    fn eq(&self, other: &T) -> bool {
        self.deref() == &**other
    }
}

/// This impl means that if `ss` is a `SymbolStr`:
/// - `*ss` is a `str`;
/// - `&*ss` is a `&str`;
/// - `&ss as &str` is a `&str`, which means that `&ss` can be passed to a
///   function expecting a `&str`.
impl Deref for SymbolStr {
    type Target = str;
    #[inline]
    fn deref(&self) -> &str {
        // This is save since this is only created for interned strings which are bump allocated and therefore never moved
        // Since SymbolStr contains a raw pointer and is therefore not Sync And Senc so the threadlocal will always outlive any SymbolStr in Safe code
        unsafe { self.string.as_ref() }
    }
}

impl Debug for SymbolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

impl Display for SymbolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self.deref(), f)
    }
}

pub mod kw {
    pub use super::kw_generated::*;
}

pub mod sym {
    pub use super::sym_generated::*;
}

pub mod sysfun {
    pub use super::sysfun_generated::*;
}

symbols! {
    Keywords{
        above,
        abs,
        absdelay,
        absdelta,
        abstol,
        access,
        acos,
        acosh,
        ac_stim,
        aliasparam,
        always,
        analog,
        analysis,
        and,
        asin,
        asinh,
        assert,
        assign,
        atan,
        atan2,
        atanh,
        automatic,
        begin,
        branch,
        buf,
        bufif0,
        bufif1,
        case,
        casex,
        casez,
        ceil,
        cell,
        cmos,
        config,
        connect,
        connectmodule,
        connectrules,
        continuous,
        cos,
        cosh,
        cross,
        ddt,
        ddt_nature,
        ddx,
        deassign,
        default,

        defparam,
        design,
        disable,
        discipline,
        discrete,
        domain,
        driver_update,
        edge,
        // else, Already removed by the lexer and not allowed by the rust macros
        end,
        endcase,
        endconfig,
        endconnectrules,
        enddiscipline,
        endfunction,
        endgenerate,
        endmodule,
        endnature,
        endparamset,
        endprimitive,
        endspecify,
        endtable,
        endtask,
        event,
        exclude,
        exp,
        final_step,
        flicker_noise,
        floor,
        flow,
        // for, Already removed by the lexer and not allowed by the rust macros
        force,
        forever,
        fork,
        from,
        function,
        generate,
        genvar,
        ground,
        highz0,
        highz1,
        hypot,
        idt,
        idtmod,
        idt_nature,
        // if, Already removed by the lexer and not allowed by the rust macros

        ifnone,
        incdir,
        include,
        inf,
        initial,
        initial_step,
        inout,
        input,
        instance,
        integer,
        join,
        laplace_nd,
        laplace_np,
        laplace_zd,
        laplace_zp,
        large,
        last_crossing,
        liblist,
        library,
        limexp,
        ln,
        localparam,
        log,
        macromodule,
        max,
        medium,
        merged,
        min,
        module,
        nand,
        nature,
        negedge,
        net_resolution,
        nmos,
        noise_table,
        noise_table_log,
        nor,
        noshowcancelled,
        not,
        notif0,
        notif1,
        or,
        output,
        parameter,
        paramset,
        pmos,


        posedge,
        potential,
        pow,
        primitive,
        pull0,
        pull1,
        pulldown,
        pullup,
        pulsestyle_onevent,
        pulsestyle_ondetect,
        rcmos,
        real,
        realtime,
        reg,
        release,
        repeat,
        resolveto,
        rnmos,
        rpmos,
        rtran,
        rtranif0,
        rtranif1,
        scalared,
        sin,
        sinh,
        showcancelled,
        signed,

        slew,
        small,
        specify,
        specparam,
        split,
        sqrt,
        string,
        strong0,
        strong1,
        supply0,
        supply1,
        table,
        tan,
        tanh,
        task,
        time,
        timer,
        tran,
        tranif0,
        tranif1,
        transition,
        tri,
        tri0,
        tri1,
        triand,
        trior,
        trireg,

        units,
        unsigned,
        // use, Already removed by the lexer and not allowed by the rust macros
        uwire,
        vectored,
        wait,
        wand,
        weak0,
        weak1,
        // while, Already removed by the lexer and not allowed by the rust macros
        white_noise,
        wire,
        wor,
        wreal,
        xnor,
        xor,
        zi_nd,
        zi_np,
        zi_zd,
        zi_zp,
    }

    Symbols{
        EMPTY: " ",
        OpenVAF,

        openvaf_allow,
        openvaf_warn,
        openvaf_deny,
        openvaf_forbid,

        desc,
        op,

        ac,
        dc,
        noise
    }

    SystemFunctions{
        display,
        strobe,
        write,
        monitor,
        monitoron,
        monitoroff,
        debug,

        fclose,
        fopen,
        fdisplay,
        fwrite,
        fstrobe,
        fmonitor,
        fgets,
        fscanf,
        swrite,
        sformat,
        sscanf,
        rewind,
        fseek,
        ftell,
        fflush,
        ferror,
        feof,
        fdebug,

        finish,
        stop,
        fatal,

        warning,
        error,
        info,
        abstime,

        bitstoreal,
        realtobits,

        test_plusargs: "test$plusargs",
        value_plusargs: "value$plusargs",

        dist_chi_square,
        dist_exponential,
        dist_poisson,
        dist_uniform,
        dist_erlang,
        dist_normal,
        dist_t,
        rando,
        arandom,
        rdist_chi_square,
        rdist_exponential,
        rdist_poisson,
        rdist_uniform,
        rdist_erlang,
        rdist_normal,
        rdist_t,

        clog2,
        ln,
        log10,
        exp,
        sqrt,
        pow,
        floor,
        ceil,
        sin,
        cos,
        tan,
        asin,
        acos,
        atan,
        atan2,
        hypot,
        sinh,
        cosh,
        tanh,

        asinh,
        acosh,
        atanh,

        temperature,
        vt,
        simparam,
        simparam_str: "sympara$str",

        simprobe,

        discontinuity,
        limit,
        bound_step,

        mfactor,
        xposition,
        yposition,
        angle,

        hflip,
        vflip,

        param_given,
        port_connected,

        analog_node_alias,
        analog_port_alias,

        table_model,

    }


}
