use std::fmt::Display;
use std::ops::Deref;

use smol_str::SmolStr;

use crate::ast::{self, SysFun};
use crate::SyntaxToken;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Name(SmolStr);

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Name {
    /// Shortcut to create inline plain text name
    pub const fn new_inline(text: &str) -> Name {
        Name(SmolStr::new_inline(text))
    }

    /// Resolve a name from the text of token.
    pub fn resolve(raw_text: &str) -> Name {
        if raw_text.starts_with('\\') {
            Name(SmolStr::new(&raw_text[1..raw_text.len() - 1]))
        } else {
            Name(raw_text.into())
        }
    }

    /// A fake name for things missing in the source code.
    ///
    /// For example, `impl Foo for {}` should be treated as a trait impl for a
    /// type with a missing name. Similarly, `struct S { : u32 }` should have a
    /// single field with a missing name.
    ///
    /// Ideally, we want a `gensym` semantics for missing names -- each missing
    /// name is equal only to itself. It's not clear how to implement this in
    /// salsa though, so we punt on that bit for a moment.
    pub const fn missing() -> Name {
        Name::new_inline("[missing name]")
    }

    pub fn is_reserved(&self) -> bool {
        kw::is_reserved(self.0.as_str())
    }

    pub fn is_reserved_compat(&self) -> bool {
        kw_comp::is_reserved(self.0.as_str())
    }

    pub fn is_known_sysfun(&self) -> bool {
        sysfun::is_known(self.0.as_str())
    }

    pub fn is_sysfun(&self) -> bool {
        self.0.as_str().starts_with('$')
    }
}

impl Deref for Name {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

pub trait AsIdent {
    fn as_ident(&self) -> Option<Name>;
}

impl AsIdent for ast::Expr {
    fn as_ident(&self) -> Option<Name> {
        self.as_raw_ident().as_ref().map(AsName::as_name)
    }
}

impl AsIdent for ast::Path {
    fn as_ident(&self) -> Option<Name> {
        self.as_raw_ident().as_ref().map(AsName::as_name)
    }
}

impl From<Name> for SmolStr {
    fn from(name: Name) -> Self {
        name.0
    }
}

// impl AsIdent for crate::Path {
//     fn as_ident(&self) -> Option<Name> {
//         if self.is_root_path {
//             return None;
//         }

//         if let [name] = &*self.segments {
//             Some(name.clone())
//         } else {
//             None
//         }
//     }
// }

impl AsName for SysFun {
    fn as_name(&self) -> Name {
        Name::resolve(self.sysfun_token().unwrap().text())
    }
}

pub trait AsName {
    fn as_name(&self) -> Name;
}

impl AsName for ast::Name {
    fn as_name(&self) -> Name {
        Name::resolve(&self.text())
    }
}

impl AsName for ast::NameRef {
    fn as_name(&self) -> Name {
        Name::resolve(&self.text())
    }
}

impl AsName for ast::PathSegment {
    fn as_name(&self) -> Name {
        Name::resolve(self.syntax.text())
    }
}

impl AsName for SyntaxToken {
    fn as_name(&self) -> Name {
        Name::resolve(self.text())
    }
}

macro_rules! keywords {
    ($($ident:ident),* $(,)?) => {
        $(
            #[allow(bad_style, dead_code)]
            pub const $ident: super::Name =
                super::Name::new_inline(stringify!($ident));
        )*
        pub mod raw{
            $(
                #[allow(bad_style, dead_code)]
                pub const $ident: &str = stringify!($ident);
            )*

            #[allow(bad_style, dead_code)]
            pub const use_:&str = "use";
        }

        pub fn is_reserved(name: &str) -> bool{
            matches!(name,$(stringify!($ident) |)* "use")
        }
    };
}

pub mod kw {
    #[allow(bad_style, dead_code)]
    pub const use_: super::Name = super::Name::new_inline("use");
    #[allow(bad_style, dead_code)]
    pub const root: super::Name = super::Name::new_inline("$root");
    keywords! {
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
        case,
        ceil,
        cell,
        config,
        continuous,
        cos,
        cosh,
        cross,
        ddt,
        ddt_nature,
        ddx,
        deassign,

        defparam,
        design,
        disable,
        discipline,
        discrete,
        domain,
        driver_update,
        edge,
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
        min,
        module,
        nand,
        nature,
        negedge,
        net_resolution,
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


        posedge,
        potential,
        pow,
        primitive,
        pulsestyle_onevent,
        pulsestyle_ondetect,
        real,
        realtime,
        release,
        repeat,
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
        sqrt,
        string,
        table,
        tan,
        tanh,
        task,
        time,
        timer,
        tran,
        tranif0,
        tranif1,

        units,
        unsigned,
        uwire,
        vectored,
        wait,
        wand,
        pulldown,
        pullup,

        white_noise,
        wire,
        wor,
        xnor,
        xor,
        zi_nd,
        zi_np,
        zi_zd,
        zi_zp,

        transition,
    }
}

/// keywords that will never be used by openvaf because they belong to (exotic parts) of the
/// digital subset of VerilogAMS. According to the standard these are still reserved.
/// However some compact models still use these and OpenVAF should allow that.
/// Therefore we emit a (warn by default) lint when these are used
pub mod kw_comp {
    keywords! {
        // gate level logic
        cmos,
        nmos,
        pmos,
        rcmos,
        rnmos,
        rpmos,

        // tri state
        tri,
        tri0,
        tri1,
        triand,
        trior,
        trireg,

        // drive strengths
        highz0,
        highz1,
        pull0,
        pull1,
        weak0,
        weak1,
        strong0,
        strong1,
        supply0,
        supply1,
        medium,

        // As specified by the standard
        connect,
        connectmodule,
        connectrules,
        merged,
        net_resolution,
        resolveto,
        split,
        wreal,

        casex,
        casez,

        reg,


        buf,
        bufif0,
        bufif1,

    }
}

pub mod sysfun {
    macro_rules! system_functions {
        ($($ident:ident),* $(,)?) => {
            $(
                #[allow(bad_style, dead_code)]
                pub const $ident: super::Name =
                    super::Name::new_inline(concat!("$",stringify!($ident)));
            )*

            pub mod raw{
                $(
                    #[allow(bad_style, dead_code)]
                    pub const $ident: &str = concat!("$",stringify!($ident));
                )*

                #[allow(bad_style, dead_code)]
                pub const test_plusargs:&str= "$test$plusargs";
                #[allow(bad_style, dead_code)]
                pub const value_plusargs:&str = "$value$plusargs";
                #[allow(bad_style, dead_code)]
                pub const simparam_str: &str ="$simpara$str";
            }

            pub fn is_known(name: &str) -> bool{
                matches!(name,$(concat!("$",stringify!($ident)) |)* "$test$plusargs" | "$value$plusargs" | "$simpara$str")
            }
        };
    }

    #[allow(bad_style, dead_code)]
    pub const test_plusargs: super::Name = super::Name::new_inline("$test$plusargs");
    #[allow(bad_style, dead_code)]
    pub const value_plusargs: super::Name = super::Name::new_inline("$value$plusargs");
    #[allow(bad_style, dead_code)]
    pub const simparam_str: super::Name = super::Name::new_inline("$simparam$str");

    system_functions! {
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

        dist_chi_square,
        dist_exponential,
        dist_poisson,
        dist_uniform,
        dist_erlang,
        dist_normal,
        dist_t,
        random,
        arandom,
        rdist_chi_square,
        rdist_exponential,
        rdist_poisson,
        rdist_uniform,
        rdist_erlang,
        rdist_normal,
        rdist_t,

        abs,
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
        max,
        min,

        temperature,
        vt,
        simparam,

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
