use expect_test::expect;

use crate::tests::TestDataBase;

#[test]
fn ddx() {
    let db = TestDataBase::new(
        "/root.va",
        r#"
        `include "disciplines.va"
        module diode(a, c);
            real x;
            inout c;
            inout a;
            electrical a;
            electrical c;
            branch (a,c) br_ac;
            real foo;
            analog begin
                x = ddx(1.0,V(a));
                x = ddx(1.0,I(br_ac));

                // Non standard
                x = ddx(1.0,V(a,c));
                x = ddx(1.0,$temperature);

                // these must be rejected
                x = ddx(1.0,V(br_ac));
                x = ddx(1.0,I(a,c));
                x = ddx(1.0,I(a));
                x = ddx(1.0,I(<a>));
                x = ddx(1.0,V(<a>));

                // random fuzz
                x = ddx(1.0,1.0);
                x = ddx(1.0,foo);
            end
        endmodule
    "#,
    );
    let actual = db.lower_and_check();
    expect![[r#"
        warning[L011]: the unkown supplied to the ddx operator
           ┌─ /root.va:16:29
           │
        16 │                 x = ddx(1.0,V(a,c));
           │                             ^^^^^^ unkown is not standard compliant
           │
           = help: expected one of the following
           = branch current acces: I(branch), I(a,b)
           = node voltage: V(x)
           = explicit voltage: V(x,y)
           = temperature: $temperature
           = non_standard_code is set to warn by default

        error: invalid unkown was supplied to the ddx operator
           ┌─ /root.va:20:29
           │
        20 │                 x = ddx(1.0,V(br_ac));
           │                             ^^^^^^^^ invalid ddx unkown
           │
           = help: expected one of the following
           = branch current acces: I(branch), I(a,b)
           = node voltage: V(x)
           = explicit voltage: V(x,y)
           = temperature: $temperature

        error: invalid unkown was supplied to the ddx operator
           ┌─ /root.va:21:29
           │
        21 │                 x = ddx(1.0,I(a,c));
           │                             ^^^^^^ invalid ddx unkown
           │
           = help: expected one of the following
           = branch current acces: I(branch), I(a,b)
           = node voltage: V(x)
           = explicit voltage: V(x,y)
           = temperature: $temperature

        error: invalid unkown was supplied to the ddx operator
           ┌─ /root.va:22:29
           │
        22 │                 x = ddx(1.0,I(a));
           │                             ^^^^ invalid ddx unkown
           │
           = help: expected one of the following
           = branch current acces: I(branch), I(a,b)
           = node voltage: V(x)
           = explicit voltage: V(x,y)
           = temperature: $temperature

        error: invalid unkown was supplied to the ddx operator
           ┌─ /root.va:23:29
           │
        23 │                 x = ddx(1.0,I(<a>));
           │                             ^^^^^^ invalid ddx unkown
           │
           = help: expected one of the following
           = branch current acces: I(branch), I(a,b)
           = node voltage: V(x)
           = explicit voltage: V(x,y)
           = temperature: $temperature

        error: invalid unkown was supplied to the ddx operator
           ┌─ /root.va:24:29
           │
        24 │                 x = ddx(1.0,V(<a>));
           │                             ^^^^^^ invalid ddx unkown
           │
           = help: expected one of the following
           = branch current acces: I(branch), I(a,b)
           = node voltage: V(x)
           = explicit voltage: V(x,y)
           = temperature: $temperature

        error: access of port-branch potential
           ┌─ /root.va:24:29
           │
        24 │                 x = ddx(1.0,V(<a>));
           │                             ^^^^^^ invalid potential access
           │
           = help: only the flow of port branches like <foo> can be accessed

    "#]]
    .assert_eq(&actual);
}

#[test]
fn function() {
    let db = TestDataBase::new(
        "/root.va",
        r#"
        module diode;
            analog function real test;
                input x,y;
                real x,y;
                test = x*y;
            endfunction
        endmodule
    "#,
    );
    let actual = db.lower_and_check();
    expect![[r#""#]].assert_eq(&actual);
}

#[test]
fn function_resolve() {
    let db = TestDataBase::new(
        "/root.va",
        r#"

`define EXPL_THRESHOLD  80.0
`define MAX_EXPL        5.540622384e34
`define MIN_EXPL        1.804851387e-35
`define N_MINLOG        1.0e-38
`define LN_N_MINLOG    -87.498233534
`define DELTA_1         0.02
`define REFTEMP         300.15

// Model type definitions
`define ntype           1
`define ptype          -1

// Physical Constants
`define q               1.60219e-19     // Unit: C
`define EPS0            8.85418e-12     // Unit: F/m
`define KboQ            8.61708e-5      // Unit: J/deg

module test;
    // Clamped Exponential Function
    analog function real lexp;
       input x;
       real x;

       begin
          if (x > `EXPL_THRESHOLD) begin
             lexp  =  `MAX_EXPL * (1.0 + x - `EXPL_THRESHOLD);
          end else if (x < -`EXPL_THRESHOLD) begin
             lexp  =  `MIN_EXPL;
          end else begin
             lexp  =  exp(x);
          end
       end
    endfunction

    // Clamped log Function
    analog function real lln;
       input x;
       real x;

       begin
          lln  =  ln(max(x, `N_MINLOG));
       end
    endfunction

    // Hyperbolic Smoothing Function
    analog function real hypsmooth;
       input x, c;
       real x, c;

       begin
          hypsmooth  =  0.5 * (x + sqrt(x * x + 4.0 * c * c));
       end
    endfunction

    // Hyperbolic Smoothing max Function
    analog function real hypmax;
       input x, xmin, c;
       real x, xmin, c;

       begin
          hypmax  =  xmin + 0.5 * (x - xmin - c + sqrt((x - xmin - c) * (x - xmin - c) - 4.0 * xmin * c));
       end
    endfunction

    // Temperature Dependence Type
    analog function real Tempdep;
       input PARAML, PARAMT, DELTEMP, TEMPMOD;
       real PARAML, PARAMT, DELTEMP, TEMPMOD;

       begin
          if (TEMPMOD != 0) begin
             Tempdep  =  PARAML + hypmax(PARAMT * DELTEMP, -PARAML, 1.0e-6);
          end else begin
             Tempdep  =  PARAML * hypsmooth(1.0 + PARAMT * DELTEMP - 1.0e-6, 1.0e-3);
          end
       end
    endfunction

    // Temperature Dependence Type
    analog function real Tempdep2;
        input PARAML, PARAMT, DELTEMP, TEMPMOD;
        real PARAML, PARAMT, DELTEMP, TEMPMOD;

        Tempdep2  =  PARAML + hypmax(PARAMT * DELTEMP, -PARAML, 1.0e-6);
    endfunction
endmodule
    "#,
    );
    let actual = db.lower_and_check();
    expect![[r#""#]].assert_eq(&actual);
}
