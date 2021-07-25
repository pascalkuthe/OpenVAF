pub const BRANCH: &str = r#"
module test (output a,input b);
    branch (a,b) ab1,ab2;
    branch (<a>) pa;
    branch (<b>) pb;
endmodule
"#;


pub const CONTRIUBTE: &str = r#"
module test;
    analog begin
        I(x)<+(V(x)-V0)**2/(R1R2);
    end
endmodule
"#;

pub const MODULE_HEADER: &str = r#"
module test1;
endmodule

module test2 (a,b,c,d,e,f,g);
    output a;
    input b;
    inout wire c;
    inout electrical d;
    output electrical e,f;
    inout electrical wire g;
endmodule

module test3 (output a,input electrical b,inout electrical tri c);
   /*
    * lel
    *lele
    */
endmodule
"#;

pub const NET_DECLARATION: &str = r#"
module test ();
    wire x,y;
    electrical z;
    wire electrical l;

endmodule
"#;

pub const STATEMENTS: &str = r#"
`include "disciplines.va"
`include "disciplines.va"


`define branches\
    branch (A,x) ax;\
    branch (A,y) ay;\
    branch (x,B) xb;\
    branch (y,B) yb;\
    branch (x,y) xy;

`define calculate(a,b)\
    b**a/2;\
    C = C**2;


module schaltung (A,B);
    inout electrical A,B;
    electrical x,y;
    `branches
    real C = 30;

    analog begin
        if (V(ax) > 100) begin
            if (V(ax) > 100) begin
                C=42*31+1;
            end
                        if (V(ax) > 100) begin
                            C=42*31+1;
                        end else             if (V(ax) > 100) begin
                                                 C=42*31+1;
                                             end else begin
                                             end
        end
        I(xy) <+ (C*V(ax)+I(xy))/30;
        // I(ax) <+ `calculate(31,V(ax))
        I(ay) <+ V(ay)/20;
        if (V(ax) > 100) begin
            C=42*31+1;
        end
        I(ax) <+ C == 0 ? V(ax)/40 : 42;
        $stprobe("foo",c,"bar");
        ; // Empty statements are allowed by the standard
        ;
        I(ay) <+ C*V(ay)/50;
    end
endmodule
"#;

pub const VARIABLE_DECLARATION: &str = r#"
module test ();
    real x;
    integer y,z;//the default value is only here to test whether this crashes; Expressions are testes separately
    time t;
    realtime rt;
    analog x = 2==y;
endmodule
"#;
