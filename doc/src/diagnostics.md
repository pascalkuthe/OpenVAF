# Diagnostics

The biggest surface for interaction between users and a compiler are diagnostic messages such as warnings and errors.

OpenVAF impliments a general diagnostics system that allows OpenVAF tools to serve high quality messages to users.
We will use a simple diode as an example which is missing a semicolon.

``` verilog
`include "constants.va"
`include "disciplines.va"

`define DIODE_BODY\
  analog begin\
        I_s = I_s0*($temperature/T_0)**zeta\
        V_t = `P_K*$temperature/`P_Q;\
        I_d = I_s * exp( V(d) /(m*V_t) );\
   end\

module Diode(inout electrical A,inout electrical C);
  branch (A,C) d;
  parameter real I_s0 = 1E-11 from [1E-20:1];
  parameter real m = 1 from [0.1:10];
  parameter real zeta = 1;
  parameter real T_0 = 300 from [0:inf];
  real V_t,I_s;
  integer i;
  (*extract*) real I_d;

  `DIODE_BODY

endmodule
```

Compiling this snippet with an OpenVAF based tool will result in an error msg that looks as follows:

{{#include diagnostics/examples/missing_semicolon.html}}

You may notice the hint to print a macro backtrace. The exact contents of this message depend on the tool used (verilogae in this case)
but the implimentation when this feature is enable is provided by OpenVAF. Enabeling it in this case will print the following error message

{{#include diagnostics/examples/missing_semicolon_expansion.html}}

This feature can be very useful when debugging nested macros! It also works for files included using `include FILENAME`
and even arguments to macros

## Diagnostic systems

OpenVAF has two kinds of error systems (but both are based on the diagnostic system).
The first system is for critical errors that occur during compilation such as syntax errors, names that could not be found and so on.
OpenVAF is split into multiple stages which depend upon each other. While OpenVAF is able to continue the current state when an error occurs it may not start the next state.
As such the program aborts and prints all errors that occurs.

The second error system is called the linting system. Lints are diagnostics that a user may reasonably want to ignore.
As such lints do not cause OpenVAF to abort instead they are collected and printed after compilation completes.
Each lint has an associated level:

* `Deny`
* `Warn`
* `Allow`

When a lint is set to `Allow` it is ignored while `Warn` lvl lints will only print a warning.
`Deny` lvl warnings will cause the compilation to fail and print a normal looking error message.

While each lint has a default lvl lint lvls can be overwritten (the exact mechanism depends on the tool. Usually that should be CLI arguments).
This book documents all lints and their default lvl that are built into OpenVAF.

The lint system is extendable however as such different tools may add more lints. These lints always start with `*pluginname*::`.
These lints are not documented here
