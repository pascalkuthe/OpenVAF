# OpenVAF

[![license](https://img.shields.io/badge/license-GPL%203.0-brightgreen)](https://gitlab.com/DSPOM/OpenVAF/-/blob/master/LICENSE)
![maintaince](https://img.shields.io/badge/maintenance-actively--developed-informational)
[![builds.sr.ht status](https://builds.sr.ht/~dspom/OpenVAF.svg)](https://builds.sr.ht/~dspom/OpenVAF?)
![coverage](https://img.shields.io/badge/coverage-68%25-yellowgreen)

OpenVAF is a Verilog-A compiler. The OpenVAF Project is not executable by itself
but serves as the main component of various sub-projects such as VerilogAE and OSDIC.
The major aim of this Project is to provide a high-quality standard compliant compiler for Verilog-A.
Furthermore, the project aims to bring modern compiler construction algorithms/data structures to a field with a lack of such tooling.

Some highlights of OpenVAF include:

* IDE aware design
* high-quality diagnostic messages
* linting framework (similar to rustc)
* modular backend including data flow analysis and various state of the art compiler optimization algorithms
* fast binary generation using LLVM
* robust auto differentiation implementation

# Compilation

To build the project, simply run

    cargo build

, which will build the project. The command

    cargo test

will run the test cases.


# Acknowledgement

The architectures of the [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer) and [rustc](https://github.com/rust-lang/rust/) have heavily inspired the design of this compiler.

# Copyright

This work is free software and licensed under the GPL-3.0 license.
It contains code that is derived from [rustc](https://github.com/rust-lang/rust/) and [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer). These projects are both licensed und the MIT license. As required a copy of the license and disclaimer can be found in `copyright/LICENSE_MIT`


