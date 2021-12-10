# OpenVAF

[![license](https://img.shields.io/badge/license-GPL%203.0-brightgreen)](https://gitlab.com/DSPOM/OpenVAF/-/blob/master/LICENSE)
![maintaince](https://img.shields.io/badge/maintenance-actively--developed-informational)
[![builds.sr.ht status](https://builds.sr.ht/~dspom/OpenVAF.svg)](https://builds.sr.ht/~dspom/OpenVAF?)

OpenVAF is a compiler for Verilog-A aimed predominantly at compact modeling.
This Project is not an executable itself but rather servers as the main component of various sub projects such as VerilogAE and OSDIC.
The aim of this Project is to provide a high quality standard compliant compiler for Verilog-A.
Furthermore, it aims to bring modern compiler construction algorithms/data structures to a field with a lack of such tooling.

Some highlights of OpenVAF include:

* IDE aware design
* High quality diagnostic messages
* A linting framework (similar to rustc)
* A modular backend including data flow analysis and various state of the art compiler optimization algorithms
* Fast binary generation using LLVM
* A robust auto differentiation implementation
# Acknowledgement

The architectur [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer) and [rustc](https://github.com/rust-lang/rust/) have heavily inspired the design of this compiler.

# Copyright

This work is free software and licensed under the GPL-3.0 license.
It contains code that is derived from [rustc](https://github.com/rust-lang/rust/) and [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer). These projects are both licensed und the MIT license. As required a copy of the license and disclaimer can be found in `copyright/LICENSE_MIT`


