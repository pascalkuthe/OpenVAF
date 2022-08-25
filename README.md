# OpenVAF

[![license](https://img.shields.io/badge/license-GPL%203.0-brightgreen)](https://gitlab.com/DSPOM/OpenVAF/-/blob/master/LICENSE)
![maintaince](https://img.shields.io/badge/maintenance-actively--developed-informational)
[![pipeline status](https://gitlab.com/DSPOM/OpenVAF/badges/master/pipeline.svg)](https://gitlab.com/DSPOM/OpenVAF/-/commits/master)
![coverage](https://img.shields.io/badge/coverage-68%25-yellowgreen)

OpenVAF is a Verilog-A compiler. The OpenVAF Project is not executable by itself
but serves as the main component of various sub-projects such as VerilogAE and OSDI.
The major aim of this Project is to provide a high-quality standard compliant compiler for Verilog-A.
Furthermore, the project aims to bring modern compiler construction algorithms/data structures to a field with a lack of such tooling.

Some highlights of OpenVAF include:

* IDE aware design
* high-quality diagnostic messages
* linting framework (similar to rustc)
* modular backend including data flow analysis and various state of the art compiler optimization algorithms
* fast binary generation using LLVM
* robust auto differentiation implementation


## Projects

The development of OpenVAF and related tools is tightly coupled and therefore happens in a single repository.
This repositry currently contains the following useable projects:

## VerilogAE

Allows obtaining model equations (calculates the value of a single Variable) from Verilog-A files.
Source code can be found in crates/verilogae.
VerilogAE is primarily useable from python (available on pypi) but can also be compiled as a static/shared library and called by any programming language (that supports the C ABI).

VerilogAE is used in production at SemiMod and various partners.
Its stable and ready for general use.

## OpenVAF - OSDI

OpenVAF is available as a standalone CLI program that can compile Verilog-A files to shared objects that comply with the simulator independent OSDI interface.
The sourcecode for the cli application can be found in crates/openvaf-driver.

OpenVAF - OSDI has been tested with a preliminary NGSPICE prototype and can already support a large array of compact models.
However due to the larger feature set (compared to VerilogAE) additional testing and verification is still required and some Verilog-A language features are currently not supported.


## Melange

Melange is an experimental circuit simulator that leverage OpenVAF to gain access to compact models.
This allows it to easily support a large array of models without the huge effort associated with traditional simulators.
The focus of melange is primarily on automation.
That means melange focuses on providing an ergonomic and extensible API in mainstream programming languages (python and rust currently) instead of a special purpose netlist format.
However, to remain compatible with existing PDKs a subset of the spectre netlist format can be parsed.

Melange is currently in early development and most features are not complete.
Some mockups of planned usage can be found in examples/melange.
A working minimal example (in rust) can be found in crates/melange/test.rs

## Compilation

Building OpenVAF requires LLVM-14 and rust/cargo 1.63 (best installed with rustup).
To build the project, simply run

    cargo build

, which will build the project. The command

    cargo test

will run the test cases.


## Acknowledgement

The architectures of the [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer) and [rustc](https://github.com/rust-lang/rust/) have heavily inspired the design of this compiler.

## Copyright

This work is free software and licensed under the GPL-3.0 license.
It contains code that is derived from [rustc](https://github.com/rust-lang/rust/) and [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer). These projects are both licensed und the MIT license. As required a copy of the license and disclaimer can be found in `copyright/LICENSE_MIT`


