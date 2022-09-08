# OpenVAF

[![license](https://img.shields.io/badge/license-GPL%203.0-brightgreen)](https://gitlab.com/DSPOM/OpenVAF/-/blob/master/LICENSE)
![maintaince](https://img.shields.io/badge/maintenance-actively--developed-informational)
[![pipeline status](https://gitlab.com/DSPOM/OpenVAF/badges/master/pipeline.svg)](https://gitlab.com/DSPOM/OpenVAF/-/commits/master)

OpenVAF is a Verilog-A compiler that can compile Verilog-A files for use in circuit simulator.
The major aim of this Project is to provide a high-quality standard compliant compiler for Verilog-A.
Furthermore, the project aims to bring modern compiler construction algorithms/data structures to a field with a lack of such tooling.

Some highlights of OpenVAF include:

* fast compile times (usually below 1 second for most compact models)
* high-quality diagnostic messages
* easy installation (no run dependencies even for cross compilation)
* fast runtime surpassing existing solutions by 30%-60%, often matching handwritten models
* IDE aware design

## Projects

The development of OpenVAF and related tools is tightly coupled and therefore happens in a single repository.
This repositry currently contains the following useable projects:

### OpenVAF

OpenVAF is the main project of the repository and all other tools use openvaf as a library in some form.
OpenVAF can be build as a standalone CLI program that can compile Verilog-A files to shared objects that comply with the simulator independent OSDI interface.

OpenVAF has been tested with a preliminary NGSPICE prototype and melange.
It can already support a large array of compact models.
However due to the larger feature set (compared to VerilogAE) additional testing and verification is still required.
Furthermore some Verilog-A language features are currently not supported.

### VerilogAE

Allows obtaining model equations (calculates the value of a single Variable) from Verilog-A files.
VerilogAE is primarily useable from python (available on pypi) but can also be compiled as a static/shared library and called by any programming language (that supports the C ABI).

VerilogAE is used in production at SemiMod and various partners.
Its stable and ready for general use.

### Melange

Melange is an experimental circuit simulator that leverage OpenVAF to gain access to compact models.
This allows it to easily support a large array of models without the huge effort associated with traditional simulators.
The focus of melange is primarily on automation.
That means melange focuses on providing an ergonomic and extensible API in mainstream programming languages (python and rust currently) instead of a special purpose netlist format.
However, to remain compatible with existing PDKs a subset of the spectre netlist format can be parsed.

Melange is currently in early development and most features are not complete.
Some mockups of planned usage can be found in melange/examples.
Some working minimal examples (in rust) can be found in melange/core/test.rs

## Building OpenVAF

Building OpenVAF requires rust/cargo 1.63 (best installed with [rustup](https://rustup.rs/)).
Furthermore LLVM-14 development libaries and toolchain are required.
Older versions are not supported and will produce compilation errors.
Newever version might work, however OpenVAF directly uses some internals of the lld-linker that may change between versions.
Openvaf requires the following components of the llvm toolchain:

- llvm-ar/llvm-link/llvm-dlltool
- clang/clang-cl
- lld developement libaries
- llvm development libaraies

On fedora these can be installed with `sudo dnf install llvm-devel lld-devel`. 
On debian based distros the [llvm provided packages](https://apt.llvm.org/) can be used instead.
Windows developers must compile llvm themselves.
Once all dependencies are satisfied you can build the entire project by running:

``` shell
cargo build
```

By default OpenVAF will link against the static LLVM libaries, to avoid runtime dependencies.
However package managers usually do not provide all required static libaries so shared libaries are usually used during development.
Simply the the `LLVM_LINK_SHARED` enviorment variable during linking to use the shared system libaries.
If multiple LLVM versions are installed (often the case on debian) the `LLVM_CONFIG` enviorment variable can be set to the path of the correct `llvm-config` binary.
Release binaries are built in [special docker images](https://github.com/pascalkuthe/ferris-ci) that include all necessary dependencies.
An example build invocation using shared libaries on debian is shown below:

``` shell
LLVM_LINK_SHARED=1 LLVM_CONFIG="llvm-config-14" cargo build
```

OpenVAF includes many integration and unit tests inside its sourcecode.
For development [cargo-nexttest](https://nexte.st/) is recommended to run these tests as it significantly reduces the test runtime.
However the builtin cargo test runner (requires no extra installation) can also be used.
To run the testsuite simply call:

``` shell
cargo test # default test runner, requires no aditional installation
cargo nextest # using cargo-nextest, much faster but must be installed first
```

By default the testsuite will skip slow integration tests that compile entire compact models.
These can be enableded by setting the `RUN_SLOW_TESTS` enviroment variable:

``` shell
RUN_SLOW_TESTS=1 cargo test 
```

## Acknowledgement

The architectures of the [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer) and [rustc](https://github.com/rust-lang/rust/) have heavily inspired the design of this compiler.

## Copyright

This work is free software and licensed under the GPL-3.0 license.
It contains code that is derived from [rustc](https://github.com/rust-lang/rust/) and [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer). These projects are both licensed und the MIT license. As required a copy of the license and disclaimer can be found in `copyright/LICENSE_MIT`