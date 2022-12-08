<picture>
  <source media="(prefers-color-scheme: dark)" srcset="logo_light.svg">
  <source media="(prefers-color-scheme: light)" srcset="logo_dark.svg">
  <img alt="OpenVAF" src="logo_dark.svg">
</picture>

<br>    
<br>
<br>


OpenVAF is a Verilog-A compiler that can compile Verilog-A source files to machine code for use in circuit simulators.
The major goal of this Project is to provide a high-quality standard compliant compiler for Verilog-A.
Furthermore, the project aims to bring modern compiler construction algorithms/data structures to a field with a sever lack of such tooling.

Some highlights of OpenVAF include:

* **very fast compile** times (usually below 1 second for most compact models)
* a high-quality **user interface**
* **easy setup** (no runtime dependencies even for cross-compilation)
* **fast simulations** surpassing existing solutions by 30%-60%, often matching handwritten models
* IDE aware design

## Projects

The development of OpenVAF and related tools are tightly coupled and therefore happens in a single repository.
This repository currently contains the following useable projects:

### OpenVAF - Verilog-A Compiler

OpenVAF is the main project of the repository and all other tools use OpenVAF as a library in some form.
OpenVAF can be built as a standalone CLI program that can compile Verilog-A files to shared objects that comply with the simulator-independent OSDI interface.

OpenVAF has been tested with a preliminary Ngspice prototype and Melange.
It can already support a large variety of compact models.
However, due to the larger feature set (compared to VerilogAE), additional integration tests and verification are ongoing.
Furthermore, some Verilog-A language features are currently not supported.

### VerilogAE - Verilog-A in Python

VerilogAE allows to interface model equations defined in Verilog-A source files from Python. 
VerilogAE is primarily useable from Python (available on pypi) but can also be compiled as a static/shared library and can be called by any programming language (that supports the C API).

VerilogAE is used in production since 2020 at SemiMod GmbH and various partners for compact model parameter extraction in Python.
It is stable and ready for general use.

### Melange

Melange is an experimental circuit simulator that leverages OpenVAF to interface compact models.
This enables it to easily support a large array of models without the huge effort associated with model implementation in traditional circuit simulators.
The focus of Melange is primarily on automation.
That means Melange focuses on providing an ergonomic and extensible API in mainstream programming languages (python and rust currently) instead of a special-purpose netlist format. 
However, to remain compatible with existing PDKs, a subset of the spectre netlist format can be parsed.

Melange is currently in early development and most features are not complete.
Some mockups of planned usage can be found in `melange/examples`.
Some working minimal examples (in rust) can be found in `melange/core/test.rs`.

## Building OpenVAF

Building OpenVAF requires rust/cargo 1.63, which is recommended to be installed with [rustup](https://rustup.rs/).
Furthermore, LLVM-14 development libraries and the corresponding toolchain are required.
Older versions are not supported and will produce compilation errors.
Newer versions might work, however, OpenVAF directly uses some internals of the `lld-linker` that may change between versions.
OpenVAF requires the following components of the LLVM toolchain:

- llvm-ar/llvm-link/llvm-dlltool
- clang/clang-cl
- lld development libraries
- LLVM development libraries

On fedora, these can be installed with `sudo dnf install llvm-devel lld-devel`. 
On debian based distros, the [llvm provided packages](https://apt.llvm.org/) can be used instead.
Windows developers must compile LLVM themselves.
Once all dependencies are satisfied you can build the entire project by running:

``` shell
cargo build
```

By default, OpenVAF will link against the static LLVM libraries, to avoid runtime dependencies.
However, package managers usually do not provide all required static libraries so shared libraries are usually used during development.
Simply set the `LLVM_LINK_SHARED` environment variable during linking to use the shared system libraries.
If multiple LLVM versions are installed (often the case on Debian) the `LLVM_CONFIG` environment variable can be set to the path of the correct `llvm-config` binary.
Release binaries are built-in [special docker images](https://github.com/pascalkuthe/ferris-ci) that include all necessary dependencies.
An example build invocation using shared libraries on Debian is shown below:

``` shell
LLVM_LINK_SHARED=1 LLVM_CONFIG="llvm-config-14" cargo build
```

OpenVAF includes many integration and unit tests inside its source code.
For development, [cargo-nexttest](https://nexte.st/) is recommended to run these tests as it significantly reduces the test runtime.
However, the built-in cargo test runner (which requires no extra installation) can also be used.
To run the test-suite simply call:

``` shell
cargo test # default test runner, requires no aditional installation
cargo nextest # using cargo-nextest, much faster but must be installed first
```

By default, the test-suite will skip slow integration tests that compile entire compact models.
These can be enabled by setting the `RUN_SLOW_TESTS` environment variable:

``` shell
RUN_SLOW_TESTS=1 cargo test 
```

## Acknowledgement

The architectures of the [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer) and [rustc](https://github.com/rust-lang/rust/) have heavily inspired the design of this compiler.

## Copyright

This work is free software and licensed under the GPL-3.0 license.
It contains code that is derived from [rustc](https://github.com/rust-lang/rust/) and [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer). These projects are both licensed under the MIT license. As required a copy of the license and disclaimer can be found in `copyright/LICENSE_MIT`