# OpenVAF

<!-- [![pipeline status](https://gitlab.com/DSPOM/OpenVAF/badges/master/pipeline.svg)](https://gitlab.com/DSPOM/OpenVAF/-/commits/master) -->
[![license](https://img.shields.io/badge/license-GPL%203.0-brightgreen)](https://gitlab.com/DSPOM/OpenVAF/-/blob/master/LICENSE)
![maintaince](https://img.shields.io/badge/maintenance-actively--developed-informational)
<!-- ![documentation](https://img.shields.io/badge/maintenance-actively--developed-informational) -->
<!-- [![dcoumentation](https://img.shields.io/badge/documentation-WIP-yellow)](https://dspom.gitlab.io/OpenVAF) -->

A framework that allows implementing compilers for VerilogA aimed predominantly at compact modelling written in Rust.
The aim of this Project is to provide a high quality fully standard compliant compiler frontend for VerilogA.
Furthermore, it aims to bring modern compiler construction algorithms/data structures to a field with a lack of such tooling.
The goal is to allow the creation of opensource static analysis tools and (JIT) compilers for the use in the field.
While OpenVAF aims to be an independent library it was primarily created for use in [VerilogAE](https://dspom.gitlab.io/verilogae/). 
As such demonstration of the practical capabilities of OpenVAF can be found there.

Furthermore, note that this Project has not yet reached a 1.0 release and is still in active development as such the public API may change in the future.

Some highlights of OpenVAF include:

* High quality diagnostic messages
* A lining framework (similar to rustc) built on this framework
* A Data flow analysis framework
* Algorithms to construct control dependence graph (combined with reaching definitions this allows construction of a program dependence graph)
* A state-of-the art backward slicing algorithm using the program dependence graph
* Simple constant folding 
* A backend to automatically generate rust code in procedural macros or for build script
* High performance
* Automatic derivative calculation (currently requires that the variable and the unknown it is derived by is known. A forward autodiff algorithm may be added in the future)
* Fast binary generation using LLVM
* Modular setup to easily allow construction of multiiple compilers

# Acknowledgement

The architectur [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer) and [rustc](https://github.com/rust-lang/rust/) have heavily inspired the design of this compiler.

# Copyright

This work is free software and licensed under the GPL-3.0 license.
It contains code that is derived from [rustc](https://github.com/rust-lang/rust/) and [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer). These projects are both licensed und the MIT license. As required a copy of the license and disclaimer can be found in `copyright/LICENSE_MIT`

