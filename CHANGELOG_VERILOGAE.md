# Changelog - VerilogAE

All notable changes to OpenVAF relevant to VerilogAE will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 1.0 - 2023-02-01

### Added

* Added errors for branches with incompatible disciplines.
* Statically integrate the `lld` linker and C runtime shims to remove any external dependencies.
* Enable LLVM Scalar Vectorization to automatically use SIMD instructions where possible.

### Fixed

* Provide errors instead of crashing for illegal nature access.
* Rare miss-compilations/crashes caused by treating a branch instruction as a jump instruction during CFG simplification.
* Discontinuity in the derivative of `pow(x,y)` for `x=0`.
* Unhelpful syntax errors for item declarations
* Crash when encountering potential/flow probe with no arguments
* Swapped signatures for `slew` and `transition`
* `aliasparam` declarations being ignored

##  0.9.0-beta8 - 2022-07-19

### Fixed

* Fixed select expressions producing incorrect values.
* Fixed crash when using retrieve on variable without any writes

### Changed

* Do not optimize parameter value checks to drastically reduce compile times.

##  0.9.0-beta7 - 2022-06-24

### Fixed

* Fix max_exlusive was always set to min_inclusive.

## 0.9.0-beta6 - 2022-06-01

Initial release of VerilogAE as a library
