# Changelog - OpenVAF OSDI

All notable changes to OpenVAF relevant to OSDI will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]

## Breaking Changes

* Removed LLD integration which made packaging exceptionally difficult and caused crashes on Windows.
  That mean that OpenVAF now requires that the system linker (and `ucrt.lib` when targeting windows) are available.

### Fixed

* Correctly detect file encoding.
* Invocations of `$limit` that use user defined functions.
* Correctly parse net declarations without attached discipline (usually `ground`).
* Panic that occurs when a model contains no branch contributes.
* Panic for voltage sources when the associated current is unused.
* Panic for parameters with `exclude` expressions.
* Panic for instance parameters.
* Swapped signatures for `slew` and `transition`
* `aliasparam` declarations being ignored

## 22.12.0 - 2022-12-16

### Added

* Added support for `absdelay`
* Added support for the `analysis` function (always returned true previously).
* Automatically remove unused nodes (includes noise network until noise support is added).
* Added errors for branches with incompatible disciplines.
* Statically integrate the `lld` linker and C runtime shims to remove any external dependencies.
* Added `--print-expansion` CLI option to print the preprocessed file.
* Enable LLVM Scalar Vectorization to automatically use SIMD instructions where possible.

### Fixed

* Do not generate unkowns for noise phase shifts with ddt.
* Provide errors instead of crashing for unsupported Verilog-A builtins.
* Provide errors instead of crashing for illegal nature access.
* Rare miss-compilations/crashes caused by treating a branch instruction as a jump instruction during CFG simplification.
* Discontinuity in the derivative of `pow(x,y)` for `x=0`.
* Unhelpful syntax errors for item declarations
* Crash when encountering potential/flow probe with no arguments

## 0.1.2 - 2022-07-19

### Added

* Added support for `$limit`
* Added support for `$abstime` (now returns a non-zero value for large signal simulations).
* Added support for `$bound_step` (now the simulator can read the minimum value passed to `$bound_step`).
* Add general description to the start of the help text.

### Changed

* Validate format arguments and properly handle all argument types instead of just passing the format specifier to C

## 0.1.1 - 2022-06-26

### Changed

* Refactor the algorithm that determines whether `ddt` needs an unkown, so that OpenVAF doesn't create one when not required.
* Automatically add whitespace between auto generated format characters for `$strope etc.`
* Check that fmt literals used in $display are correct and match the format arguments to avoid crashes/undefined behavior.

### Fixed

* `ddx` always returned 0
* Remove infinite loop during global value numbering optimization.
* Linking error if cache directory was missing during batch compilation.
* Fixed crash for nodes without any contributions besides collapsing
* Fixed select expressions producing incorrect values
* Fixed that the cache-dir argument was called chache-dir (typo)


## 0.1.0 - 2022-06-20

Initial prototype release
