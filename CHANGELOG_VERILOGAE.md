# Changelog - VerilogAE

All notable changes to OpenVAF relevant to VerilogAE will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]

### Fixed

* provide errors instead of crashing for unsupported Verilog-A builtins

##  0.9.0-beta8 - 2022-07-19

### Fixed

* Fixed select expressions producing incorrect values
* Fixed crash when using retrieve on variable without any writes

### Changed

* Do not optimize parameter value checks to drastically reduce compile times

##  0.9.0-beta7 - 2022-06-24

### Fixed

* Fix max_exlusive was always set to min_inclusive

## 0.9.0-beta6 - 2022-06-01

Initial release of VerilogAE as a library
