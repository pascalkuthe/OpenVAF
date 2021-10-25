# Lints

## Default lvls

The following lints are set to `Warn` by default

* `macro_overwritten`
* `macro_file_cutoff`
* `attribute_overwritten`
* `ignored_display_task`
* `rounding_derivative`
* `standard_nature_constants`

The following lints are set to `Deny` by default
* `constant_overflow`

Currently there are no lints set to `Allow` by default (this may change in the future)



## Explanation

Some lints have an associated documentation id (`LXXX`) because they required a bit more explanation.
These lints are covered here:

### **L001** - `standard_nature_constants`
OpenVAF has a system that allows tools to specify what nature constants they wish to use (currently used for `$vt`).
While some tools may wish to always use certain nature constants others may wish to let the user decide.
If no constants are specified the builtin **NIST2010** constants are used.
As this can be unexpected to unaware uses OpenVAF warns in this case by default


### **L002** - `constant_overflow`
OpenVAF calculates constant values at compile time for improved static analysis and faster compiler times (and in some cases to conform with the standard).
During these operations a value may overflow (by raising a number to an too high exponent or dividing by 0 for example).
In such cases OpenVAF will emit this error lint by default. 

However this error is not necessary critical on its own 
(there are situations where constant folding is required an appropriate error will be displayed then in additionally).
As such users can set this lint to allow or warn so that OpenVAF will still compile (note that this will likely lead to overflows at runtime and is therefore rarely a good idea)

