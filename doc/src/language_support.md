OpenVAF follows the [Verilog-AMS Language Reference Manual 2.4.0][vams].
Currently the goal is only to support the analog (Verilog-A) subset definied within this standard.
That means that digital behaviour can not be parsed.


Furthermore the OpenVAF compiler is primiarly aimed at compact modelling.
To better facilitate compact modelling (and to reduce the scope of the implementation) the behaviour of the compiler
differes from the one defined in the standard. 
Herein all differences between the language subset implemented by OpenVAF and the Verilog-A subset of the [Verilog-AMS Language Reference Manual][vams] are documented.


# Incomplete Features


Many language features in the [Verilog-AMS Language Reference Manual][vams] are aimed at behavioural modelling or describing entire circuits.
These features are hard or even impossible support when statically compiling compact models.
Therefore OpenVAF purpusfully does not implement these features.
Here all lange features not implemented by OpenVAF are laid out.

*This section is currently incomplete*

## (Analog) Event Control Statements

* *Standard Section* 5.10 Analog event control statements
* *Status* Syntax for events other than `inital_step` and `final_step` can't be parsed

The Verilog-AMS standard allows marking statements with event control.
Thes statements are only executed when the indicated event has occured.
These events are usually not used in compact modelling because they may introduce discountinueties.
Therefore OpenVAF only supportes the `inital_step` and `final_step` events for initalization code.

### Examples

There are four kinds of events specified in the standard.
They are listed here with an example and an indication to show whether OpenVAF currently supports this syntax:

* Global events (`@(initial_step)`, `@(final_step)`) *suported*
* Named (manually trigerred) events (`@foo`) *not supported*
* Monitored events (`@(cross(V(smpl) - thresh, dir))`) *not supported*
* Or Events (a combination of multiple other events) `@(initial_step or cross(V(smpl)-2.5,+1))` *not supported*



# Additional Features

Some features that are not part of the VerilogA standard have been added to OpenVAF.
The need for these features arose when OpenVAF was used in practice for compact model compilation and parameter extraction.
Below a list of these features is provided together with an example.


| Feature                                                                     | Example                      |
|-----------------------------------------------------------------------------|------------------------------|
| [Voltage Derivatives](#symbolic-derivatives-by-voltage-difference)          |  `ddx(foo,V(a,b))`           |
| [Temperature Derivatives](#symbolic-derivatives-by-temperature)             |  `ddx(foo,$temperature)`     |


In the following section each feature -including a motivation- is explained in detail.
To make it easy to remain standard compliant OpenVAF will emit a warning by default when any one of the listed features are used.


## Symbolic Derivatives by Temperature 

The [Verilog-AMS Language Reference Manual][vams] allows calculating derivatives with the `ddx` ananlog filter.
However only derivatives by node voltage `V(node)` or branch currents (`I(branch)`) are allowed.
For parameter extraction derivatives by ambient Temperature (`$temperature`) may be of intereset for extracting temperature dependence.

### Guide

The behaviour of the `ddx` analog filter is extended so that `ddx(foo,$temperature)` is valid.
All voltages and currents are considered as constants (so their derivatives are zero) and only the derivative of `$temperature` is 1.
Otherwise the `ddx` filter behaves identical as when used with nodes/branches.
So the derivative of the argument is calculated by repeated application of the chain rule.

### Examples

``` verilog
x = ddx($temperature,$temperature)
y = ddx(v(node),$temperature)
z = ddx(i(branch),$temperature)
// x = 1, y=z=0

foo = 20*exp($temperature/10)+V(node)
bar = ddx(foo,$temperature)
// bar = 2*exp($temperature/10)
```




## Symbolic Derivatives by Voltage Difference

Equations of compact models usually depend upon Voltage differences `V(a,b)` (or equivalently branch voltage `V(br_ab)`).
The derivatives of these Model Equations are required/useful during parameter extraction.

They are usually emulated with the derivative of the upper node `ddx(foo,V(a,b) = ddx(foo,V(a))`.
However this stops working when an expression depends on multiple branch voltages as demonstrated by the example below.
To ensure correct behaviour it is therefore more desirable to calculate the derivative by `V(a,b)` directly.

``` verilog
foo = V(a,b) + V(c,a)
dfoo1 = ddx(foo, V(a))
dfoo2 = ddx(foo,V(a,b))
// dfoo1 = 0, dfoo2 = 1
```

### Guide

The behaviour of the `ddx` analog filter is extended so that `ddx(foo,V(node1,node2))` is valid.
Branch currents are treated as constants. Voltage access is treated as folows:

* The derivative of `V(node1,node2)` is 1
* The derivative of `V(node2,node1)` is -1
* The derivative of `V(branchX)` is 1 if the branches nodes are `node1` and `node2`: `branch (node1, node2) branchX`
* The derivative of `V(branchX)` is -1 if the branches nodes are `node2` and `node1`: `branch (node2, node1) branchX`
* In all other cases the derivative is 0

Otherwise the `ddx` filter behaves identical as when used with nodes/branches.
So the derivative of the argument is calculated by repeated application of the chain rule.

### Example

``` verilog
...

branch (b,e) br_be;

begin
    Vt = $vt;
    Ib = Isbc*exp(V(b,c)/Vt) + Isbe*exp(V(br_be)/Vt);
    gbc = ddx(Ib, V(b,c)); // gbc =  Isbc/Vt*exp(V(b,c)/Vt)
    gbe = ddx(Ib, V(b,e)); // gbe =  Isbe/Vt*exp(V(b,c)/Vt)

end

...

```

### Technical Background

For performance reasons osdic also uses these derivatives instead of derivatives by individual nodes to calculate matrix entries.
Consider a network with two nodes `a` and `b` that are connected by a single branch `br_ab` whose current only depends upon the voltage difference of these two nodes. 
The matrix entries can then be calculated as follows: 

``` verilog
ddx(I(<a>),V(a))=ddx(I(<b>),V(b)) = ddx(I(br_ab),V(a,b))
ddx(I(<a>),V(b))= ddx(I(<a>),V(b)) = - ddx(I(br_ab),V(a,b))
```

Since almost most equations in compact models have the form shown above using this tequinice effectively allows reducing the number of derivatives by a factor of 4.
Considering how large these derivatives can get it is unlikely even modern compilers could optimze these duplications away completly.
Even if they could achieve this it would significantly increase compile time.
Therefore it is preferable for osdic to calculate derivatives by voltage difference and then calculate the Matrix entries from the result.

[vams]: https://www.accellera.org/images/downloads/standards/v-ams/VAMS-LRM-2-4.pdf
[vae]: dspom.gitlab.io/VerilogAE

