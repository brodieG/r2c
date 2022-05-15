# fapply - Fast Repeated Numerical Calculations in R.

**Proof of Concept**.  Lightly tested, experimental.  TBD if this becomes a
supported package.

## Overview

R code using a small subset of R functions and operating exclusively on numeric
vectors can be converted to C and compiled to native instructions.  The standard
arithmetic operators and basic statistics are supported, and the user can
combine these to implement statics.

Once compiled, the code can be called repeatedly with varying data without R
interpreter overhead.  Currently `fapply` supports running the compiled code
against non-overlapping groups in `data.frame`s.

## Performance


## How Does it Work?


## Future

There is a good chance none of this gets implemented.  How much I do will be a
function of interest, mine and others.

### More Functions

Any function that observes vectorized/recycling semantics (e.g. arithmetic
operators), outputs fixed sized output (e.g. `mean`, `range`), or outputs
results the size of an input (e.g. `quantile`), can be accommodated by the
`fapply` framework.  However, it is still necessary to implement them in C, so
functions that have direct analogues in C or that can be coded simply in C are
the best candidates.

The following should be straightforward to implement:

* `cos`, `sin`, and other trigonometric functions.
* `abs`, unary `+`, `-`.
* `min`, `max`, `first`, `last`.
* `range`.
* `length`, `seq_along`.
* and many others.
* `[[`, but only with integer-like scalar double index values.
* `[`, but only with integer-like double index values.

More challenging due to code complexity, but otherwise compatible with `fapply`:

* `quantile`.
* and others.

Some other useful functions will require more work:

* `diff`, because the result size of `n - 1` is not currently supported.

Functions that will likely not be implementable:

* `seq`, except perhaps for narrow cases where the parameters are constants  or
  perhaps select expressions such as `length(n)`, but even this becomes
  complicated..

### Other Repetition Structures

#### Window Functions

It will be straightforward to implement a runner that invokes the compiled code
on a sliding window instead of on groups.  The main complication is accounting
for incomplete windows at the beginning and end of the data.

#### Loops

It should be possible to implement a narrow set of 

Conceivably 

#### Overlapping Windows

adapt the existing code to apply a
compiled expression 

* Window functions.
* Lead/Lag etc.

### Assignment and Multi-Line Statements

To allow for explicit intermediate variables.

### Optimizations

* More aggressive re-use of intermediate memory.
* Identification of re-used calculations.

### Function Interface


could use any combination of these to create their own statistics.
Additionally, a "runner" will be provided to allow the native code to be called
with different groups of the data without incurring the R evaluation overhead
for each group.

Conceptually this is similar to what e.g. `data.table`'s gforce, which for
common statistics such as `mean(x)` skips the R function completely and instead
runs native instructions on each group.  The primary difference is that with
`fapply` it will be possible to compose complex statistics from the supported
functions, whereas gforce is limited to single function single symbol
expressions like `mean(x)`.

This also bears some resemblance to `{inline}` and `{Rcpp}` in as much as there
is a need to compile C code, but unlike those the user needs no experience
whatsoever with C or C++.

## Related Work

* Odin
* `data.table`'s Gforce.
* `dplyr`'s ?


