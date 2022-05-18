# r2c - Fast Iterated Statistics in R

**Proof of Concept**.  Lightly tested, experimental, and incomplete.

Compiles a selected subset of R into native instructions so that
that expressions composed from that subset can be executed repeatedly on
varying data without interpreter overhead.

## Background and Motivation

R is nearly as fast as statically compiled languages for many common numerical
calculations.  R's vector-first variables are a major reason for this, as it
allows implicit loops, e.g. in:

```
set.seed(1)
n <- 1e7
x <- runif(n)  # random uniform numeric vector of length `n`
y <- runif(n)  # random uniform numeric vector of length `n`

system.time(z <- x + y)
```

The code semantics of the above are equivalent to (assuming equal length `x` and
`y` and length greater than zero):

```
z <- numeric(length(x))
system.time(
  for(i in seq_along(x)) {
    z[i] <- x[i] + y[i]
  }
)
```

The implicit loop syntax is nicer, and much faster as the loop is executed
directly in machine instructions without interpreter overhead:

    <bar plot>

Unfortunately there are tasks that require iterated explicit calls to R
functions that are saddled by the interpreter overhead.  A common one is to
compute group statistics, as in:

```
g <- sample(n/10, n, replace=TRUE)  # make groups with size ~10 elements
x.g <- split(x, g)                  # split x by group

system.time(vapply(x.g, var, 0))
##   user  system elapsed 
## 13.301   0.342  14.292 
``

In this case we'll be calling the `var` R function ~1 million times.  Other
problematic applications include rolling window statistics, explicit loops
(e.g. to re-use prior calculations), solvers, and more.

## The Trade Off

R is an incredibly flexible programming language, but if all we want to do is
compute statistics on numeric vectors, most of that flexibility is wasted on our
dull work.  Additionally, this flexibility makes it difficult to automatically
optimize R code into native instructions.

If we accept to work with only numeric vectors, arithmetic operators, basic
mathematical functions, and basic statistics, it becomes possible to
programatically translate R expressions into C code which can then be compiled
to native instructions.

While it may seem like the constraints is restrictive, we can do quite a lot
with even just a minimal set of functions.  For example, we can implement `var`
as:

```
sum((x - mean(x)) ^ 2) / (length(x) - 1)
```

## The Payoff

Speed, of course:

```
library(r2c)
r2c_var <- r2cq(sum((x - mean(x)) ^ 2) / (length(x) - 1))
system.time(group_exec(r2c_var, data.frame(x), g))
```


Compilation cost.

Most data analysis is carried out on numeric vectors, so the implicit syntax is
clearer and more convenient.  But more importantly for performance, the loop can
be implemented in statically compiled code, which makes it fast.

avoids interpreter overhead
for each invocation of `+` (as well as the two calls to `[` to get the scalar
value).



through
a combination of inte

R code using a small subset of R functions and operating exclusively on numeric
vectors can be converted to C and compiled to native instructions.  The standard
arithmetic operators and basic statistics are supported, and the user can
combine these to implement statics.

Once compiled, the code can be called repeatedly with varying data without R
interpreter overhead.  Currently `r2c` supports running the compiled code
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
`r2c` framework.  However, it is still necessary to implement them in C, so
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

More challenging due to code complexity, but otherwise compatible with `r2c`:

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
`r2c` it will be possible to compose complex statistics from the supported
functions, whereas gforce is limited to single function single symbol
expressions like `mean(x)`.

This also bears some resemblance to `{inline}` and `{Rcpp}` in as much as there
is a need to compile C code, but unlike those the user needs no experience
whatsoever with C or C++.

## Related Work

* Odin
* `data.table`'s Gforce.
* `dplyr`'s ?


