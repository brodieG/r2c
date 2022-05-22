# r2c - Fast Iterated Statistics in R

**Proof of Concept**.  Lightly tested, experimental, and incomplete.  The
interface will change substantially over the next few iterations.

Compiles a selected subset of R into native instructions so that
that expressions composed from that subset can be executed repeatedly on
varying data without interpreter overhead.

## Background and Motivation

R is nearly as fast as statically compiled languages for many common numerical
calculations such as arithmetic on two large numeric vectors:

```
set.seed(1)
n <- 1e7
x <- runif(n)
y <- runif(n)

system.time(z <- x + y)
##   user  system elapsed
##  0.023   0.000   0.023
```

Or statistics:

```
system.time(sum(x))
sys.time(sum(x))
##   user  system elapsed
##  0.043   0.000   0.043
```

On my system that's about 2-5 **CPU cycles** for each of the 10 million
operations and associated loop overhead.  Hard to get much faster.  If we
maintain a high ratio of native operations to R level calls our programs will be
fast.  But some tasks require more R-level calls, for example computing
statistics on groups:

```
g <- cumsum(sample(c(TRUE, rep(FALSE, 9)), n, replace=TRUE))
x.split <- split(x, g)
length(x.split)            # ~1MM groups
## [1] 1002099
mean(lengths(x.split))     # ~10 average size
## [1] 9.98

system.time(g.mean <- vapply(x.split, sum, numeric(1L)))
##   user  system elapsed 
##  0.802   0.014   0.856 
```

Despite the same number of additions, our program slowed by over 20x.  And this
is with a primitive R function that does nothing but go directly to C code:

```
sum
## function (..., na.rm = FALSE)  .Primitive("sum")
```

The result is larger in the group case, but allocating and writing that is fast
as shown by the earlier example with `z <- x + y` which is ~10x the size.

## What If We Could Compile R?

That would be nice, wouldn't it?  Well, we (at least I) can't compile the
entirety of R, but...

```
library(r2c)
r2c_mean <- r2cq(mean(x))
## clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -fno-common -std=c99 -pedantic -Wall -Wextra -c /var/folders/47/5by_gx_x4wncdt46pzcb9bcr0000gq/T//RtmpsSyRz0/file60911e4258d2/code-ceujp2sv.c -o /var/folders/47/5by_gx_x4wncdt46pzcb9bcr0000gq/T//RtmpsSyRz0/file60911e4258d2/code-ceujp2sv.o
## clang -mmacosx-version-min=10.13 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o /var/folders/47/5by_gx_x4wncdt46pzcb9bcr0000gq/T//RtmpsSyRz0/file60911e4258d2/code-ceujp2sv.so /var/folders/47/5by_gx_x4wncdt46pzcb9bcr0000gq/T//RtmpsSyRz0/file60911e4258d2/code-ceujp2sv.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
```

And now:

```
set.seed(1)
n <- 1e5
x <- runif(n)
g <- cumsum(sample(c(TRUE, rep(FALSE, 9)), n, replace=TRUE))
library(r2c)
r2c_mean <- r2cq(mean(x))

system.time(g.mean2 <- group_exec(r2c_mean, g, x, sorted=TRUE))
##   user  system elapsed 
##  0.132   0.001   0.133 
identical(g.mean, g.mean2)
## [1] TRUE

r2c_sum <- r2cq(sum(x))
sys.time(g.sum2 <- group_exec(r2c_sum, x, g, sort=FALSE))

```

```
library(data.table)
setDTthreads(threads = 1)
dt <- data.table(x, g)
setkey(dt, g)
system.time(dt[, mean(x), g])
```

## Constraints

R is an incredibly flexible programming language, but if all we want to do is
compute statistics on numeric vectors, most of that flexibility is wasted on our
dull work.  Additionally, this flexibility makes it difficult to automatically
optimize R code into native instructions.

If we accept to work with only numeric vectors, basic arithmetic operators and
statistics, and forsake method dispatch,  it becomes possible to
programmatically translate R expressions into C code.  The constraints are
restrictive, but we can do a lot under them.  For example, we can implement
`var` (single variable) as:

```
sum((x - mean(x)) ^ 2) / (length(x) - 1)
```

## Performance

Speed, of course:

```
library(r2c)
o <- order(g)
xo <- x[o]
go <- g[o]
system.time(r2c_var <- r2cq(sum((x - mean(x)) ^ 2) / (length(x) - 1)))
r2c_mean <- r2cq(mean(x))

system.time(v2 <- group_exec(r2c_var, data.frame(x=xo), go, sort=FALSE))
system.time(m1 <- vapply(x.g, mean.default, 0))
system.time(m2 <- group_exec(r2c_mean, data.frame(x=xo), go, sort=FALSE))
identical(unname(v1), v2)
all.equal(unname(v1), v2)
```

Vs. `data.table`

## Caveats


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

## 

set.seed(1)
x <- runif(1e7)
microbenchmark::microbenchmark(
  mean(x) * length(x),
  sum(x)
)

