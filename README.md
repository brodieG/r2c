# r2c - Fast Iterated Statistic Computation in R

**Proof of Concept**.  Experimental, incomplete, with an interface subject to
change.

Compiles a subset of R into machine code so that expressions composed with that
subset can be applied repeatedly on varying data without interpreter overhead.

## Background and Motivation

R is nearly as fast as statically compiled languages for many common numerical
calculations:

    set.seed(1)
    n <- 1e7
    x <- runif(n)
    y <- runif(n)

    system.time(x + y)
    ##   user  system elapsed
    ##  0.023   0.000   0.023

    system.time(sum(x))
    ##   user  system elapsed
    ##  0.043   0.000   0.043

On [my system](#notes-on-benchmarking) that's about 2-5 **CPU cycles** for each
of the 10 million operations and associated loop overhead.  Hard to get much
faster[^1].  If we maintain a high ratio of native operations to R level calls
our programs will be fast.

Some tasks require more R-level calls, such as computing group statistics[^7]:
<span id=in-r>

    g <- cumsum(sample(c(TRUE, rep(FALSE, 9)), n, replace=TRUE))
    x.split <- split(x, g)
    length(x.split)            # ~1MM groups
    ## [1] 1002099
    mean(lengths(x.split))     # ~10 average size
    ## [1] 9.98

    system.time(g.sum <- vapply(x.split, sum, 0))
    ##   user  system elapsed 
    ##  0.735   0.004   0.747 

Despite the same number of additions, our program **slowed by ~20x**.  And this
is with a primitive R function that does nothing but go directly to C code:

    sum
    ## function (..., na.rm = FALSE)  .Primitive("sum")

## What If We Could Compile R?

That would be nice, wouldn't it?  Well, we (at least I) can't compile the
entirety of R, but a small set we can manage:

    library(r2c)
    r2c_sum <- r2cq(sum(x))

And now:

    system.time(g.sum.r2c <- group_exec(r2c_sum, g, x, sorted=TRUE))
    ##   user  system elapsed 
    ##  0.103   0.000   0.105 
    length(g.sum.r2c)
    ## [1] 998608
    identical(g.sum, g.sum.r2c)
    ## [1] TRUE

Nearly as fast as the simple sum despite the additional overhead of managing the
groups and the larger result.

## Are we Re-Inventing the Wheel?

If you're satisfied with simple expressions such as `sum(x)` then there are
alternatives.  In particular [`{data.table}`][1] with it's Gforce[^2]
optimization is a great option:

    library(data.table)            # 1.14.0
    setDTthreads(threads = 1)      # single thread for apples-to-apples
    dt <- data.table(x, g)
    setkey(dt, g)                  # pre-sort data

    system.time(g.sum.dt <- dt[, sum(x), g])
    ##   user  system elapsed
    ##  0.205   0.027   0.234
    identical(unname(g.sum), g.sum.dt[['V1']])
    ## [1] TRUE

This does something similar to `{r2c}`, recognizing the sum expression and using
native code instead of R interpreted code, but without the need for ad hoc
compilation.

[`{collapse}`][4], takes a different approach by providing specialized functions
to compute statistics on groups, and this allows it to be very fast:

    library(collapse)              # 1.8.6
    cg <- GRP(g)                   # pre-sort data

    system.time(g.sum.c <- fsum(x, cg, na.rm=FALSE))
    ##   user  system elapsed
    ##  0.027   0.000   0.027
    all.equal(g.sum.r2c, g.sum.c)
    ## [1] TRUE

Another intriguing option is [`{FastR}`][2], an implementation of R that can JIT
compile R code to run on the [Graal VM][3].  It requires a different runtime
(i.e. you can't just run your normal R installation) and has other trade-offs,
including warm-up cycles and compatibility limitations[^3].  But otherwise you
type in what [you would have in normal R](#in-r) and see some impressive
speed-ups.

This is what the timings look across the different methods discussed so far:

<!--
graal.slope <- read.table(
  text=grep(
    "\\d+\\.\\d+", readLines('extra/benchmarks-graal-mapply.txt'),
    value=TRUE
) )
graal.sum <- read.table(
  text=grep(
    "\\d+\\.\\d+", readLines('extra/benchmarks-graal-sum.txt'),
    value=TRUE
) )
Method <- c('sum(x)', 'vapply(..., sum)', 'data.table', 'collapse', 'r2c',
'FastR')
types <- c('Single Pass', 'Group Wise')
graal.sum.2 <- unique(cummin(graal.sum[['V3']]))
dat <- data.frame(
  Method=factor(Method, levels=Method),
  type=factor(c('Single Pass', rep('Group Wise', 5)), levels=types),
  time=c(0.043, 0.747, 0.234, 0.027, 0.105, min(graal.sum.2))
)
dat.graal <- data.frame(
  Method=factor('FastR', levels=Method),
  type=factor('Group Wise', levels=types),
  time=graal.sum.2
)
dat.both <- rbind(
  cbind(dat.graal, alpha=.2),
  cbind(dat, alpha=1)
)
dat.arrow <- data.frame(
  Method=factor('FastR', levels=Method),
  time=rev(range(graal.sum.2)) + diff(range(graal.sum.2)) * c(-.01,.01),
  type=factor('Group Wise', levels=types)
)
(p <- ggplot(dat.both, aes(y=time)) +
  geom_col(aes(x=Method, alpha=I(alpha)), position='identity') +
  geom_line(
    aes(x=I(rep(4.65, 2))),
    data=dat.arrow, arrow=arrow(type='closed', length=unit(.1, "inches")),
  ) +
  geom_text(
    x=4.75, y=mean(range(graal.sum.2)),
    label=paste0(
      strwrap("GraalVM requires several iterations to \"warm\" up.", 16),
      collapse="\n"
    ),
    size=3,
    hjust=0
  ) +
  geom_text(data=dat, aes(x=Method, label=time), vjust=-.2) +
  ggtitle("Group Sum (10MM Obs, ~1MM Groups)") +
  facet_grid(.~type, drop=TRUE, scales="free_x", space="free") +
  ylab("Time in Seconds") + xlab(NULL))
ggsave("extra/time_gsum_all-vs.png", p)
-->
![](https://github.com/brodieG/r2c/raw/initial/extra/time_gsum_all-vs.png)

For this specific task `{collapse}` makes an impressive case for itself[^4].
Both `{r2c}` and `{data.table}` bear the overhead of applying arbitrary
C functions groupwise[^5], and this is part of the reason they are slower for
this task.  Additionally, `{data.table}` has extra structure for multi-threading
support that might add overhead, so it is at a disadvantage when we force single
thread benchmarking on small groups.

Let's look at what happens with more complex expressions, such as the slope
of a bivariate regression:

    r2c_slope <- r2cq(sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2))
    system.time(g.slp.r2c <- group_exec(r2c_slope, g, list(x, y), sorted=TRUE))
    ##   user  system elapsed 
    ##  0.273   0.001   0.275 

`{r2c}` performance scales with the complexity of the operation, as one might
expect.  But not so with `{data.table}`'s Gforce:

    dt <- data.table(x, y, g)
    setkey(dt, g)                  # pre-sort data
    mean <- mean.default           # Avoid S3 dispatch
    system.time(g.slope.dt <- dt[, sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2), g])
    ##   user  system elapsed 
    ## 11.397   0.062  11.519 
    identical(unname(g.slp.r2c), g.slope.dt[['V1']])
    ## [1] TRUE

We can infer comparing with base timings that `{data.table}` falls back to
standard R evaluation[^2]:

    y.split <- split(y, g)
    slope <- \(x, y) sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2)
    system.time(g.slp <- mapply(slope, x.split, y.split))
    ##   user  system elapsed 
    ## 12.719   0.193  13.408 
    identical(g.slp, g.slp.r2c)
    ## [1] TRUE

`{collapse}` does better if we carefully compose the equivalent expression,
but it is slowed down by the need to re-expand intermediate statistics back to
group size.  We do that here with `rep` in `fsum2` and `fmean2`:

    fmean2 <- function(x, cg) rep(fmean(x, cg, na.rm=FALSE), cg[['group.sizes']])
    fsum2 <- function(x, cg) rep(fsum(x, cg, na.rm=FALSE), cg[['group.sizes']])
    system.time(
      g.slp.c <-
        fsum((x - fmean2(x, cg)) * (y - fmean2(y, cg)), cg, na.rm=FALSE) /
        fsum((x - fmean2(x, cg))^2, cg, na.rm=FALSE)
    )
    ##   user  system elapsed 
    ##  1.737   0.035   1.776 

`{FastR}` also does reasonably well, but warm-up cycles remain an issue, and
even after them it does not keep up with `{r2c}`.

<!--
```
Method <- c(
'slope(x)', 'vapply(..., slope)', 'data.table', 'collapse', 'r2c', 'FastR'
)
graal.slope.2 <- unique(cummin(graal.slope[['V3']]))
dat <- data.frame(
  Method=factor(Method, levels=Method),
  type=factor(c('Single Pass', rep('Group Wise', 5)), levels=types),
  time=c(0.250, 12.570 , 11.519, 1.776, 0.284, min(graal.slope.2))
)
dat.graal <- data.frame(
  Method=factor('FastR', levels=Method),
  type=factor('Group Wise', levels=types),
  time=graal.slope.2
)
dat.both <- rbind(
  cbind(dat.graal, alpha=.2),
  cbind(dat, alpha=1)
)
dat.arrow <- data.frame(
  Method=factor('FastR', levels=Method),
  time=rev(range(graal.slope.2)) + diff(range(graal.slope.2)) * c(-.01,.01),
  type=factor('Group Wise', levels=types)
)
(p <- ggplot(dat.both, aes(y=time)) +
  geom_col(aes(x=Method, alpha=I(alpha)), position='identity') +
  geom_line(
    aes(x=I(rep(4.7, 2))),
    data=dat.arrow, arrow=arrow(type='closed', length=unit(.1, "inches")),
  ) +
  geom_text(
    x=4.75, y=mean(range(graal.slope.2)),
    label=paste0(
      strwrap("GraalVM requires several iterations to \"warm\" up.", 16),
      collapse="\n"
    ),
    size=3,
    hjust=0
  ) +
  geom_text(data=dat, aes(x=Method, label=round(time, 2)), vjust=-.2) +
  ggtitle("Group Slope (10MM Obs, ~1MM Groups)") +
  facet_grid(.~type, drop=TRUE, scales="free_x", space="free") +
  ylab("Time in Seconds") + xlab(NULL))
ggsave("extra/time_glope_all-vs.png", p)
```
-->
![](https://github.com/brodieG/r2c/raw/initial/extra/time_glope_all-vs.png)

To summarize:

> `{r2c}` allows you to create complex statistics from simple building blocks
> with R syntax and semantics, and to iterate them as fast as if you had written
> them in a statically compiled language yourself.

## Interlude - On Sorting

Note our groups are purposefully pre-sorted:

    head(g, 20)
    ## [1] 0 0 1 1 1 1 1 2 2 2 2 2 2 2 3 3 3 4 4 4

And for the base examples we do not time the splitting step.

This is to keep the focus benchmarks on the statistic computation.  When
computing simple group statistics like `sum`, the sorting / splitting step can
be slower than the statistic computation.

Fast group statistic computation requires fast sorting to organize data into
groups.  This is possible in R thanks to `{data.table}` contributing their fast
radix sort starting with R 3.3.0.  Without this contribution `{r2c}` and other
implementations would be uncompetitive.

## Caveats - Of Course ...

First is that `r2c` requires compilation.  I have not included that step in
timings[^6] under the view that the compilation time will be amortized over many
calculations.  The facilities for this don't exist yet, but the plan is to to
have `{r2c}` maintain a local library of pre-compiled user-defined functions,
and for packages to compile `{r2c}` functions at install-time.

More importantly, we cannot compile and execute arbitrary R expressions:

* Only `{r2c}` implemented counterpart functions may be used (currently: basic
  arithmetic operators and `sum`/`mean`)
* Primary numeric inputs must be attribute-less (e.g. to avoid expectations of
  S3 method dispatch or attribute manipulation).
* Future `{r2c}` counterparts will be limited to functions that return
  attribute-less numeric vectors of constant size (e.g. `mean`), or of the size
  of one of their inputs (e.g. like `+`, or even `quantile`).

Within these constraints `r2c` is flexible.  For example, it is possible to have
arbitrary R objects for secondary parameters, as well as to reference
group-invariant data:

    w <- c(1, NA, 2, 3)
    u <- c(-1, 1, 0)
    h <- rep(1:2, each=2)

    r2c_fun <- r2cq(sum(x, na.rm=TRUE) * y)
    group_exec(r2c_fun, groups=h, data=list(x=w), MoreArgs=list(y=u))
    ##  1  1  1  2  2  2
    ## -1  1  0 -5  5  0

Notice the `na.rm`, and that the `u` in `list(y=u)` is re-used in full for each
group setting the output size to 3.

The C counterparts to the R functions are intended to produce identical outputs,
but have different implementations.  As such, it is possible that for a
particular set of inputs on a particular platform the results might diverge.

## Future - Maybe?

In addition to cleaning up the existing code, there are many extensions that can
be built on this proof of concept.  Some are listed below.  How many I end up
working on will depend on some interaction of external and my own interest.

### Better Grouping Semantics

Implement multi-column grouping and non-integer grouping columns.

### More Functions

Functions that have direct analogues in C or are simple to code in C are the
best candidates, subject to the previously described restrictions.  Thus the
following should be straightforward to implement:

* `abs`, unary `+` and `-`.
* `min`, `max`, `first`, `last`.
* `cos`, `sin`, and other trigonometric functions.
* `range`.
* `length`, `seq_along`.
* `[[`, `[`, and maybe `$`, likely with limitations on allowable index types.
* Many others.

More challenging due to code complexity, but otherwise compatible with `{r2c}`:

* `quantile`, and others.

Some other useful functions will require more work:

* `diff`, because the result size of `n - 1` is not currently supported.

Functions that will likely not be implementable:

* `seq`, except perhaps for narrow cases where the parameters are constants or
  perhaps select expressions such as `length(n)`, but even this becomes
  complicated.
* And many more.

### Simple Assignment and Multi-Line Statements

While non-trivial, the existing structure should allow explicit intermediate
variables and multi-call expressions.  Both of these already exist implicitly as
part of the call processing logic.

### Other Repetition Structures

#### Window Functions

It will be straightforward to implement a runner that invokes the compiled code
on a sliding window instead of on groups.  The main complication is accounting
for incomplete windows at the beginning and end of the data.

#### Loops

More complex but in theory possible are loops that reference read/write vectors
with subsetting and subset assignment, thus allowing results of previous loop
iterations to be re-used in later ones.

#### Solvers

It should be possible to build a solver around `r2c` compiled expressions, but
there already exist similar implementations.  In particular Rich FitzJohn's
[`{Odin}`](https://github.com/mrc-ide/odin) uses a very similar approach to
`r2c` to generate C routines for use with `deSolve`.

#### API

There are likely many applications that could benefit from the capabilities
provided by `{r2c}`.  It should be possible to define an interface for use by
external code.  Conceivably, `{data.table}` could be extended to run `{r2c}`
compiled expressions.

Additionally, it should be possible to allow users to define their own C
routines that integrated into the `{r2c}` framework.

### Re-using Compilation / Cleanup

Ideally once an expression is compiled into an `{r2c}` function it would be
preserved for re-use in future R sessions.  Doing so within a package would be
relatively straight-forward, but it should also be possible to create a local
library to store such objects in.

We'll also need to ensure that the methods we use to access the compiled
instructions are legal, as what we do now is slightly questionable.  More
generally, the C internals have been implemented with the sole priority of
producing a proof of concept rather than robust extensibility, and will need
cleanup.

### Optimizations

* More aggressive re-use of intermediate memory.
* Identification of re-used calculations.
* Reduce per-group/iteration overhead.

And likely more.  So far the focus has been on implementation rather than
optimization.

## Related Work

* [`{Odin}`](https://github.com/mrc-ide/odin), which implements a very similar R
  to C translation and compilation, but specialized for differential
  equation solving problems.
* [`{data.table}`][1]'s Gforce (see `?data.table::datatable.optimize).
* [`{FastR}`][2] an implementation of R that can JIT compile R code to run on
  the [Graal VM][3].
* [`{collapse}`][4]'s specialized group statistic functions.
* [`{inline}`][7] to allow compilation and access to generated native code
  directly from R.
* In theory [`{dplyr}`][5]'s Hybrid Eval is similar to Gforce, but AFAICT it was
  [quietly dropped][6] and despite suggestions it might return for v1.1 I see no
  trace of it in the most recent 1.1 candidate development versions (as of
  2022-07-03).

## Notes on Benchmarking

Benchmarks are under:

    R version 4.2.0 (2022-04-22)
    Platform: x86_64-apple-darwin17.0 (64-bit)
    Running under: macOS Big Sur/Monterey 10.16

On an Intel(R) Core(TM) m5-6Y54 CPU @ 1.20GHz (early 2016 Macbook), using the
average of 11 iterations run after one `gc()` call, and -O2 optimization level.
Different systems / compilers / settings may produce different results.

[1]: https://github.com/Rdatatable
[2]: https://github.com/oracle/fastr
[3]: https://www.graalvm.org/
[4]: https://github.com/SebKrantz/collapse
[5]: https://dplyr.tidyverse.org/
[6]: https://github.com/tidyverse/dplyr/issues/5017
[7]: https://github.com/eddelbuettel/inline
[8]: https://twitter.com/BrodieGaslam/status/1527829442374025219?s=20&t=rg6aybJlGxPEUwBsI0ii1Q

[^1]: Depending on your compilation settings and machine, there is room for
  improvement, but not enough that R stands out as being particularly slow at
  this task.
[^2]: Gforce is available for simple expressions of the form `fun(var)` for
  many of the basic statistic functions (see `?data.table::datatable.optimize).
[^3]: My limited experience with {`FastR`}is that it is astonishing, but also
  frustrating.  What it does is amazing, but the compatibility limitations are
  real (e.g.  with the current version neither {`data.table`} nor {`ggplot2`}
  install out of the box, and more), and performance is volatile (e.g. package
  installation and some other tasks are painfully slow, some expressions will
  hiccup after the initial warm-up).  At this point it does not seem like a
  viable drop-in replacement to R.  It likely excels at running scalar
  operations in loops and similar, something that R itself struggles at.
[^4]: `{collapse}` defaults to `na.rm=TRUE`, hence the need to switch for an
  apples to apples comparison.  Notice that `fsum` with groups is faster than
  even the straight up `sum` without groups, primarily because it handles the
  `narm` as a dedicated branch instead of a conditional in the loop (this is an
  [oddity with `sum`[8] on some platforms).  `fsum` also uses a plain double
  accumulator and not the long double used by the other implementations so the
  results are not identical to the other implementations that use long doubles
  (on systems that support them).  Curiously on my system summing long doubles
  is faster than summing doubles.
[^5]: Knowing that a single operation is to be applied over a single vector
  provides nice optimization opportunities.  Notwithstanding, `{r2c}` likely
  could be optimized further as the primary focus has been simply to get it
  working, but optimizations will be offset by the need to add interrupts and
  similar which are currently omitted.
[^6]: The first compilation can be quite slow as it requires loading the
  compiler, etc.  Subsequent compilations run in the tenths of seconds.
[^7]: For this very specific task R also provides `rowsum`, but as it is limited
  to sums and we cannot separate the splitting and summing steps for timing we
  will not discuss it further.

