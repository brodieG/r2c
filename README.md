# r2c - Fast Iterated Statistics in R

**Proof of Concept**.  Lightly tested, experimental, and incomplete.  The
interface will change substantially over the next few iterations.

Compiles a selected subset of R into native instructions so that expressions
using only that subset can be executed repeatedly on varying data without
interpreter overhead.

## Background and Motivation

R is nearly as fast as statically compiled languages for many common numerical
calculations such as arithmetic on two large numeric vectors:

    set.seed(1)
    n <- 1e7
    x <- runif(n)
    y <- runif(n)

    system.time(z <- x + y)
    ##   user  system elapsed
    ##  0.023   0.000   0.023

Or statistics:

    system.time(sum(x))
    ##   user  system elapsed
    ##  0.043   0.000   0.043

On [my system](#notes-on-benchmarking) that's about 2-5 **CPU cycles** for each
of the 10 million operations and associated loop overhead.  Hard to get much
faster[^1].  If we maintain a high ratio of native operations to R level calls
our programs will be fast.  But some tasks require more R-level calls, such as
computing group statistics:

    g <- cumsum(sample(c(TRUE, rep(FALSE, 9)), n, replace=TRUE))
    x.split <- split(x, g)
    length(x.split)            # ~1MM groups
    ## [1] 1002099
    mean(lengths(x.split))     # ~10 average size
    ## [1] 9.98

    system.time(g.sum <- vapply(x.split, sum, 0))
    ##   user  system elapsed 
    ##  0.735   0.004   0.747 

Despite the same number of additions, our program slowed by ~20x.  And this
is with a primitive R function that does nothing but go directly to C code:

    sum
    ## function (..., na.rm = FALSE)  .Primitive("sum")

> Notice I don't time the split step.  This is so we can focus on the overhead
> of the statistic computation alone.

## What If We Could Compile R?

That would be nice, wouldn't it?  Well, we (at least I) can't compile the
entirety of R, but...

    library(r2c)
    r2c_sum <- r2cq(sum(x))
    ## clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -fno-common -std=c99 -pedantic -Wall -Wextra -c /var/folders/47/5by_gx_x4wncdt46pzcb9bcr0000gq/T//RtmpsSyRz0/file60911e4258d2/code-ceujp2sv.c -o /var/folders/47/5by_gx_x4wncdt46pzcb9bcr0000gq/T//RtmpsSyRz0/file60911e4258d2/code-ceujp2sv.o
    ## clang -mmacosx-version-min=10.13 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o /var/folders/47/5by_gx_x4wncdt46pzcb9bcr0000gq/T//RtmpsSyRz0/file60911e4258d2/code-ceujp2sv.so /var/folders/47/5by_gx_x4wncdt46pzcb9bcr0000gq/T//RtmpsSyRz0/file60911e4258d2/code-ceujp2sv.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation

And now:

    system.time(g.sum.r2c <- group_exec(r2c_sum, g, x, sorted=TRUE))
    ##   user  system elapsed 
    ##  0.103   0.000   0.105 
    length(g.sum.r2c)
    ## [1] 998608
    identical(g.sum, g.sum.r2c)
    ## [1] TRUE

<!--
library(ggplot2)
Method <- c('sum(x)', 'vapply(x.split, sum, 0)', 'r2c_sum')
dat <- data.frame(
  Method=factor(Method, Method),
  time=c(0.043, 0.747, 0.105)
)
p <- ggplot(dat) + geom_col(aes(Method, time)) +
  ggtitle("Sum Benchmarks") +
  ggtitle("Sum (10MM Obs, ~1MM Groups)") +
  ylab("Time in Seconds") + xlab(NULL)
ggsave("extra/time_sum_base-vs-r2c.png", p)
-->
![](https://github.com/brodieG/r2c/raw/initial/extra/time_sum_base-vs-r2c.png)

Not quite as fast as a simple sum, but there is overhead in computing each group
separately, and `r2c` minimizes that relative to `vapply`.

> Our `g` variable is pre-sorted so that we may keep the sorting overhead out of
> the benchmarks.  This is analogous to excluding the split step from the
> earlier timings.

## Are we Re-Inventing the Wheel?

If you're satisfied with simple expressions such as `sum(x)` then there are
alternatives.  In particular [`{data.table}`][1] with it's Gforce[^2]
optimization is a great option:

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
Method <- c('sum(x)', 'vapply(..., sum)', 'data.table', 'r2c', 'graal')
types <- c('Single Pass', 'Group Wise')
graal.sum.2 <- unique(cummin(graal.sum[['V3']]))
dat <- data.frame(
  Method=factor(Method, levels=Method),
  type=factor(c('Single Pass', rep('Group Wise', 4)), levels=types),
  time=c(0.043, 0.747, 0.234, 0.105, min(graal.sum.2))
)
dat.graal <- data.frame(
  Method=factor('graal', levels=Method),
  type=factor('Group Wise', levels=types),
  time=graal.sum.2
)
dat.both <- rbind(
  cbind(dat.graal, alpha=.2),
  cbind(dat, alpha=1)
)
dat.arrow <- data.frame(
  Method=factor('graal', levels=Method),
  time=rev(range(graal.sum.2)) + diff(range(graal.sum.2)) * c(-.01,.01),
  type=factor('Group Wise', levels=types)
)
(p <- ggplot(dat.both, aes(y=time)) +
  geom_col(aes(x=Method, alpha=I(alpha)), position='identity') +
  geom_line(
    aes(x=I(rep(3.65, 2))),
    data=dat.arrow, arrow=arrow(type='closed', length=unit(.1, "inches")),
  ) +
  geom_text(
    x=3.75, y=mean(range(graal.sum.2)),
    label=paste0(
      strwrap("GraalVM requires several iterations to \"warm\" up.", 16),
      collapse="\n"
    ),
    size=3,
    hjust=0
  ) +
  ggtitle("Group Sum (10MM Obs, ~1MM Groups)") +
  facet_grid(.~type, drop=TRUE, scales="free_x", space="free") +
  ylab("Time in Seconds") + xlab(NULL))
ggsave("extra/time_gsum_all-vs.png", p)
-->
![](https://github.com/brodieG/r2c/raw/initial/extra/time_gsum_all-vs.png)

[`{FastR}`][2] is intriguing as it accelerates all of R[^3], not just iterated
statistic computation. It requires a different runtime (i.e. you can't just run
your normal R installation) and has other trade-offs, including the warm-up
period depicted above and compatibility limitations.

But `data.table`'s Gforce does not work on complex expressions such as the slope
of a bivariate regression:

    slope <- \(x, y) sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2)

Here `data.table` falls back to normal R evaluation:

<!--
```
Method <- c('slope(x)', 'vapply(..., slope)', 'data.table', 'r2c', 'graal')
graal.slope.2 <- unique(cummin(graal.slope[['V3']]))
dat <- data.frame(
  Method=factor(Method, levels=Method),
  type=factor(c('Single Pass', rep('Group Wise', 4)), levels=types),
  time=c(0.250, 12.570 , 11.519, 0.284, min(graal.slope.2))
)
dat.graal <- data.frame(
  Method=factor('graal', levels=Method),
  type=factor('Group Wise', levels=types),
  time=graal.slope.2
)
dat.both <- rbind(
  cbind(dat.graal, alpha=.2),
  cbind(dat, alpha=1)
)
dat.arrow <- data.frame(
  Method=factor('graal', levels=Method),
  time=rev(range(graal.slope.2)) + diff(range(graal.slope.2)) * c(-.01,.01),
  type=factor('Group Wise', levels=types)
)
(p <- ggplot(dat.both, aes(y=time)) +
  geom_col(aes(x=Method, alpha=I(alpha)), position='identity') +
  geom_line(
    aes(x=I(rep(3.7, 2))),
    data=dat.arrow, arrow=arrow(type='closed', length=unit(.1, "inches")),
  ) +
  geom_text(
    x=3.75, y=mean(range(graal.slope.2)),
    label=paste0(
      strwrap("GraalVM requires several iterations to \"warm\" up.", 16),
      collapse="\n"
    ),
    size=3,
    hjust=0
  ) +
  ggtitle("Group Slope (10MM Obs, ~1MM Groups)") +
  facet_grid(.~type, drop=TRUE, scales="free_x", space="free") +
  ylab("Time in Seconds") + xlab(NULL))
ggsave("extra/time_glope_all-vs.png", p)
```
-->
![](https://github.com/brodieG/r2c/raw/initial/extra/time_gslope_all-vs.png)



`data.table` sees a ~8x slowdown, whereas `r2c` is essentially unaffected.  The
differences get starker as we increase the complexity of the calculation:


More complex expressions force `data.table` to fallback to standard R
evaluation, and in this case make it ~40x slower than `r2c`.  To summarize:


Nope, we're not re-inventing the wheel.

## Caveats - Of Course ...

First is that `r2c` requires compilation.  I have not included that step in
timings under the view that the compilation time will be amortized over many
calculations.  The facilities for this don't exist yet, but the plan is to to
have `r2c` maintain a local library of pre-compiled user-defined functions.

More importantly, we cannot compile and execute arbitrary R expressions:

* Only `r2c` implemented counterpart functions may be used (currently: basic
  arithmetic operators and `sum`/`mean`)
* Primary numeric inputs must be attribute-less (e.g. to avoid expectations of
  S3 method dispatch or attribute manipulation).
* Future `r2c` counterparts will be limited to functions that return
  attribute-less numeric vectors of constant size (e.g. `mean`), or of the size
  of one of their inputs (e.g. like `+`, or even `quantile`).

Within these constraints `r2c` is flexible.  For example, it is possible to have
arbitrary R objects for secondary parameters, as well as to reference
group-invariant data:

    w <- c(1, NA, 2, 3)
    u <- c(-1, 1, 0)
    h <- rep(1:2, each=2)

    r2c_fun <- r2cq(sum(x, na.rm=TRUE) * y)
    ## ... compilation output omitted ...
    group_exec(r2c_fun, groups=h, data=list(x=w), MoreArgs=list(y=u))
    ##  1  1  1  2  2  2
    ## -1  1  0 -5  5  0

Notice the `na.rm`, and that the `u` in `list(y=u)` is re-used in full for each
group setting the output size to 3.

## Future - Maybe?

In addition to cleaning up the existing code, there are many extensions that can
be built on this proof of concept.  Some are listed below.  How many I end up
working on will depend on some interaction of external and my own interest.

### More Functions

Functions that have direct analogues in C or are simple to code in C are the
best candidates, subject to the previously described restrictions.  Thus the
following should be straightforward to implement:

* `abs`, unary `+` and `-`.
* `min`, `max`, `first`, `last`.
* `cos`, `sin`, and other trigonometric functions.
* `range`.
* `length`, `seq_along`.
* `[[`, `[`, and maybe `$`, likely with limitations on allowable index values.
* Many others.

More challenging due to code complexity, but otherwise compatible with `r2c`:

* `quantile`.
* And others.

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

There are likely many applications that would benefit from the capabilities
provided by `r2c`.  It should be possible to define an interface for use by
external code.

Additionally, it should be possible to allow users to define their own C
routines that integrated into the `r2c` framework.

### Re-using Compilation / Cleanup

Ideally once an expression is compiled into an `r2c` function it would be
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

And likely more.  So far the focus has been on implementation rather than
optimization.

## Related Work

* [`{Odin}`](https://github.com/mrc-ide/odin), which implements a very similar R
  to C translation and compilation, but specialized for differential
  equation solving problems.
* `data.table`'s Gforce (see `?data.table::datatable.optimize).
* `dplyr`'s Hybrid Eval?

## Notes on Benchmarking

Benchmarks are under:

    R version 4.2.0 (2022-04-22)
    Platform: x86_64-apple-darwin17.0 (64-bit)
    Running under: macOS Big Sur/Monterey 10.16

On an Intel(R) Core(TM) m5-6Y54 CPU @ 1.20GHz (early 2016 Macbook), using the
average of 11 iterations run after one `gc()` call.

    mean <- mean.default   # Avoid S3 dispatch for data.table
    system.time(
      g.slope.dt <- dt[,
        sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2), g
      ][['V1']]
    )
    ##   user  system elapsed 
    ## 11.397   0.062  11.519 

    r2c_slope <- r2cq(sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2))
    ## ... compilation output omitted ...
    system.time(g.slope.r2c <- group_exec(r2c_slope, g, list(x, y), sorted=TRUE))
    ##   user  system elapsed 
    ##  0.279   0.002   0.284 
    identical(unname(g.slope.r2c), g.slope.dt)
    ## [1] TRUE

For reference, with `base`:

    y.split <- split(y, g)
    system.time(
      g.slope.base <- mapply(
        function(x, y)
          sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2),
          x.split, y.split
    ) )
    ##   user  system elapsed 
    ## 11.895   0.079  12.168 
    identical(g.slope.r2c, g.slope.base)
    ## [1] TRUE

    library(data.table)            # 1.14.0
    setDTthreads(threads = 1)      # for apples-to-apples
    dt <- data.table(x, g)
    setkey(dt, g)
    system.time(g.sum.dt <- dt[, sum(x), g][['V1']])
    ##   user  system elapsed
    ##  0.205   0.027   0.234
    identical(unname(g.sum.r2c), g.sum.dt)
    ## [1] TRUE

This is in the same ballpark as `r2c`.  But look at what happens if we
complicate things by computing `sum(x + y)` instead of `sum(x)`:

    dt <- data.table(x, y, g)
    setkey(dt, g)
    system.time(g.sum.xy.dt <- dt[, sum(x + y), g][['V1']])
    ##   user  system elapsed 
    ##  1.645   0.015   1.672 

    r2c_sum2 <- r2cq(sum(x + y))
    system.time(g.sum.xy.r2c <- group_exec(r2c_sum2, g, list(x, y), sorted=TRUE))
    ##   user  system elapsed 
    ##  0.118   0.001   0.121 

    identical(unname(g.sum.xy.r2c), g.sum.xy.dt)
    ## [1] TRUE


[1]: https://github.com/Rdatatable
[2]: https://github.com/oracle/fastr
[3]: 

[^1]: Depending on your compilation settings, there is likely some room for
  improvement, but not enough that R stands out as being particularly slow at
  this task.
[^2]: Gforce is available for simple expressions of the form `fun(var)` for
  many of the basic statistic functions (see `?data.table::datatable.optimize).
[^3]: I guess it is possible to "compile" the whole/most of R.
