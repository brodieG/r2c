# r2c - Fast Iterated Statistic Computation in R

**Proof of Concept**.  Experimental, incomplete, with an interface subject to
change.

"Compiles" a subset of R into machine code so that expressions composed with
that subset can be applied repeatedly on varying data without interpreter
overhead.  `{r2c}` provides speed ups of up to 100x for iterated statistics,
with R semantics, and without the challenges of directly compilable languages.

## "Compiling" R

Currently `{r2c}` can "compile" R expressions composed of basic binary operators
and statistics.  "Compile" is in quotes because `{r2c}` generates an equivalent
C program, and compiles that.  To compute the slope of a bivariate regression we
might use:

    library(r2c)
    r2c_slope <- r2cq(
      sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
    )
    (slope.r2c <- with(iris, r2c_slope(Sepal.Width, Sepal.Length)))

This is equivalent to:

    slope <- function(x, y)
      sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
    (slope.base <- with(iris, slope(Sepal.Width, Sepal.Length)))
    identical(slope.r2c, slope.base)

`{r2c}` is able to do this by focusing on a limited set of functions and the
special case of attribute-less numeric vectors (see [Caveats](#caveats)).

## Iterating `{r2c}` Functions

The primary use case of `{r2c}` functions is iteration. `{r2c}` is fast
because it avoids the R interpreter overhead otherwise required for each
iteration.  There are currently two iteration mechanisms available:

* `group_exec`: compute on disjoint groups in data (a.k.a. split-apply-combine).
* `roll*_exec`: compute on (possibly) overlapping sequential windows in data.

I have not found good alternatives for the general use case of `{r2c}`, as can
be seen from the timings of computing group and window slopes[^14]:

    <group: plot of 1e6 groups of 10 and 1e6 windows of 10>
    Need somewhwere to show the reference code
    Windows:
    * vapply
    * zoo::rollapply
    * data.table::fapply
    * slider::

`{collapse}` stands out as a possible exception in the grouped statistics case,
although one must be familiar enough with its semantics to translate a regular R
expression to one that will be fast in `{collapse}`.  `{fastr}` is also
interesting, but has other drawbacks.

For the special case of a simple statistic many packages provide dedicated
pre-compiled alternatives, some of which are faster than {`r2c`}:

    <sum: plot of 1e6 groups of 10 and 1e6 windows of 10>

But these are all limited to a simple pre-defined set of statistics.

To summarize:

> `{r2c}` is fastest at calculating complex expressions that are iterated
> repeatedly, while also retaining base R semantics for numeric inputs.

See the [Related Work](#related-work) and [Code and notes on
benchmarking](#notes-on-benchmarking) sections.

## Caveats

First is that `r2c` requires compilation.  I have not included that step in
timings[^6] under the view that the compilation time will be amortized over many
calculations.  The facilities for this don't exist yet, but the plan is to to
have `{r2c}` maintain a local library of pre-compiled user-defined functions,
and for packages to compile `{r2c}` functions at install-time.

More importantly, we cannot compile and execute arbitrary R expressions:

* Only `{r2c}` implemented counterpart functions may be used (currently: basic
  arithmetic operators and `sum`/`mean`/`length`)
* Primary numeric inputs must be attribute-less (e.g. to avoid expectations of
  S3 method dispatch or attribute manipulation), and any `.numeric` methods
  defined will be ignored[^10].
* Future `{r2c}` counterparts will be limited to functions that return
  attribute-less numeric vectors of constant size (e.g. `mean`), or of the size
  of one of their inputs (e.g. like `+`, or even `quantile`).

Within these constraints `r2c` is flexible.  For example, it is possible to have
arbitrary R objects for secondary parameters, as well as to reference
iteration-invariant data:

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
working on will depend on some interaction of external interest and my own.

* Expand the set of R functions that can be translated.
* Additional runners (e.g. an `apply` analogue).
* Optimizations (identify repeated calculations, re-use memory more
  aggressively).
* Preserve previously "compiled" functions.
* Assignment operator (`<-`).
* Multi-line expressions.
* Basic loop support, and maybe logicals and branches.
* Get on CRAN (there is currently at least one questionable thing we do).
* API to allow other native code to invoke `{r2c}` functions.

## Related Work

### "Compiling" R

[`FastR`][2] an implementation of R that can JIT compile R code to run on the
[Graal VM][3].  It requires a different runtime (i.e. you can't just run your
normal R installation) and has other trade-offs, including warm-up cycles and
compatibility limitations[^3].  But otherwise you type in what you would have
in normal R and see some impressive speed-ups.

[The Ř virtual machine][19] an academic project that is superficially similar to
`FastR` (it's [thesis][20] explains differences).  Additionally
[`renjin`][18] appears to offer similar capabilities and tradeoffs as
`FastR`.  I have tried neither `Ř` nor `renjin`.

Closer to `{r2c}`, there are at least four packages that operate on the
principle of translating R code into C (or C++), compiling that, and providing
access to the resulting native code from R:

* [`{Odin}`](https://github.com/mrc-ide/odin), specialized for differential
  equation solving problems.
* [`{ast2ast}`](https://github.com/Konrad1991/ast2ast/), also targeting ODE
  solving and optimization.
* [`{armacmp}`](https://github.com/dirkschumacher/armacmp), a DSL for
  formulating linear algebra code in R that is translated into C++.
* [`{nCompiler}`](https://github.com/nimble-dev/nCompiler), a tool for
  generating C++ and interfacing it with R.

Most of these seem capable of computing iterated statistics in some form, and
experienced users can likely achieve it with some work, but it will be difficult
for someone familiar only with R.

Finally, [`{inline}`][7] and [`{Rcpp}`[16] allow you to write code in C/C++ and
easily interface it with R.

### Fast Group and Rolling Statistics

I do not know of any packages that compile R expressions to avoid interpreter
overhead in applying them over groups or windows of data.  The closest is
packages that recognize expressions they have equivalent pre-compiled code.
This is limited to single simple statistics:

* [`{data.table}`][1]'s Gforce (see `?data.table::datatable.optimize`).
* In theory [`{dplyr}`][5]'s Hybrid Eval is similar to Gforce, but AFAICT it was
  [quietly dropped][6] and despite suggestions it might return for v1.1 I see no
  trace of it in the most recent 1.1 candidate development versions (as of
  2022-07-03).

Additionally, there is [`{collapse}`][4] which provides specialized group
statistic functions.  These are quite fast, particularly for simple statistics,
but you have to be familiar with `{collapse}` semantics to compose complex
statistics from simple ones.

Several packages provide fast dedicated functions for a small set of simple
rolling window statistics:

* `base::filter` for weighted rolling sums / means.
* [`{data.table}`][1]'s `froll*` functions.
* [`{slider}`][14] `slide_<stat>` and `slide_index_<stat>`.
* [`{roll}`][22], with a good description of the "on-line" algorithm in the
  README.
* [`{zoo}`][12] `roll<stat>`.
* [`{RcppRoll}`][].
* [`{runner}`][].

`{data.table}`, `{roll}` and `{slider}` distinguish themselves with algorithms
that avoid recomputing overlapping window sections.  `{data.table}` and `{roll}`
uses the "on-line" algorithm (see the `{roll}` README for an explanation) and
`{slider}` the "segment tree" algorithm.  The "on-line" algorithm is fastest,
but theoretically more susceptible to numerical precision issues[^13].  For
larger windows, these implementations will be much faster than `{r2c}` which
naively recomputes each window in full.

## Acknowledgments

* R Core for developing and maintaining such a wonderful language.
* [Matt Dowle](https://github.com/mattdowle) and [Arun
  Srinivasan](https://github.com/arunsrinivasan) for contributing the
  `{data.table}`'s radix sort to R.
* [Sebastian Krantz](https://github.com/SebKrantz) for the idea of pre-computing
  group meta data for possible re-use (taken from `collapse::GRP`).
* [Achim Zeileis][11] et al. for `rollapply` in [`{zoo}`][12] from the design of
  which `window_exec` borrows elements.
* [David Vaughan][13] for ideas on window functions, including the index concept
  used in `window_i_exec`, borrowed from [`{slider}`][14].
* Byron Ellis and [Peter Danenberg](https://github.com/klutometis) for the
  inspiration behind `lcurry` (see [`functional::CurryL`][15]), used in tests.
* [Hadley Wickham](https://github.com/hadley/) and [Peter
  Danenberg](https://github.com/klutometis) for
  [roxygen2](https://cran.r-project.org/package=roxygen2).
* [Tomas Kalibera](https://github.com/kalibera) for
  [rchk](https://github.com/kalibera/rchk) and the accompanying vagrant image,
  and rcnst to help detect errors in compiled code.  Tomas also worked on the
  [precursor to the Oracle `FastR`][21].
* [Winston Chang](https://github.com/wch) for the
  [r-debug](https://hub.docker.com/r/wch1/r-debug/) docker container, in
  particular because of the valgrind level 2 instrumented version of R.
* [Hadley Wickham](https://github.com/hadley/) et al. for
  [ggplot2](https://ggplot2.tidyverse.org/).

## Notes on Benchmarking

[Benchmarks][10] are under:

    R version 4.2.1 (2022-06-23)
    Platform: x86_64-apple-darwin17.0 (64-bit)
    Running under: macOS Big Sur ... 10.16

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
[9]: https://www.brodieg.com/tags/hydra/
[10]: https://github.com/brodieG/r2c/blob/logo-old-method/extra/benchmarks-public.Rmd
[11]: https://www.zeileis.org/
[12]: https://cran.r-project.org/package=zoo
[13]: https://github.com/DavisVaughan
[14]: https://github.com/r-lib/slider
[15]: https://cran.r-project.org/package=functional
[16]: https://cran.r-project.org/package=Rcpp
[17]: https://docs.renjin.org/en/latest/package/index.html
[18]: https://www.renjin.org/index.html
[19]: https://r-vm.net
[20]: https://thesis.r-vm.net/main.html
[21]: https://github.com/allr/purdue-fastr
[22]: https://github.com/jjf234/roll

[^1]: Depending on your compilation settings and machine, there is room for
  improvement, but not enough that R stands out as being particularly slow at
  this task.
[^2]: Gforce is available for simple expressions of the form `fun(var)` for
  many of the basic statistic functions (see `?data.table::datatable.optimize).
[^3]: My limited experience with `{FastR}`is that it is astonishing, but also
  frustrating.  What it does is amazing, but the compatibility limitations are
  real (e.g.  with the current version neither {`data.table`} nor {`ggplot2`}
  install out of the box, and more), and performance is volatile (e.g. package
  installation and some other tasks are painfully slow, some expressions will
  hiccup after the initial warm-up).  At this point it does not seem like a
  viable drop-in replacement to R.  It likely excels at running scalar
  operations in loops and similar, something that R itself struggles at.
[^4]: Notice that `fsum` with groups is faster than
  even the straight up `sum` without groups, primarily because it handles the
  `narm` as a dedicated branch instead of a conditional in the loop (this is an
  [oddity with `sum`][8] on some platforms).  `fsum` also uses a plain double
  accumulator and not the long double used by the other implementations so the
  results are not identical to the other implementations that use long doubles
  (on systems that support them).  Curiously on my system summing long doubles
  is faster than summing doubles (absent NAs, inifinities, or denormals, for
  which long double performance collapses).
[^5]: We can make `{collapse}` a little faster by computing `mean(x)` once and
  re-using the result, but at that point the comparison is not apples to apples
  anymore.
[^6]: The first compilation can be quite slow as it requires loading the
  compiler, etc.  Subsequent compilations run in tenths of seconds.
[^7]: For this very specific task R also provides `rowsum`, but as it is limited
  to sums and we cannot separate the splitting and summing steps for timing we
  will not discuss it further.
[^8]: Alternatives involve using `fwithin(x)` as a replacement for
  `(x - fmean(x, g, TRA="replace_fill"))` and `fgroup_by(g) |> fsummarize(...)`
  to avoid the need to repeatedly specify groups, although timings are similar
  with these changes.
[^9]: In order to make the benchmarks comparable, we use `r2c::mean1` instead of
  `base::mean`.  This is to ensure that all implementations are using a single
  pass mean calculation as that is what `fmean` does.
[^10]: E.g. don't expect S3 dispatch to work if you define `mean.numeric`,
  although why one would do that for functions covered by `{r2c}` is unclear.
[^11]: Pre-grouping in this case means primarily computing group-offsets in
  data already sorted by group.  Depending on the data, sorting and grouping can
  be a significant part of the computational cost, although it is similar for
  all all implementations tested here.  `{r2c}`, `{data.table}`, `{collapse}`
  all use radix sort, although `{collapse}` also has a hash-based grouping
  algorithm that is particularly effective for integer groups in which the
  grouped result fits in CPU cache.  Benchmarking the grouping is out of scope
  of this document for the time being, but it is an important part of group
  statistic computation.
[^13]: On systems with 80bit long double accumulators, it seems likely that the
  "on-line" algorithm will be sufficiently precise for most applications.
[^14]: It turns out there is `roll::roll_lm` that can compute slopes, but it
  cannot handle the general case of composing arbitrary statistics from the
  ones it implements.
