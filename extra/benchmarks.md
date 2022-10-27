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


