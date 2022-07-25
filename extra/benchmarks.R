

system.time <- sys.time <- function(exp, reps=11) {
  res <- matrix(0, reps, 5)
  time.call <- quote(base::system.time({NULL}))
  time.call[[2]][[2]] <- substitute(exp)
  gc()
  for(i in seq_len(reps)) {
    res[i,] <- eval(time.call, parent.frame())
  }
  structure(res, class='proc_time2')
}
print.proc_time2 <- function(x, ...) {
  print(
    structure(
      # x[order(x[,3]),][ceiling(nrow(x)/2),],
      round(colMeans(x), 3),
      names=c("user.self", "sys.self", "elapsed", "user.child", "sys.child"),
      class='proc_time'
) ) }

# - In Order -------------------------------------------------------------------

library(r2c)
library(collapse)
library(data.table); setDTthreads(1);

set.seed(1)
n <- 1e7
gn <- 10
ng <- n/gn
x <- runif(n) * runif(n)  # full 64 bit precision randomness
y <- runif(n) * runif(n)
g <- cumsum(sample(c(TRUE, rep(FALSE, gn - 1)), n, replace=TRUE))
# g <- cumsum(sample(c(TRUE, rep(FALSE, 1000)), n, replace=TRUE))
g.r2c <- process_groups(g, sorted=TRUE)
g.clp <- GRP(g)
dt <- data.table(x, g)
setkey(dt, g)

r2c_sum <- r2cq(sum(x))

x.split <- split(x, g)
y.split <- split(y, g)
system.time(g.sum <- vapply(x.split, sum, 0))
##   user  system elapsed 
##  0.701   0.008   0.713 
system.time(g.sum.r2c <- group_exec(r2c_sum, g.r2c, x))
##   user  system elapsed 
##  0.054   0.001   0.056 
identical(g.sum, g.sum.r2c)
## [1] TRUE
system.time(g.sum.clp <- fsum(x, g.clp, na.rm=FALSE))
##   user  system elapsed 
##  0.030   0.000   0.031 
all.equal(g.sum, g.sum.clp)
## [1] TRUE
system.time(g.sum.dt <- dt[, sum(x), keyby=g][['V1']])
##   user  system elapsed 
##  0.205   0.027   0.234 
all.equal(unname(g.sum), g.sum.dt)
## [1] TRUE
identical(unname(g.sum), g.sum.dt)
## [1] FALSE

r2c_slope <- r2cq(sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2))
system.time(r2c.slope <- group_exec(r2c_slope, g.r2c, list(x, y)))
##   user  system elapsed 
##  0.227   0.001   0.229 
r2c_slope0 <- r2cq(sum((x - mean0(x)) * (y - mean0(y))) / sum((x - mean0(x)) ^ 2))
system.time(r2c.slope0 <- group_exec(r2c_slope0, g.r2c, list(x, y)))
##   user  system elapsed 
##  0.148   0.001   0.150 

fmean2 <- function(x, cg) fmean(x, cg, na.rm=FALSE, TRA="replace_fill")
system.time(
  g.slp.c <-
    fsum((x - fmean2(x, g.clp)) * (y - fmean2(y, g.clp)), g.clp, na.rm=FALSE) /
    fsum((x - fmean2(x, g.clp))^2, g.clp, na.rm=FALSE)
)
##   user  system elapsed 
##  0.245   0.001   0.247 


set.seed(1)
gn <- 1e3
ng <- n/gn
g <- cumsum(sample(c(TRUE, rep(FALSE, gn - 1)), n, replace=TRUE))

x.split <- split(x, g)
y.split <- split(y, g)
g.r2c <- process_groups(g, sorted=TRUE)
g.clp <- GRP(g)
dt <- data.table(x, g)
setkey(dt, g)

system.time(g.sum <- vapply(x.split, sum, 0))
##   user  system elapsed 
##  0.041   0.000   0.041 
system.time(g.sum.r2c <- group_exec(r2c_sum, g.r2c, x))
##   user  system elapsed 
##  0.017   0.000   0.017 
identical(g.sum, g.sum.r2c)
## [1] TRUE
system.time(g.sum.clp <- fsum(x, g.clp, na.rm=FALSE))
##   user  system elapsed 
##  0.042   0.000   0.043 
all.equal(g.sum, g.sum.clp)
## [1] TRUE
system.time(g.sum.dt <- dt[, sum(x), keyby=g][['V1']])
##   user  system elapsed 
##  0.178   0.026   0.205 
all.equal(unname(g.sum), g.sum.dt)
## [1] TRUE

system.time(r2c.slope <- group_exec(r2c_slope, g.r2c, list(x, y)))
##   user  system elapsed 
##  0.118   0.001   0.119 
system.time(r2c.slope0 <- group_exec(r2c_slope0, g.r2c, list(x, y)))
##   user  system elapsed 
##  0.092   0.000   0.093 

system.time(
  g.slp.c <-
    fsum((x - fmean2(x, g.clp)) * (y - fmean2(y, g.clp)), g.clp, na.rm=FALSE) /
    fsum((x - fmean2(x, g.clp))^2, g.clp, na.rm=FALSE)
)
##   user  system elapsed 
##  0.310   0.007   0.319 

dt <- data.table(x, y, g)
setkey(dt, g)
mean <- mean0
system.time(
  dt[, sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2), keyby=g]
)
##   user  system elapsed 
##  0.343   0.014   0.359 
slope0 <- function(x, y) sum((x - mean0(x)) * (y - mean0(y))) / sum((x - mean0(x)) ^ 2)
slope <- function(x, y) sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2)
system.time(slope.base <- mapply(slope, x.split, y.split))
##   user  system elapsed 
##  0.297   0.012   0.310 
system.time(slope0.base <- mapply(slope0, x.split, y.split))
system.time(slope0.single <- slope0(x, y))
##   user  system elapsed 
##  0.259   0.001   0.261 

dtg <- dt |> fgroup_by(g);

system.time(
   g.slp.clp.2 <- fsummarise(
     dtg,
     slope =
     fsum(fwithin(y, na.rm = FALSE), fwithin(y, na.rm = FALSE), na.rm = FALSE) %/=%
     fsum(x_center^2, na.rm = FALSE)
   )
)
dt |>
  fgroup_by(g) |>
  fsummarise(slope = fsum(fwithin(x) * fwithin(y)) / fsum(fwithin(x^2)))

dt |>
  fgroup_by(g) |>
  fmutate(x_center = fwithin(x)) |>
  fsummarise(slope = fsum(fwithin(x) * fwithin(y)) / fsum(fwithin(x)^2))

dt |>
   fgroup_by(g) |>
   fmutate(x_center = fwithin(x, na.rm = FALSE)) |>
   fsummarise(
   slope =
     fsum(x_center, fwithin(y, na.rm = FALSE), na.rm = FALSE) %/=%
     fsum(x_center, na.rm = FALSE)^2
   )

# - Older stuff ----------------------------------------------------------------

##   user  system elapsed
##  0.244   0.001   0.246

system.time(
  g.slp.cwithin <-
    fsum(
      fwithin(x, cg, na.rm=FALSE) * fwithin(y, cg, na.rm=FALSE),
      cg, na.rm=FALSE
    ) /
    fsum(fwithin(x, cg, na.rm=FALSE)^2, cg, na.rm=FALSE)
)
##   user  system elapsed
##  0.234   0.001   0.237

r2c_slope <- r2cq(
  sum((x - mean(x)) * (y - mean(y))) /
  sum((x - mean(x)) ^ 2)
)
system.time(g.slp.r2c <- group_exec(r2c_slope, g, list(x, y), sorted=TRUE))
##   user  system elapsed
##  0.282   0.001   0.284

r2c_slope0 <- r2cq(
  sum((x - mean0(x)) * (y - mean0(y))) /
  sum((x - mean0(x)) ^ 2)
)
system.time(
  g.slp.r2c0 <- group_exec(r2c_slope0, g, list(x, y), sorted=TRUE)
)
##   user  system elapsed
##  0.198   0.001   0.200

all.equal(g.slp.c, g.slp.r2c)
## [1] TRUE
all.equal(g.slp.r2c0, g.slp.r2c)
## [1] TRUE
all.equal(g.slp.cwithin, g.slp.r2c0)
## [1] TRUE
all.equal(g.slp.cwithin, g.slp.r2c)
## [1] TRUE

# - Shuffle --------------------------------------------------------------------

## Most of this stuff was done with n<-1e8

g2 <- sample(g)

system.time(r2c.res <- group_exec(r2c_sum, g2, x, sorted=TRUE))
system.time(clp.res <- fsum(x, g2, na.rm=FALSE))
cg <- GRP(g)
bench::mark(group_exec(r2c_sum, g, x, sorted=TRUE),fsum(x, GRP(g), na.rm=FALSE))

system.time(r2c.res <- group_exec(r2c_sum, g, x))
system.time(clp.res <- fsum(x, g, na.rm=FALSE))

## 1e8 randomized, 1e7 groups.  Interesting that order switches

## > system.time(r2c.res <- group_exec(r2c_sum, g, x))
##    user  system elapsed
##  11.443   0.994  12.510
## > system.time(clp.res <- fsum(x, g, na.rm=FALSE))
##    user  system elapsed
##  18.118   0.951  19.227
## > g <- sample(n/10, n, replace=TRUE)
## > bench::mark(group_exec(r2c_sum, g, x), fsum(x, g, na.rm=FALSE))
## # A tibble: 2 × 13
##   expression                     min   median `itr/sec` mem_alloc
##   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>
## 1 group_exec(r2c_sum, g, x)      13s      13s    0.0770    1.82GB
## 2 fsum(x, g, na.rm = FALSE)    20.1s    20.1s    0.0497    1.04GB

## 1e8 randomized, 1e3 groups.

## > system.time(r2c.res <- group_exec(r2c_sum, g, x))
##    user  system elapsed
##  10.180   0.974  11.214
## > system.time(clp.res <- fsum(x, g, na.rm=FALSE))
##    user  system elapsed
##   1.714   0.294   2.013
##
## > bench::mark(group_exec(r2c_sum, g, x), fsum(x, g, na.rm=FALSE))
## # A tibble: 2 × 13
##   expression                     min   median `itr/sec` mem_alloc
##   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>
## 1 group_exec(r2c_sum, g, x)   11.12s   11.12s    0.0899    1.49GB
## 2 fsum(x, g, na.rm = FALSE)    2.17s    2.17s    0.462   765.99MB

## 1e8 randomized, only 10 groups.
## > system.time(r2c.res <- group_exec(r2c_sum, g, x))
##    user  system elapsed
##   2.518   0.717   3.345
## > system.time(clp.res <- fsum(x, g, na.rm=FALSE))
##    user  system elapsed
##   0.707   0.243   0.958

## Some non-randomized views with 1e7 for reference

## > bench::mark(group_exec(r2c_sum, g, x, sorted=TRUE),fsum(x, cg, na.rm=FALSE))
## # A tibble: 2 × 13
##   expression                                    min   median `itr/sec` mem_alloc
##   <bch:expr>                               <bch:tm> <bch:tm>     <dbl> <bch:byt>
## 1 group_exec(r2c_sum, g, x, sorted = TRUE)    1.28s    1.28s     0.779   343.4MB
## 2 fsum(x, cg, na.rm = FALSE)                249.7ms 282.36ms     3.54     76.3MB
## # … with 8 more variables: `gc/sec` <dbl>, n_itr <int>, n_gc <dbl>,
## #   total_time <bch:tm>, result <list>, memory <list>, time <list>, gc <list>

## Here we include the GRP call

## > bench::mark(group_exec(r2c_sum, g, x, sorted=TRUE),fsum(x, GRP(g), na.rm=FALSE))
## # A tibble: 2 × 13
##   expression                                    min   median `itr/sec` mem_alloc
##   <bch:expr>                               <bch:tm> <bch:tm>     <dbl> <bch:byt>
## 1 group_exec(r2c_sum, g, x, sorted = TRUE)    1.37s    1.37s     0.731     343MB
## 2 fsum(x, GRP(g), na.rm = FALSE)              2.46s    2.46s     0.407     954MB
## # … with 8 more variables: `gc/sec` <dbl>, n_itr <int>, n_gc <dbl>,
## #   total_time <bch:tm>, result <list>, memory <list>, time <list>, gc <list>


## Very weird patterns of what's better when.  Seems likely that what's
## happening is that we're hitting swap earlier with r2c due to the re-ordering
## and keeping of the vectors.  So it's fast to re-order vectors, but if you run
## out of memory it's bad news.

## This shows that the slowness is completely from ordering the vector.  IIRC
## collapse doesn't order, just for each group has the corresponding indices and
## fetches them.
##
## test_order2 ---------- : 12.34 -  0.00
##     reorder ---------- : 10.77 - 10.77
##     do.call ---------- :  1.57 -  0.00
##         <Anonymous> -- :  1.57 -  1.57

## Collapse with 1e7 groups
##
## Ticks: 11860; Iterations: 1; Time Per: 17.87 seconds; Time Total: 17.87 seconds; Time Ticks: 11.86
##
##                                             seconds
## fsum ------------------------------ : 17.87 -  0.00
##     fsum.default ------------------ : 17.87 -  1.02
##         qF ------------------------ : 16.85 -  0.00
##             hashfact -------------- : 16.85 -  0.00
##                 groupfact_sorted -- : 16.85 - 13.78
##                     Csv ----------- :  2.81 -  2.81
##                     forder.int ---- :  0.26 -  0.26

## Collapse with 1e5 groups (size ~1000)
##
## Ticks: 4159; Iterations: 2; Time Per: 2.645 seconds; Time Total: 5.290 seconds; Time Ticks: 4.159
##
##                                      milliseconds
## fsum ------------------------------ : 2645 -    0
##     fsum.default ------------------ : 2645 -  167
##         qF ------------------------ : 2478 -    0
##             hashfact -------------- : 2478 -    0
##                 groupfact_sorted -- : 2478 - 1952
##                     Csv ----------- :  521 -  521

## > g <- sample(n/10, n, replace=TRUE)
## > bench::mark(fsum(x, g, na.rm=FALSE))
## # A tibble: 1 × 13
##   expression                     min   median `itr/sec` mem_alloc `gc/sec` n_itr
##   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int>
## 1 fsum(x, g, na.rm = FALSE)    17.8s    17.8s    0.0562    1.04GB        0     1
## # … with 6 more variables: n_gc <dbl>, total_time <bch:tm>, result <list>,
## #   memory <list>, time <list>, gc <list>
## > g <- sample(n/1e3, n, replace=TRUE)
## > bench::mark(fsum(x, g, na.rm=FALSE))
## # A tibble: 1 × 13
##   expression                     min   median `itr/sec` mem_alloc `gc/sec` n_itr
##   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int>
## 1 fsum(x, g, na.rm = FALSE)    2.52s    2.52s     0.397     766MB    0.397     1
## # … with 6 more variables: n_gc <dbl>, total_time <bch:tm>, result <list>,
## #   memory <list>, time <list>, gc <list>
## Warning message:
## Some expressions had a GC in every iteration; so filtering is disabled.
## > g <- sample(n/5, n, replace=TRUE)
## > bench::mark(fsum(x, g, na.rm=FALSE))
## # A tibble: 1 × 13
##   expression                     min   median `itr/sec` mem_alloc `gc/sec` n_itr
##   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int>
## 1 fsum(x, g, na.rm = FALSE)    17.8s    17.8s    0.0562    1.34GB        0     1
## # … with 6 more variables: n_gc <dbl>, total_time <bch:tm>, result <list>,
## #   memory <list>, time <list>, gc <list>

## Memory required seems to grow with number of groups, so the theory that this
## is a swapping issue still stands.  This may just be the group data itself?
## No, that's only part of it.  The ordered vector itself is much bigger.  Note
## collapse uses Calloc, and it's unclear whether that shows up in the memory
## reporting (it seems like it does not show up).
##
## According to this though there should be some very large memory allocations
## that aren't showing up, and even in the 1e8 case we're not seeing the
## slowdowns with few groups that we're attributing to swapping for r2c.  So
## either the Calloc memory is not subject to the swapping problem (hard to see
## why), or we're misunderstanding the use of Calloc by collapse.
##
## FWIW trying on a 16GB system the problem goes away for `r2c`.  Haven't tied
## out the memory usage issue.

## >  g <- sample(n/5, n, replace=TRUE)
## > bench::mark(group_exec(r2c_sum, g, x))
## # A tibble: 1 × 13
##   expression                     min   median `itr/sec` mem_alloc
##   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>
## 1 group_exec(r2c_sum, g, x)    20.4s    20.4s    0.0490    2.16GB
##
## >  g <- sample(n/10, n, replace=TRUE)
## > bench::mark(group_exec(r2c_sum, g, x))
## # A tibble: 1 × 13
##   expression                     min   median `itr/sec` mem_alloc `gc/sec` n_itr
##   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int>
## 1 group_exec(r2c_sum, g, x)    12.9s    12.9s    0.0777    1.82GB   0.0777     1
##
## >  g <- sample(n/1e3, n, replace=TRUE)
## > bench::mark(group_exec(r2c_sum, g, x))
## # A tibble: 1 × 13
##   expression                     min   median `itr/sec` mem_alloc `gc/sec` n_itr
##   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int>
## 1 group_exec(r2c_sum, g, x)    11.6s    11.6s    0.0859    1.49GB   0.0859     1

## This is for ~1e7 groups, so groups alone don't account for it
##
## > xx <- r2c:::group_sizes(sort(g))
## > object.size(xx)
## 119994832 bytes
## > str(xx)
## List of 3
##  $ : num [1:9999550] 9 9 5 12 11 7 9 11 10 9 ...
##  $ : int [1:9999550] 1 2 3 4 5 6 7 8 9 10 ...
##  $ : num 31

## It might be a factor for collapse:
##
## > xx <- GRP(g)
## > object.size(xx)
## 959995840 bytes
##
## The issue there is it seems to make a copy of the groups vector that is not
## quite the same as the groups vector, and that will be large.

# - Small Random ---------------------------------------------------------------

set.seed(1)
n <- 1e7
x <- runif(n) * runif(n)  # full 64 bit precision randomness
g <- sample(n/10, n, replace=TRUE)
system.time(group_exec(r2c_sum, g, x))
##   user  system elapsed
##  0.843   0.007   0.859
system.time(fsum(x, g, na.rm=FALSE))
##   user  system elapsed
##  0.810   0.010   0.829

g <- sample(n/1e3, n, replace=TRUE)
system.time(r0 <- group_exec(r2c_sum, g, x))
##    user  system elapsed
##   0.706   0.008   0.720
system.time(r1 <- fsum(x, g, na.rm=FALSE))
##    user  system elapsed
##   0.073   0.002   0.076

## Hmm, this is not good.  Seems like the hash method is better up to some
## number of distinct entries, and then crashes:

g6 <- sample(n/10, n, replace=TRUE)
g4 <- sample(n/1e3, n, replace=TRUE)
bench::mark(
  GRP(g6, method='radix'), GRP(g6, method='hash'),
  GRP(g4, method='radix'), GRP(g4, method='hash'),
  check=FALSE
)
##   expression                     min   median `itr/sec` mem_alloc
##   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>
## 1 GRP(g6, method = "radix")  425.3ms    438ms      2.28    91.6MB
## 2 GRP(g6, method = "hash")   698.8ms  698.8ms      1.43    49.6MB
## 3 GRP(g4, method = "radix")  332.1ms  332.8ms      3.00    76.4MB
## 4 GRP(g4, method = "hash")    87.5ms   87.8ms     10.7     38.3MB

## Seems to show hash is pretty fast so long as there are not too many groups.

system.time(r2 <- {
  cg <- GRP(g6)
  r2 <- fsum(x, cg, na.rm=FALSE)
})
##   user  system elapsed
##  0.512   0.005   0.524
system.time(r2 <- {
  cg <- GRP(g4)
  fsum(x, cg, na.rm=FALSE)
})
##   user  system elapsed
##  0.330   0.001   0.333
system.time(r2 <- {
  cg <- GRP(g4, method='hash')
  fsum(x, cg, na.rm=FALSE)
})
##    user  system elapsed
##   0.084   0.003   0.087

## Let's try to mess with the hash

g6 <- sample(n/10, n, replace=TRUE)
g4 <- sample(n/1e3, n, replace=TRUE)
bench::mark(
  GRP(g6, method='radix'), GRP(g6, method='hash'),
  GRP(g4, method='radix'), GRP(g4, method='hash'),
  check=FALSE
)
bench::mark(
  fsum(x, g6, na.rm=FALSE),
  fsum(x, g4, na.rm=FALSE),
  fsum(x, g4.big, na.rm=FALSE),
  check=FALSE
)
## We're going to mess with the hash, one by using one that ends up requiring
## the full size of the vector, and one that causes a collision with every
## single entry, but then by extension doesn't use the full size of the vector.

all.range <- as.integer(seq(1, n, length.out=1e3L))
g4.ar <- sample(all.range, n, replace=TRUE)
treeprof::treeprof(fsum(x, g4.ar, na.rm=FALSE))


res <- fsum(x, g6, na.rm=FALSE)

## This is actually comparable, so doesn't seem to bother it much (maybe
## suggests some of the optimizations not necessary)?
##   expression                     min   median `itr/sec` mem_alloc `gc/sec` n_itr
## 1 GRP(g6, method = "radix")    449ms    467ms      2.14    91.6MB     1.07     2
## 2 GRP(g6, method = "hash")     843ms    843ms      1.19    49.6MB     1.19     1
## 3 GRP(g4, method = "radix")    365ms    380ms      2.63    76.4MB     0        2
## 4 GRP(g4, method = "hash")      74ms    104ms      9.30    38.3MB     1.86     5
# - Small Random Slope ---------------------------------------------------------

r2c_slope0 <- r2cq(
  sum((x - mean0(x)) * (y - mean0(y))) /
  sum((x - mean0(x)) ^ 2)
)

fmean2 <- function(x, g) fmean(x, g, na.rm=FALSE, TRA="replace_fill")
clp_slope <- function(x, y, g) {
  cg <- GRP(g)
  fsum((x - fmean2(x, cg)) * (y - fmean2(y, cg)), cg, na.rm=FALSE) /
  fsum((x - fmean2(x, cg))^2, cg, na.rm=FALSE)
}

system.time(group_exec(r2c_slope0, g, list(x, y)))
system.time(clp_slope(x, y, g)

bench::mark(
  group_exec(r2c_slope0, g6, list(x, y)), clp_slope(x, y, g6),
  group_exec(r2c_slope0, g4, list(x, y)), clp_slope(x, y, g4),
  check=FALSE
)
## A tibble: 4 × 13
## expression                                  min   median `itr/sec` mem_alloc
## <bch:expr>                             <bch:tm> <bch:tm>     <dbl> <bch:byt>
## group_exec(r2c_slope0, g6, list(x, y))    1.37s    1.37s     0.730     263MB
## clp_slope(x, y, g6)                       1.65s    1.65s     0.607     359MB
## group_exec(r2c_slope0, g4, list(x, y))    1.13s    1.13s     0.882     229MB
## clp_slope(x, y, g4)                    602.38ms 602.38ms     1.66      306MB

# - Large Groups ---------------------------------------------------------------

set.seed(1)
n <- 1e8
x <- runif(n) * runif(n)  # full 64 bit precision randomness
gn <- 10
ng <- n/gn
g <- cumsum(sample(c(TRUE, rep(FALSE, (gn - 1))), n, replace=TRUE))
# g <- sample(n/10, n, replace=TRUE)
library(r2c)
f <- r2cq(sum(x))
system.time(res <- group_exec(f, g, x, sorted=TRUE))
##   user  system elapsed
##  0.859   0.025   0.918
library(collapse)
system.time(cg <- GRP(g))
##   user  system elapsed
##  1.192   0.404   1.723
system.time(res.clp <- fsum(x, cg, na.rm=FALSE))
##   user  system elapsed
##  0.311   0.033   0.358

gn <- 1000
ng <- n/gn
g <- cumsum(sample(c(TRUE, rep(FALSE, (gn - 1))), n, replace=TRUE))
system.time(res.r2c <- group_exec(f, g, x, sorted=TRUE))
##   user  system elapsed
##  0.361   0.002   0.364
system.time(cg <- GRP(g))
##   user  system elapsed
##  0.465   0.212   0.679
system.time(res.clp <- fsum(x, cg, na.rm=FALSE))
##   user  system elapsed
##  0.365   0.002   0.369

# Fewer groups favor r2c, as expected, but the weird part is that this means the
# underlying calculation is actually faster since more than half the time is
# spent computing group meta data for r2c.

##                           milliseconds
## group_exec ----------- : 348.6 -   0.0
##     group_exec_int --- : 348.6 -   0.0
##         group_sizes -- : 201.6 - 201.6
##         run_int ------ : 146.5 - 146.5

# Suggests that without the group execution overhead, we are much faster,
# possibly because we're writing sequentially to final result, whereas collapse
# is not.

# In terms of group computation, the interesting observation is that the first
# pass is done in half the time of the second, even in the case where there are
# few groups because the first pass can do xmm instructions to increment the
# counter, but the second can't because there is more to do.  Maybe we can try
# to restructure the group sizes in a way that is closer to the first loop so
# that optimization will happen (will reduce overall cost by 1/3).  But the
# problem is the double pass is a bit expensive.

# - New Pre-sorted -------------------------------------------------------------

set.seed(1)
n <- 1e8
x <- runif(n) * runif(n)  # full 64 bit precision randomness
gn <- 10
ng <- n/gn
g <- cumsum(sample(c(TRUE, rep(FALSE, (gn - 1))), n, replace=TRUE))
library(r2c)
library(collapse)
f <- r2cq(sum(x))
system.time(g.r2c <- process_groups(g, sorted=TRUE))
system.time(res.r2c <- group_exec(f, g.r2c, x))
##   user  system elapsed
##  0.426   0.007   0.436
system.time(g.clp <- GRP(g))
system.time(res.clp <- fsum(x, g.clp))
##   user  system elapsed
##  0.354   0.003   0.359
all.equal(res.r2c, res.clp)

gn <- 1000
ng <- n/gn
g <- cumsum(sample(c(TRUE, rep(FALSE, (gn - 1))), n, replace=TRUE))
system.time(g.r2c <- process_groups(g, sorted=TRUE))
system.time(res.r2c <- group_exec(f, g.r2c, x))
##   user  system elapsed
##  0.139   0.000   0.140
system.time(g.clp <- GRP(g))
system.time(res.clp <- fsum(x, g.clp))
##   user  system elapsed
##  0.503   0.002   0.507
all.equal(res.r2c, res.clp)

set.seed(1)
n <- 1e7
x <- runif(n) * runif(n)  # full 64 bit precision randomness
y <- runif(n) * runif(n)  # full 64 bit precision randomness
gn <- 10
ng <- n/gn
g <- cumsum(sample(c(TRUE, rep(FALSE, (gn - 1))), n, replace=TRUE))
f <- r2cq(sum(x))
r2c_slope0 <- r2cq(
  sum((x - mean0(x)) * (y - mean0(y))) /
  sum((x - mean0(x)) ^ 2)
)
fmean2 <- function(x, g) fmean(x, g, na.rm=FALSE, TRA="replace_fill")
clp_slope <- function(x, y, g) {
  cg <- GRP(g)
  fsum((x - fmean2(x, cg)) * (y - fmean2(y, cg)), cg, na.rm=FALSE) /
  fsum((x - fmean2(x, cg))^2, cg, na.rm=FALSE)
}

system.time(g.r2c <- process_groups(g, sorted=TRUE))
system.time(res.r2c <- group_exec(f, g.r2c, x))
##   user  system elapsed
##  0.047   0.001   0.048
system.time(g.clp <- GRP(g))
system.time(res.clp <- fsum(x, g.clp))
##   user  system elapsed
##  0.037   0.000   0.038
all.equal(res.r2c, res.clp)

system.time(r2c.slp <- group_exec(r2c_slope0, g.r2c, list(x, y)))
##   user  system elapsed 
##  0.143   0.001   0.144 
system.time(clp.slp <- clp_slope(x, y, g.clp))
##   user  system elapsed 
##  0.243   0.002   0.246 
all.equal(r2c.slp, clp.slp)

gn <- 1000
ng <- n/gn
g <- cumsum(sample(c(TRUE, rep(FALSE, (gn - 1))), n, replace=TRUE))
g.r2c <- process_groups(g, sorted=TRUE)
system.time(res.r2c <- group_exec(f, g.r2c, x))
##   user  system elapsed
##  0.016   0.000   0.016
g.clp <- GRP(g)
system.time(res.clp <- fsum(x, g.clp))
##   user  system elapsed
##  0.054   0.000   0.055
all.equal(res.r2c, res.clp)

system.time(r2c.slp <- group_exec(r2c_slope0, g.r2c, list(x, y)))
##   user  system elapsed 
##  0.088   0.000   0.089 
system.time(clp.slp <- clp_slope(x, y, g.clp))
##   user  system elapsed 
##  0.307   0.007   0.315 
all.equal(r2c.slp, clp.slp)


# - Collapse Idiomatic ---------------------------------------------------------

# Submitted by Sebastian, although in his case he used mutate.

set.seed(1)
n <- 1e7
x <- runif(n) * runif(n)  # full 64 bit precision randomness
y <- runif(n) * runif(n)
g <- cumsum(sample(c(TRUE, rep(FALSE, 9)), n, replace=TRUE))
library(data.table)
dt <- data.table(x, y, g)
dt <- dt |> fgroup_by(g)
system.time(
  res.clp <- dt |> fsummarise(
    slope =
      fsum(fwithin(x, na.rm = FALSE) * fwithin(y, na.rm = FALSE), na.rm=FALSE) %/=%
      fsum(fwithin(x, na.rm = FALSE)^2, na.rm = FALSE)
  )
)
##   user  system elapsed
##  0.228   0.002   0.233
r2c_slope0 <- r2cq(sum((x - mean0(x)) * (y - mean0(y))) / sum((x - mean0(x)) ^ 2))
system.time(r2c.slope0 <- group_exec(r2c_slope0, g, list(x, y), sorted=TRUE))
##   user  system elapsed
##  0.185   0.002   0.190

# In this case mean did not get inlined, whereas the others did.  Addly subtract
# shows up three times, but still not inlined.
#
# 3.01 s   10.2%	54.00 ms	                       run
# 1.90 s    6.4%	1.75 s	 	                       mean
# 153.00 ms    0.5%	37.00 ms                        R_finite
# 166.00 ms    0.5%	166.00 ms                      multiply
# 160.00 ms    0.5%	160.00 ms                      sum
# 156.00 ms    0.5%	156.00 ms                      sum
# 128.00 ms    0.4%	128.00 ms                      subtract
# 125.00 ms    0.4%	125.00 ms                      subtract
# 109.00 ms    0.3%	109.00 ms                      sqr
# 105.00 ms    0.3%	105.00 ms                      subtract
# 96.00 ms    0.3%	96.00 ms                       divide
# 6.00 ms    0.0%	6.00 ms	 	                       DYLD-STUB$$R_finite

# - Graal Data -----------------------------------------------------------------

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
