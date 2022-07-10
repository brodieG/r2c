

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

set.seed(1)
n <- 1e7
x <- runif(n) * runif(n)  # full 64 bit precision randomness
y <- runif(n) * runif(n)
g <- cumsum(sample(c(TRUE, rep(FALSE, 9)), n, replace=TRUE))
# g <- cumsum(sample(c(TRUE, rep(FALSE, 1000)), n, replace=TRUE))
library(r2c)
r2c_sum <- r2cq(sum(x))
r2c_mean <- r2cq(mean(x))
system.time(g.sum.r2c <- group_exec(r2c_sum, g, x, sorted=TRUE))
system.time(g.mean.r2c <- group_exec(r2c_mean, g, x, sorted=TRUE))

x.split <- split(x, g)
y.split <- split(y, g)
mean <- mean.default
system.time(g.sum <- vapply(x.split, sum, 0))
identical(g.sum, g.sum.r2c)

r2c_sum <- r2cq(sum(x))
system.time(r2c.slope <- group_exec(r2c_sum, g, list(x, y), sorted=TRUE))
r2c_sum <- r2cq(sum(x, na.rm=TRUE))

r2c_slope <- r2cq(sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2))

system.time(r2c.slope <- group_exec(r2c_slope, g, list(x, y), sorted=TRUE))

r2c_slope0 <- r2cq(sum((x - mean0(x)) * (y - mean0(y))) / sum((x - mean0(x)) ^ 2))
system.time(r2c.slope0 <- group_exec(r2c_slope0, g, list(x, y), sorted=TRUE))

r2c_slope2 <- r2cq(
  sum((x - mean0(x)) * (y - mean0(y))) /
  sum((x - mean0(x)) ^ 2)
)
system.time(
  g.slope.r2c2 <- group_exec(r2c_slope2, g, list(x, y), sorted=TRUE)
)

library(collapse)
gg <- GRP(g)
system.time(fsum(x, gg))
options(digits=3)

library(collapse)
cg <- collapse::GRP(g)

set.seed(1)
n <- 1e7
x <- runif(n) * runif(n)  # full 64 bit precision randomness
y <- runif(n) * runif(n)
g <- cumsum(sample(c(TRUE, rep(FALSE, 9)), n, replace=TRUE))
library(collapse)
library(r2c)
cg <- GRP(g)

fmean2 <- function(x, cg) fmean(x, cg, na.rm=FALSE, TRA="replace_fill")
system.time(
  g.slp.c <-
    fsum((x - fmean2(x, cg)) * (y - fmean2(y, cg)), cg, na.rm=FALSE) /
    fsum((x - fmean2(x, cg))^2, cg, na.rm=FALSE)
)
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

n <- 1e7
x <- runif(n) * runif(n)
g5 <- sample(n/5, n, replace=TRUE)
g10 <- sample(n/10, n, replace=TRUE)
g20 <- sample(n/20, n, replace=TRUE)
g50 <- sample(n/50, n, replace=TRUE)
g100 <- sample(n/1e2, n, replace=TRUE)

system.time(fsum(x, g2, na.rm=FALSE))
system.time(fsum(x, g5, na.rm=FALSE))
system.time(fsum(x, g10, na.rm=FALSE))
system.time(fsum(x, g20, na.rm=FALSE))
system.time(fsum(x, g50, na.rm=FALSE))
system.time(fsum(x, g100, na.rm=FALSE))

system.time(GRP(g2,   method='hash'))
system.time(GRP(g5,   method='hash'))
system.time(GRP(g10,  method='hash'))
system.time(GRP(g20,  method='hash'))
system.time(GRP(g50,  method='hash'))
system.time(GRP(g100, method='hash'))


system.time(sort(g5, method='radix'))
system.time(sort(g100, method='radix'))

microbenchmark::microbenchmark(times=10,
  fsum(x, g7, na.rm=FALSE),
  fsum(x, g6, na.rm=FALSE),
  fsum(x, g4, na.rm=FALSE)
)

n <- 1e7
x <- runif(n)
io <- seq_len(n) - 1L
ir <- sample(io)
system.time(order_sum(x, io))
system.time(order_sum(x, ir))

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
# g <- cumsum(sample(c(TRUE, rep(FALSE, 9)), n, replace=TRUE))
g <- sample(n/10, n, replace=TRUE)
library(r2c)
f <- r2cq(sum(x), dir="extra/tmp")
# f <- r2cq(sum(x))
gc()
system.time(res <- group_exec(f, g, x, sorted=TRUE))

library(collapse)
cg <- GRP(g)
gc()
system.time(fsum(x, cg, na.rm=FALSE))

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
