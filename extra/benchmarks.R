library(r2c)
n <- 1e6
gn <- n/1e3
set.seed(1)
g <- sample(gn, n, replace=TRUE)
x <- runif(n)
y <- runif(n)
x[sample(n, 3)] <- NA
data <- data.frame(x, y)

o <- order(g)
go <- g[o]
do <- data[o,]

# call <- quote(sum((x - mean(x)) ^ 2))
# obj <- r2c(call)
# res1 <- group_exec(obj, data, g)
# resb <-  base_grp_eval(data, g, call)
# identical(res1, resb)

library(data.table)
setDTthreads(threads = 1)
dto <- setDT(cbind(do, go))
setkey(dto, go)

obj <- r2cq(sum(x))
sys.time(res1 <- group_exec(obj, do, go, sort=FALSE))
sys.time(res2 <- dto[, sum(x), go][['V1']])
all.equal(res1, res2)

obj <- r2cq(sum(x + y))
sys.time(res1 <- group_exec(obj, do, go, sort=FALSE))
sys.time(res2 <- dto[, sum(x + y), go][['V1']])
all.equal(res1, res2)

mean <- mean.default
call <- quote(sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2))
call2 <- quote(sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) * (x - mean(x))))
obj <- r2c(call)
sys.time(res1 <- group_exec(obj, do, go, sort=FALSE))
system.time(
  res2 <- dto[,
    sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2), go
  ][['V1']]
)
all.equal(res1, res2)
identical(res1, res2)
resb <-  base_grp_eval(do, go, call)
identical(res1, resb)

d2 <- do[go==152,]
call <- quote(sum((x - mean(x)) * (y - mean(y))))
call <- quote(sum((x - mean(x)) ^ 2))
call2 <- quote(sum((x - mean(x)) * (x - mean(x))))
# call <- quote(sum((x - mean(x)) ^ 3))
obj <- r2c(call2)
g2 <- rep(1L, nrow(d2))
res1 <- group_exec(obj, d2, g2, sort=FALSE)
resb <-  base_grp_eval(d2, g2, call)
identical(res1, resb)


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
    system.time(g.sum <- vapply(x.split, sum, 0L))
    identical(g.sum, g.sum.r2c)

    r2c_sum <- r2cq(sum(x))
    system.time(r2c.slope <- group_exec(r2c_sum, g, list(x, y), sorted=TRUE))
    r2c_sum <- r2cq(sum(x, na.rm=TRUE))
    r2c_slope <- r2cq(sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2))
    system.time(r2c.slope <- group_exec(r2c_slope, g, list(x, y), sorted=TRUE))
    r2c_slope0 <- r2cq(sum((x - mean0(x)) * (y - mean0(y))) / sum((x - mean0(x)) ^ 2))
    system.time(r2c.slope0 <- group_exec(r2c_slope0, g, list(x, y), sorted=TRUE))
    r2c_slope2 <- r2cq(mean((x - sum(x)) * (y - sum(y))) / mean((x - sum(x)) ^ 2))
    system.time(group_exec(r2c_slope2, g, list(x, y), sorted=TRUE))

    library(collapse)
    gg <- GRP(g)
    system.time(fsum(x, gg))


    fmean2 <- function(x, gg) rep(fmean(x, gg), gg[['group.sizes']])
    fsum2 <- function(x, gg) rep(fsum(x, gg), gg[['group.sizes']])
    system.time(
      clp.slope <-
        fsum2((x - fmean2(x, gg)) * (y - fmean2(y, gg)), gg) /
        fsum2((x - fmean2(x, gg))^2, gg)
    )



    x.split <- split(x, g)
    y.split <- split(y, g)
    mean <- mean.default

    library(data.table)
    dt <- data.table(x, y, g)
    setkey(dt, g)



    system.time(g.sum <- vapply(x.split, sum, 0))

    system.time(g.sum <- vapply(x.split, sum, 0))

    system.time(g.slope.base <- mapply(slope, x.split, y.split))
    system.time(
      g.slope.dt <- dt[,
        sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2),
        keyby=g
    ] )



    system.time({
      res <- numeric(length(x.split))
      for(i in seq_along(x.split)) {
        xi <- x.split[[i]]
        yi <- y.split[[i]]
        res[i] <-
          sum((xi - mean(xi)) * (yi - mean(yi))) / sum((xi - mean(xi)) ^ 2)
      }
    })

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
