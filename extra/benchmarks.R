

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
    options(digits=3)

    set.seed(1)
    n <- 1e7
    x <- mx <- runif(n) * runif(n)  # full 64 bit precision randomness
    g <- rep(seq_len(n/10), each=10)

    dim(mx) <- c(10, n/10)
    library(data.table); setDTthreads(1);
    dt <- setkey(data.table(x, g), g)

    library(collapse)
    cg <- collapse::GRP(g)

    fmean2 <- function(x, cg) rep(fmean(x, cg, na.rm=FALSE), cg[['group.sizes']])
    fsum2 <- function(x, cg) rep(fsum(x, cg, na.rm=FALSE), cg[['group.sizes']])
    system.time(
      g.slp.c <-
        fsum((x - fmean2(x, cg)) * (y - fmean2(y, cg)), cg, na.rm=FALSE) /
        fsum((x - fmean2(x, cg))^2, cg, na.rm=FALSE)
    )
    system.time({
      g.mu.c <- fmean2(x, cg)
      x_g.mu.c <- (x - g.mu.c)
      g.slp.c <-
        fsum(x_g.mu.c * (y - fmean2(y, cg)), cg, na.rm=FALSE) /
        fsum(x_g.mu.c, cg, na.rm=FALSE)
    })
    dtg <- data.frame(x, y, g) |> group_by(g)
    system.time(g.sum.dply <- summarise(dtg, sum(x)))



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
