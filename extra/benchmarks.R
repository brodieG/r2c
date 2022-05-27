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
sys.time(
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
    x <- runif(n)
    y <- runif(n)
    g <- cumsum(sample(c(TRUE, rep(FALSE, 9)), n, replace=TRUE))
    x.split <- split(x, g)
    y.split <- split(y, g)
    mean <- mean.default

    system.time(
      g.slope.base <- mapply(
        function(x, y)
          sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2),
          x.split, y.split
    ) )


    system.time({
      res <- numeric(length(x.split))
      for(i in seq_along(x.split)) {
        xi <- x.split[[i]]
        yi <- y.split[[i]]
        res[i] <-
          sum((xi - mean(xi)) * (yi - mean(yi))) / sum((xi - mean(xi)) ^ 2)
      }
    })

