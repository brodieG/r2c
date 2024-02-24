unitizer_sect('basic numeric', {
  f1a <- r2cf(function(x, y) {
    res <- numeric(y)
    res[2] <- x
    res
  })
  f1a(42, 2)
  f1a(TRUE, 2)
  group_exec(f1a, 2:3, 1:2, MoreArgs=list(2))
  group_exec(f1a, 2:4, c(1, 1:2), MoreArgs=list(2))
  # error
  group_exec(f1a, 2:4, c(1, 1:2), MoreArgs=list(x=2))

  # f1b <- r2cq({
  #   y <- numeric(length(x))
  #   y[2] <- 1
  #   x * y
  # })
})
unitizer_sect('numeric_along', {
  f3a <- r2cq({
    z <- numeric_alongn(x, y)
    w <- z + x
    z[length(x) * length(y) / 2] <- 1
    w * y * z
  })
  f3a(1:3, c(10,20))
})

