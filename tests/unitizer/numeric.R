unitizer_sect('basic numeric', {
  f1a <- r2cq({
    y <- numeric(x)
    y[2] <- 1
    y
  })
  f1a(3)
  f1a(TRUE)

  f1b <- r2cq({
    y <- numeric(length(x))
    y[2] <- 1
    x * y
  })
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

