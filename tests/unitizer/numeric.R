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
unitizer_sect('convolve', {
  # based on WRE 5.2 convolution example, originally intended for benchmarking
  # purposes but revealed several bugs in for loops along the way.
  f2a.r <- function(a, b, c) {
    ab <- numeric(c)
    for(i in seq_along(a)) {
      for(j in seq_along(b)) {
        ab[i + j - 1] <- ab[i + j - 1] + a[i] * b[j]
      }
    }
    ab
  }
  f2a <- r2cf(f2a.r)
  a <- (1:3) / 2
  b <- c(.75, 2, 1/3)
  (res0 <- f2a(a, b, length(a) + length(b) - 1))
  identical(res0, r2c:::convolve(a, b))
  identical(res0,  f2a(a, b, length(a) + length(b) - 1))
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

