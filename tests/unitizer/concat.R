
unitizer_sect("basic", {
  r2c_c <- r2cf(function(...) c(...), check=TRUE)
  r2c_c(1, 2)
  r2c_c(1:3, 1:5, 1:6)
  r2c_c(1:3, numeric(), 1:6)
  r2c_c(numeric())
  dat <- subset(mtcars, am == 1 & carb > 1, c(cyl, hp, wt))
  group_exec(r2c_c, dat[-1L], dat[1L])

  # somewhat larger data
  r2c_c(x, y)
})
unitizer_sect("branches", {
  r2c_c2 <- r2cf(
    function(a, b, c) {
      tmp <- if(!a) {
        c(b, c)
      } else if (a >= 1) {
        if(a >= 2) c(b, c + 10)
        else c(b + 10, c)
      } else {
        c(b, c + 100)
      }
    },
    check=TRUE
  )
  # result type not identical for this one (int/num)
  r2c_c2(0, 1:3, 8:9)
  r2c_c2(1, 1:3, 8:9)
  r2c_c2(2, 1:3, 8:9)
  r2c_c2(-1, 1:3, 8:9)

  # branches and groups
  g <- rep(1:2, 3:4)
  group_exec(r2c_c2, 1:7, g, MoreArgs=list(a=0, c=-(8:9) * 1e3))
  group_exec(r2c_c2, 1:7, g, MoreArgs=list(a=1, c=-(8:9) * 1e3))
  group_exec(r2c_c2, 1:7, g, MoreArgs=list(a=2, c=-(8:9) * 1e3))
  group_exec(r2c_c2, 1:7, g, MoreArgs=list(a=-1, c=-(8:9) * 1e3))
})

