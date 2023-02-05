## Copyright (C) Brodie Gaslam
##
## This file is part of "r2c - Fast Iterated Statistic Computation in R"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 or 3 of the License.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses> for copies of the licenses.

unitizer_sect("Basic Rename", {
  call0 <- quote({
    x <- mean(x)
    mean(x)
  })
  (res0 <- r2c:::rename_call(call0))
  identical(call0, r2c:::unrename_call(res0[['x']], res0[['rn']]))
  call1 <- quote({
    x <- mean(x)
    y <- mean(x)
    mean(x) + mean(y)
  })
  (res1 <- r2c:::rename_call(call1))
  identical(call1, r2c:::unrename_call(res1[['x']], res1[['rn']]))
})
unitizer_sect("Control Structs", {
  call2 <- quote({
    y <- x
    if(a) x <- mean(x)
    list(a, b, x, y)
  })
  (res2 <- r2c:::rename_call(call2))
  identical(call2, r2c:::unrename_call(res2[['x']], res2[['rn']]))

  call2a <- quote({
    y <- x
    if(a) x <- mean(x)
    else x <- mean(y)
    list(a, b, x, y)
  })
  (res2a <- r2c:::rename_call(call2a))
  identical(call2a, r2c:::unrename_call(res2a[['x']], res2a[['rn']]))

  # Nested `else if` generates one extra not strictly necessary rename, but
  # that's okay and detecting when it is necessary vs not is just make-work
  call2b <- quote({
    y <- x
    if(a) {
      x <- mean(x)
    } else if (b) {
      x <- mean(y)
    }
    list(a, b, x, y)
  })
  (res2b <- r2c:::rename_call(call2b))
  identical(call2b, r2c:::unrename_call(res2b[['x']], res2b[['rn']]))

  # Again extra okay but not necessary renames from `else if`.
  call2c <- quote({
    y <- x
    if(a) {
      x <- mean(x)
    } else if (b) {
      x <- mean(y)
    } else {
      x <- mean(x)
    }
    list(a, b, x, y)
  })
  (res2c <- r2c:::rename_call(call2c))
  identical(call2c, r2c:::unrename_call(res2c[['x']], res2c[['rn']]))

  call3 <- quote({
    for(i in z) {
      y <- mean(x)
      x <- mean(y)
    }
    list(i, x, y, z)
  })
  (res3 <- r2c:::rename_call(call3))
  identical(call3, r2c:::unrename_call(res3[['x']], res3[['rn']]))

  call4 <- quote({
    for(i in z) {
      if(a) y <- mean(x)
      else x <- mean(y)
      list(x, y)
    }
    list(i, x, y, z)
  })
  (res4 <- r2c:::rename_call(call4))
  identical(call4, r2c:::unrename_call(res4[['x']], res4[['rn']]))

  call4a <- quote({
    while(i < y + z) {
      if(a) y <- mean(x)
      else x <- mean(y)
      i <- i + 1L
      list(x, y)
    }
    list(i, x, y, z)
  })
  (res4a <- r2c:::rename_call(call4a))
  identical(call4a, r2c:::unrename_call(res4a[['x']], res4a[['rn']]))

  call4b <- quote({
    repeat {
      if(a) y <- mean(x)
      else x <- mean(y)
      if((i <- i + 1L) > y) break
      list(x, y)
    }
    list(i, x, y, z)
  })
  (res4b <- r2c:::rename_call(call4b))
  identical(call4b, r2c:::unrename_call(res4b[['x']], res4b[['rn']]))

  call5 <- quote({
    for(i in z) {
      if(a) y <- mean(x)
      else {
        x <- mean(y)
        for(j in a:d) {
          y <- mean(x + y)
        }
        list(x, y, j)
      }
      list(x, y, j)
    }
    list(i, j, x, y, z)
  })
  (res5 <- r2c:::rename_call(call5))
  identical(call5, r2c:::unrename_call(res5[['x']], res5[['rn']]))


})
