## Copyright (C) Brodie Gaslam
##
## This file is part of "r2c - A DSL for Fast Statistic Computation in R"
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

unitizer_sect("Basic Symbol Copy", {
  # External symbol
  call1a <- quote(x)
  pp1a <- r2c:::preprocess(call1a, optimize=TRUE)
  r2c:::clean_call(pp1a[['call.processed']])

  # External symbol
  call1a1 <- quote(x <- x)
  pp1a1 <- r2c:::preprocess(call1a1, optimize=TRUE)
  r2c:::clean_call(pp1a1[['call.processed']])

  r2c:::pp_clean(quote({x}))

  # No vcopy because used in non-passive call
  r2c:::pp_clean(quote(mean(x)))

  # Rebound external symbol
  call1b <- quote({z <- x; x})
  pp1b <- r2c:::preprocess(call1b, optimize=TRUE)
  r2c:::clean_call(pp1b[['call.processed']])

  call1c <- quote({z <- x; z})
  pp1c <- r2c:::preprocess(call1c, optimize=TRUE)
  r2c:::clean_call(pp1c[['call.processed']])

  # Rebound external not returned
  call1c1 <- quote({z <- x; y})
  pp1c1 <- r2c:::preprocess(call1c1, optimize=TRUE)
  r2c:::clean_call(pp1c1[['call.processed']])

  # Multiple bindings
  call1d <- quote({z <- y <- x; z})
  pp1d <- r2c:::preprocess(call1d, optimize=TRUE)
  r2c:::clean_call(pp1d[['call.processed']])

  # Overwritten binding is not vcopyied (i.e. don't vcopy y)
  r2c:::pp_clean(quote({z <- y; z <- x; z}))

  # Nested passive
  call1e <- quote({z <- {y <- x}; x})
  pp1e <- r2c:::preprocess(call1e, optimize=TRUE)
  r2c:::clean_call(pp1e[['call.processed']])

  call1e1 <- quote({z <- {y <- x}; z})
  pp1e1 <- r2c:::preprocess(call1e1, optimize=TRUE)
  r2c:::clean_call(pp1e1[['call.processed']])

  call1e2 <- quote({z <- {w; y <- x}; z})
  pp1e2 <- r2c:::preprocess(call1e2, optimize=TRUE)
  r2c:::clean_call(pp1e2[['call.processed']])

  call1e3 <- quote({z <- {w; y <- x}; w})
  pp1e3 <- r2c:::preprocess(call1e3, optimize=TRUE)
  r2c:::clean_call(pp1e3[['call.processed']])
})
unitizer_sect("Detect Computed Val Return", {
  call2a <- quote(mean(x))
  pp2a <- r2c:::preprocess(call2a, optimize=TRUE)
  r2c:::clean_call(pp2a[['call.processed']])

  call2b <- quote(x <- mean(x))
  pp2b <- r2c:::preprocess(call2b, optimize=TRUE)
  r2c:::clean_call(pp2b[['call.processed']])

  call2c <- quote({x <- mean(x); x})
  pp2c <- r2c:::preprocess(call2c, optimize=TRUE)
  r2c:::clean_call(pp2c[['call.processed']])

  call2d <- quote({y <- mean(x); y})
  pp2d <- r2c:::preprocess(call2d, optimize=TRUE)
  r2c:::clean_call(pp2d[['call.processed']])

  call2e <- quote({y <- mean(x); {z <- y}})
  pp2e <- r2c:::preprocess(call2e, optimize=TRUE)
  r2c:::clean_call(pp2e[['call.processed']])

  # Passive/Assign chains work correctly
  call2f <- quote({
    x <- {y <- mean(x); z}
    y <- x
  })
  pp2f <- r2c:::preprocess(call2f, optimize=TRUE)
  r2c:::clean_call(pp2f[['call.processed']])

  # redundant vcopy gets cleaned up
  call2g <- quote({
    x <- {y <- mean(x); y}
    y <- x
  })
  pp2g <- r2c:::preprocess(call2g, optimize=TRUE)
  r2c:::clean_call(pp2g[['call.processed']])
})
unitizer_sect("Basic if/else", {
  call3a <- quote(if(a) x else y)
  pp3a <- r2c:::preprocess(call3a, optimize=TRUE)
  r2c:::clean_call(pp3a[['call.processed']])

  # No need for `y <- vcopy(y)`
  call3a1 <- quote(if(a) {y <- x; y})
  r2c:::pp_clean(call3a1)

  # Previously bound symbols without rebinding shouldn't need vcopy
  call3b <- quote({
    y <- mean(x)
    if(a) x else y
  })
  pp3b <- r2c:::preprocess(call3b, optimize=TRUE)
  r2c:::clean_call(pp3b[['call.processed']])

  call3b1 <- quote({
    x <- y <- mean(x)
    if(a) x else y
  })
  pp3b1 <- r2c:::preprocess(call3b1, optimize=TRUE)
  r2c:::clean_call(pp3b1[['call.processed']])

  # Rebinding should not require copy unless binding used again
  call3c <- quote({
    x <- y <- mean(x)
    if(a) y <- x else y
  })
  pp3c <- r2c:::preprocess(call3c, optimize=TRUE)
  r2c:::clean_call(pp3c[['call.processed']])

  # But potentially returned external always
  call3c1 <- quote({
    x <- y <- mean(x)
    if(a) {y <- x; z} else y
  })
  pp3c1 <- r2c:::preprocess(call3c1, optimize=TRUE)
  r2c:::clean_call(pp3c1[['call.processed']])

  # No copies because assigned to symbol unused after branch so we can rely on
  # the return values that are safe.
  call3c2 <- quote({
    x <- y <- mean(x)
    if(a) {y <- x; y} else y
  })
  pp3c2 <- r2c:::preprocess(call3c2, optimize=TRUE)
  r2c:::clean_call(pp3c2[['call.processed']])

  # One copy for external symbol, none for `y` b/c unused after branch
  call3c2a <- quote({
    y <- mean(x)
    if(a) {y <- x; y} else y
  })
  pp3c2a <- r2c:::preprocess(call3c2a, optimize=TRUE)
  r2c:::clean_call(pp3c2a[['call.processed']])

  # Copies because assigned symbol used after branch
  call3c2b <- quote({
    x <- y <- mean(x)
    if(a) {y <- x; y} else y
    y
  })
  pp3c2b <- r2c:::preprocess(call3c2b, optimize=TRUE)
  r2c:::clean_call(pp3c2b[['call.processed']])

  # Copies (included added to else) because rebound symbol is used
  call3c3 <- quote({
    x <- y <- mean(x)
    if(a) {y <- x; z} else z
    y
  })
  pp3c3 <- r2c:::preprocess(call3c3, optimize=TRUE)
  r2c:::clean_call(pp3c3[['call.processed']])

  # Candidates reset due to re-assignment (only second vcopied).  Problem we
  # have here is that the the merge re-introduces old candidates if they were
  # not cleared in one branch.
  call3d1 <- quote({
    if(a) y <- x
    if(b) y <- x
    y
  })
  r2c:::pp_clean(call3d1)

  # Even local assignments need to be balanced across both branches
  call3e1 <- quote({
    x <- y <- mean(z)
    if(a) x <- mean(w) else y <- mean(u)
    x + y
  })
  r2c:::pp_clean(call3e1)

  # Don't balance unused symbol
  call3e2 <- quote({
    x <- y <- mean(z)
    if(a) x <- mean(w) else y <- mean(u)
    x
  })
  r2c:::pp_clean(call3e2)

  # Don't balance symbols assigned in both branches
  call3e2 <- quote({
    x <- mean(z)
    if(a) x <- mean(w) else x <- mean(u)
    x
  })
  r2c:::pp_clean(call3e2)
  call3e3 <- quote({
    if(a) x <- mean(w) else x <- mean(u)
    x
  })
  r2c:::pp_clean(call3e3)
  call3e4 <- quote({if(a) z <- x else z <- y; z})
  r2c::pp_clean(call3e4)

  # Example of why we can't re-use pre-if/else r2c allocations
  call3f1 <- quote({
    x <- mean(y)
    if(a) {
      y <- x
    } else {
      x <- mean(x)
      z <- mean(y)
      y <- x
    }
    y + z
  })
  # In this case we don't need to vcopy z
  call3f2 <- quote({
    x <- mean(y)
    z <- if(a) {
      y <- x
    } else {
      x <- mean(x)
      z <- mean(y)
      y <- x
    }
    y + z
  })
  r2c:::pp_clean(call3f1)
  # But here we do because it is returned
  call3f3 <- quote({
    x <- mean(y)
    if(a) {
      y <- x
    } else {
      x <- mean(x)
      z <- mean(y)
      y <- x
    }
    if(b) y else z
  })
  r2c:::pp_clean(call3f3)
})
unitizer_sect("Nested", {
  call4a <- quote({
    if(a) z
    else if (if(b) x else y) w
    else u
  })
  r2c:::pp_clean(call4a)

  # Nesting that actually used vcopy
  call4b <- quote({
    y <- mean(w)
    if(a) z
    else if (b) {if(c) x else y}
    else u
  })
  r2c:::pp_clean(call4b)

  # Nesting in if-test (this is illegal)
  call4c1 <- quote(if(x <- mean(y)) x else y)
  r2c:::pp_clean(call4c1)

  # Nested assignments later used
  call4c4 <- quote({
    y <- mean(z)
    if(a) {if(b){if(y) x else w <- z} else x <- y }
    w + x
  })
  r2c:::pp_clean(call4c4)
})
