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

  # Make sure logic that cleares previous candidates is correct
  warning('exercise clearing prior bindings')

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

  # In theory no copies because assigned to symbol unused after branch,
  # but the forward pass can't tell because it's not last, but the revpass adds
  # the copy because the symbol is last.
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
})



# # Nesting, note nesting in the test clause does not require vcopy because the
# # value is not last
# call5 <- quote({
#   if(a) z
#   else if (if(b) x else y) w
#   else u
# })
# pp5 <- r2c:::preprocess(call5, optimize=TRUE)
# pp5[['call.processed']]
# r2c:::clean_call(pp5[['call.processed']])
# 
# # Nesting that actually used vcopy
# call5a <- quote({
#   y <- mean(w)
#   if(a) z
#   else if (b) {if(c) x else y}
#   else u
# })
# pp5a <- r2c:::preprocess(call5a, optimize=TRUE)
# pp5a[['call.processed']]
# r2c:::clean_call(pp5a[['call.processed']])
# 
# # Presumably `check` runs on the pre-processed expression and thus not subject
# # to the if/else substitution?
# r2cq(if(a) x1 else x2, check=TRUE)
# 
# # No else, and encumbered symbol should require a vcopy added to the else
# call6 <- quote({
#   z <- x <- y
#   if(mean(y) > .5) x <- mean(y) / 2
#   x
# })
# pp6 <- r2c:::preprocess(call6, optimize=TRUE)
# pp6[['call.processed']]
# r2c:::clean_call(pp6[['call.processed']])
# 
# call6a <- quote({
#   z <- x <- y
#   if(mean(y) > .5) x <- mean(y) / 2
#   x + z
# })
# pp6a <- r2c:::preprocess(call6a, optimize=TRUE)
# pp6a[['call.processed']]
# r2c:::clean_call(pp6a[['call.processed']])
# 
# call6b <- quote({
#   z <- x <- y
#   if(mean(y) > .5) x <- mean(y) / 2
#   x + z
# })
# pp6a <- r2c:::preprocess(call6a, optimize=TRUE)
# pp6a[['call.processed']]
# r2c:::clean_call(pp6a[['call.processed']])
# 
# 
# # This will error at runtime unless x is zero length
# call7 <- quote(if(a) y <- x)
# pp7 <- r2c:::preprocess(call7, optimize=TRUE)
# r2c:::clean_call(pp7[['call.processed']])
# 
# # Also runtime error, if/else vcopy not needed because target not used outside
# # of branch, but needed for return symbol.
# call7a <- quote({if(a) y <- x; x })
# pp7a <- r2c:::preprocess(call7a, optimize=TRUE)
# r2c:::clean_call(pp7a[['call.processed']])
# 
# 
# call7b <- quote({
#   if(a) y <- x
#   y
# })
# pp7b <- r2c:::preprocess(call7b, optimize=TRUE)
# r2c:::clean_call(pp7b[['call.processed']])
# 
# call7c <- quote({
#   x <- mean(x)
#   if(a) y <- x
#   y
# })
# pp7c <- r2c:::preprocess(call7c, optimize=TRUE)
# r2c:::clean_call(pp7c[['call.processed']])
# 
# call7d <- quote({
#   if(a) {
#     x <- mean(x)
#     y <- x
#   }
#   y
# })
# pp7d <- r2c:::preprocess(call7d, optimize=TRUE)
# r2c:::clean_call(pp7d[['call.processed']])
# 
# 
# call7 <- quote({
# x <- mean(y)
# if(a) {
#   y <- x
# } else {
#   x <- mean(x)
#   z <- mean(y)
#   y <- x
# }
# y + z
# })
# pp7 <- r2c:::preprocess(call7, optimize=TRUE)
# r2c:::clean_call(pp7[['call.processed']])
# 
# # No else, length mismatch
# y <- 1:2
# r2cq({
#   x <- y
#   if(mean(y) > .5) x <- mean(y) / 2
# })
# # No else, length mismatch but unused so irrelevant
# 
# test <- TRUE
# y <- 1
# r2cq({
#   x <- if(test) y
#   y
# })
# 
# # With else, okay to mismatch first value
# r2cq({
#   x <- z
#   x <- if(test) y else y + 1
#   x
# })(z = 1:2, y=1, test=TRUE)
# 
# # With else, no length mistmatch
# 
# x <- y
# r2cq(if(mean(y) > .5) x <- mean(y) / 2)
# 
# # Check that vcopy happens correctly
# f <- r2cq({if(a) {x <- y} else {x <- z}; x})
# get_r_code(f)
# 
# # Bound symbol discrepancy
# 
# r2cq({if(a) x <- y}; x)  # not-ok
# r2cq({if(a) x <- y}; y)  # ok
# 
# 
