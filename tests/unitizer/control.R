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

x1 <- 1
x2 <- c(1, 2)

r2cq(if(a) x1 else x2)
r2cq(if(a) x1)
r2cq(if(a) x1 else x2)

call1 <- quote(if(a) x else y)
pp1 <- r2c:::preprocess(call1, optimize=TRUE)
pp1[['call.processed']]
r2c:::clean_call(pp1[['call.processed']])

call2 <- quote({
  x <- mean(z)
  if(a) x else y
})
pp2 <- r2c:::preprocess(call2, optimize=TRUE)
pp2[['call.processed']]
r2c:::clean_call(pp2[['call.processed']])

call3 <- quote({
  y <- mean(z)
  if(a) x else y
})
pp3 <- r2c:::preprocess(call3, optimize=TRUE)
pp3[['call.processed']]
r2c:::clean_call(pp3[['call.processed']])

# Shared bindings, in this case it might have been okay not to vcopy.
call4 <- quote({
  x <- y <- mean(z)
  if(a) x else y
})
pp4 <- r2c:::preprocess(call4, optimize=TRUE)
pp4[['call.processed']]
r2c:::clean_call(pp4[['call.processed']])

# Test the lack of requirement to vcopy if the encumbered symbol is neither
# assigned nor last.


# Nesting, note nesting in the test clause does not require vcopy because the
# value is not last
call5 <- quote({
  if(a) z
  else if (if(b) x else y) w
  else u
})
pp5 <- r2c:::preprocess(call5, optimize=TRUE)
pp5[['call.processed']]
r2c:::clean_call(pp5[['call.processed']])

# Nesting that actually used vcopy
call5a <- quote({
  y <- mean(w)
  if(a) z
  else if (b) {if(c) x else y}
  else u
})
pp5a <- r2c:::preprocess(call5a, optimize=TRUE)
pp5a[['call.processed']]
r2c:::clean_call(pp5a[['call.processed']])

# Presumably `check` runs on the pre-processed expression and thus not subject
# to the if/else substitution?
r2cq(if(a) x1 else x2, check=TRUE)

# No else, and encumbered symbol should require a vcopy added to the else
call6 <- quote({
  z <- x <- y
  if(mean(y) > .5) x <- mean(y) / 2
  x
})
pp6 <- r2c:::preprocess(call6, optimize=TRUE)
pp6[['call.processed']]
r2c:::clean_call(pp6[['call.processed']])

call6a <- quote({
  z <- x <- y
  if(mean(y) > .5) x <- mean(y) / 2
  x + z
})
pp6a <- r2c:::preprocess(call6a, optimize=TRUE)
pp6a[['call.processed']]
r2c:::clean_call(pp6a[['call.processed']])

call6b <- quote({
  z <- x <- y
  if(mean(y) > .5) x <- mean(y) / 2
  x + z
})
pp6a <- r2c:::preprocess(call6a, optimize=TRUE)
pp6a[['call.processed']]
r2c:::clean_call(pp6a[['call.processed']])

# Should only need to copy `y` once
call6c <- quote({
  z <- x <- y
  x
})
pp6c <- r2c:::preprocess(call6c, optimize=TRUE)
pp6c[['call.processed']]
r2c:::clean_call(pp6c[['call.processed']])


call7 <- quote({
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
pp7 <- r2c:::preprocess(call7, optimize=TRUE)
pp7[['call.processed']]
r2c:::clean_call(pp7[['call.processed']])

# No else, length mismatch
y <- 1:2
r2cq({
  x <- y
  if(mean(y) > .5) x <- mean(y) / 2
})
# No else, length mismatch but unused so irrelevant

test <- TRUE
y <- 1
r2cq({
  x <- if(test) y
  y
})

# With else, okay to mismatch first value
r2cq({
  x <- z
  x <- if(test) y else y + 1
  x
})(z = 1:2, y=1, test=TRUE)

# With else, no length mistmatch

x <- y
r2cq(if(mean(y) > .5) x <- mean(y) / 2)

# Check that vcopy happens correctly
f <- r2cq({if(a) {x <- y} else {x <- z}; x})
get_r_code(f)

# Bound symbol discrepancy

r2cq({if(a) x <- y}; x)  # not-ok
r2cq({if(a) x <- y}; y)  # ok


