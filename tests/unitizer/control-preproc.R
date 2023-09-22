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
  r2c:::pp_clean(call1a)

  # External symbol
  call1a1 <- quote(x <- x)
  r2c:::pp_clean(call1a1)
  r2c:::pp_clean(quote({x}))

  # No vcopy because used in non-passive call
  r2c:::pp_clean(quote(mean(x)))

  # Rebound external symbol
  call1b <- quote({z <- x; x})
  r2c:::pp_clean(call1b)

  call1c <- quote({z <- x; z})
  r2c:::pp_clean(call1c)

  # Rebound external not returned
  call1c1 <- quote({z <- x; y})
  r2c:::pp_clean(call1c1)

  # Multiple bindings
  call1d <- quote({z <- y <- x; z})
  r2c:::pp_clean(call1d)

  # Overwritten binding is not vcopyied (i.e. don't vcopy y)
  r2c:::pp_clean(quote({z <- y; z <- x; z}))

  # Nested passive
  call1e <- quote({z <- {y <- x}; x})
  r2c:::pp_clean(call1e)

  call1e1 <- quote({z <- {y <- x}; z})
  r2c:::pp_clean(call1e1)

  call1e2 <- quote({z <- {w; y <- x}; z})
  r2c:::pp_clean(call1e2)

  call1e3 <- quote({z <- {w; y <- x}; w})
  r2c:::pp_clean(call1e3)

  # Empty braces
  call1f <- quote({})
  r2c:::pp_clean(call1f)
})
unitizer_sect("Detect Computed Val Return", {
  call2a <- quote(mean(x))
  r2c:::pp_clean(call2a)

  call2b <- quote(x <- mean(x))
  r2c:::pp_clean(call2b)

  call2c <- quote({x <- mean(x); x})
  r2c:::pp_clean(call2c)

  call2d <- quote({y <- mean(x); y})
  r2c:::pp_clean(call2d)

  call2e <- quote({y <- mean(x); {z <- y}})
  r2c:::pp_clean(call2e)

  # Passive/Assign chains work correctly
  call2f <- quote({
    x <- {y <- mean(x); z}
    y <- x
  })
  r2c:::pp_clean(call2f)

  # redundant vcopy gets cleaned up
  call2g <- quote({
    x <- {y <- mean(x); y}
    y <- x
  })
  r2c:::pp_clean(call2g)

  # Don't copy external symbol nested in computing expression
  call2h <- quote(-x)
  r2c:::pp_clean(call2h)
})
unitizer_sect("Basic if/else", {
  call3a <- quote(if(a) x else y)
  r2c:::pp_clean(call3a)

  # No need for `y <- vcopy(y)`
  call3a1 <- quote(if(a) {y <- x; y})
  r2c:::pp_clean(call3a1)

  # Don't copy external symbol nested in computing expression
  call3a2 <- quote(if(a) -x)
  r2c:::pp_clean(call3a2)

  # Strictly this case does not require a vcopy of the `y`, but it is generally
  # unsafe to try to reconcile non-branch local memory so we don't try to find
  # the special cases where it is okay.
  call3b <- quote({
    y <- mean(x)
    if(a) x else y
  })
  r2c:::pp_clean(call3b)

  # See 3b, copies not strictly necessary here but we do them anyway, but only
  # not necessary because both `x` and `y` point to same memory so no harm in
  # reconciling them.
  call3b1 <- quote({
    x <- y <- mean(x)
    if(a) x else y
  })
  r2c:::pp_clean(call3b1)

  # Again lean cautious and always copy things being returned if not local
  call3c <- quote({
    x <- y <- mean(x)
    if(a) y <- x else y
  })
  r2c:::pp_clean(call3c)

  # In this case it is not safe to not copy at all.
  call3c1 <- quote({
    x <- y <- mean(x)
    if(a) {y <- x; z} else y
  })
  r2c:::pp_clean(call3c1)

  # See 3b1
  call3c2 <- quote({
    x <- y <- mean(x)
    if(a) {y <- x; y} else y
  })
  r2c:::pp_clean(call3c2)

  # We must copy here as otherwise reconciliation would try to merge `mean(x)`
  # and a copy of `x`.
  call3c2a <- quote({
    y <- mean(x)
    if(a) {y <- x; y} else y
  })
  r2c:::pp_clean(call3c2a)

  # Test effect of using assigned symbol out of branch
  call3c2b <- quote({
    x <- y <- mean(x)
    if(a) {y <- x; y} else y
    y
  })
  r2c:::pp_clean(call3c2b)

  # Copies (included added to else) because rebound symbol is used
  call3c3 <- quote({
    x <- y <- mean(x)
    if(a) {y <- x; z} else z
    y
  })
  r2c:::pp_clean(call3c3)

  # Triggering candidate should obviate need to copy return value
  call3d0 <- quote({
    if(a) y <- x
    y
  })
  r2c:::pp_clean(call3d0)

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
  call3e3 <- quote({
    x <- mean(z)
    if(a) x <- mean(w) else x <- mean(u)
    x
  })
  r2c:::pp_clean(call3e3)
  call3e3a <- quote({
    if(a) x <- mean(z)
    else x <- mean(y)
    x
  })
  r2c:::pp_clean(call3e3a)
  call3e3b <- quote({if(a) z <- x else z <- y; z})
  r2c:::pp_clean(call3e3b)

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
  r2c:::pp_clean(call3f1)

  # Copy `x` in the else branch because we use it outside the branch and we
  # can't have two references to the same memory that needs to be reconciled.
  call3f1a <- quote({
    x <- mean(y)
    if(a) {
      y <- x
    } else {
      x <- mean(x)
      z <- mean(y)
      y <- x
    }
    y + z + x
  })
  r2c:::pp_clean(call3f1a)

  # Inline comments (see source):
  call3f2 <- quote({
    x <- mean(y)
    z <- if(a) {
      y <- x       # Need to copy both return value and `x` as both used.
    } else {
      x <- mean(x) # `x` is branch local computation.
      z <- mean(y) # `z` is reset by if/else return value, so do nothing.
      y <- x       # `x` branch local: only reconcile. Return needs copy.
    }
    y + z
  })
  r2c:::pp_clean(call3f2)
  # Even though z and y are computed by the time we get to second branch, we
  # need to copy them again because if we didn't prior uses of them would get
  # merged in the reconciliation process.
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

  # Assignment chain across `if` boundary
  call3g1 <- quote({
    x <- if(a) y <- z
    x
  })
  r2c:::pp_clean(call3g1)

  # Assignment chain across `if` boundary, no explicit return
  call3g2 <- quote({
    x <- if(a) y <- z
  })
  r2c:::pp_clean(call3g2)

  # Assignment chain across `if` boundary, with computing assignment unused
  call3h1 <- quote({
    x <- if(a) y <- mean(z)
    x
  })
  r2c:::pp_clean(call3h1)
  # Assignment chain across `if` boundary, with computing assignment used
  call3h2 <- quote({
    x <- if(a) y <- mean(z)
    x + y
  })
  r2c:::pp_clean(call3h2)
})
unitizer_sect("Nested", {
  # Nesting in both test and body
  call4a <- quote({
    if(a) z
    else if (if(b) x else y) w
    else u
  })
  r2c:::pp_clean(call4a)

  # Nesting in nested if.
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

  # Local symbol becomes non-local due to nesting; this should be fine because
  # the nested vcopying should make it again local.
  call4d1 <- quote(
    if(a) {
      x <- mean(y)
      if(b) x <- z
      x
  } )
  r2c:::pp_clean(call4d1)
  call4d2 <- quote({
    if(a) {
      x <- mean(y)
      if(b) x <- z
    }
    x
  })
  r2c:::pp_clean(call4d2)

  # Non local bindings unused
  call4d3 <- quote({
    if(a) {
      x <- mean(y)
      if(b) x <- z
    }
    z
  })
  r2c:::pp_clean(call4d3)

  # Nested binding expression used (but not actual binding)
  call4e1 <- quote(mean(if(a) x <- z else y))
  r2c:::pp_clean(call4e1)

  # Indirect return of nested
  call4f1 <-quote({
    x2 <- if(a == 1) a
    else if (a == 2) b
    else c
    x2
  })
  r2c:::pp_clean(call4f1)

  # Nested via else/if, only return value reconcile
  call4f2 <-quote({
    x2 <- if(a == 1) {
      x3 <- mean(x)
    } else if (a == 2) {
      x3 <- sum(x)
    } else {
      x3 <- a
    }
    x2
  })
  r2c:::pp_clean(call4f2)

  # Nested via else/if, check assign-reconcile
  call4f2 <-quote({
    if(a == 1) {
      x3 <- mean(x)
    } else if (a == 2) {
      x3 <- sum(x)
    } else {
      x3 <- a
    }
    x3
  })
  r2c:::pp_clean(call4f2)

  # Nested via else/if, check assign+reconcile when return used
  call4f3 <-quote({
    x2 <- if(a == 1) {
      x3 <- mean(x)
    } else if (a == 2) {
      x3 <- sum(x)
    } else {
      x3 <- a
    }
    x2 + x3
  })
  r2c:::pp_clean(call4f3)

  # Cascading of reconcile 
  call4f4 <- function(a, b, c, d) {
    if(a) {
      if (b) x <- c else x <- d
      x
    } else {
      x <- b
    }
  }

  # Additional tests in control-eval, e.g. f4b1
})
unitizer_sect("Vcopy Branch Return", {
  # `x <- mean(z)` requires vcopy when symbol is used outside of local context
  # as it cannot share an alloc with a branch return value.
  call5a1 <- quote({
    y <- if(a) {
      x <- mean(y)
      sum(y)
    } else {
      x <- mean(z)
    }
    x + y
  })
  r2c:::pp_clean(call5a1)
  # Return external symbol in one of the branches that should be copied.
  call5a2 <- quote({
    y <- if(a) {
      x <- mean(y)
      y
    } else {
      x <- mean(z)
    }
    x + y
  })
  r2c:::pp_clean(call5a2)
  # Both branches need to vcopy the assignment return
  call5b1 <- quote({
    y <-
      if(a) x <- mean(y)
      else x <- mean(z)
    x + y
  })
  r2c:::pp_clean(call5b1)
  # Neither branch needs to copy, and only need to reconcile return value
  call5b2 <- quote(
    if(a) x <- mean(y)
    else x <- mean(z)
  )
  r2c:::pp_clean(call5b2)

  # Shouldn't need copies b/c we don't use return value.
  call5b3 <- quote({
    if(a) x <- mean(y)
    else x <- mean(z)
    x
  })
  r2c:::pp_clean(call5b3)

  # Vcopy of required because last, not because of assignments
  call5b4 <- quote(
    if(a) x <- y
    else x <- z
  )
  r2c:::pp_clean(call5b4)
  # Return value is unused, but need to vcopy for external symbols
  call5b5 <- quote({
    if(a) x <- y
    else x <- z
    x
  })
  r2c:::pp_clean(call5b5)
  # Double vcopy for return and symbol
  call5b6 <- quote({
    w <-
      if(a) x <- y
      else x <- z
    x + w
  })
  r2c:::pp_clean(call5b6)

  # Branch as input to computing fun
  call5c1 <- quote(mean(if(a) x else y))
  r2c:::pp_clean(call5c1)

})
unitizer_sect('multi-assign', {
  # everything needs to be rec/vcopy, except `mean(z)` which needs rec only
  call6a1 <- quote({
    u <- if(a) {
      x <- y <- mean(z)
    } else {
      x <- y <- w
    }
    u + x + y + w
  })
  r2c:::pp_clean(call6a1)

  # Don't use if/else result, so no need for outer rec/vcopy
  call6a2 <- quote({
    if(a) {
      x <- y <- mean(z)
    } else {
      x <- y <- w
    }
    x + y + w
  })
  r2c:::pp_clean(call6a2)

  # We don't use `y`, originally we still did the recs for it to avoid the
  # complication of tracking what was and wasn't needed, but cumulative
  # changes have resulted in it seemingly being tracked correctly.  So now no
  # rec for `y` which isn't needed.
  call6a3a <- quote({
    u <- if(a) {
      x <- y <- mean(z)
    } else {
      x <- y <- w
    }
    u + x + w
  })
  r2c:::pp_clean(call6a3a)
  # Also skip rec for `r` in addition to `y`.
  call6a3a1 <- quote({
    u <- if(a) {
      x <- y <- r <- mean(z)
    } else {
      x <- y <- r <- w
    }
    u + x + w
  })
  r2c:::pp_clean(call6a3a1)
  # Here we should be able to skip one of the rec/vcopys
  call6a3b <- quote({
    u <- if(a) {
      x <- y <- mean(z)
    } else {
      x <- y <- w
    }
    u + y + w
  })
  r2c:::pp_clean(call6a3b)

  # Multi-assign but not in branch result
  call6b1 <- quote({
    u <- if(a) {
      x <- y <- mean(z)
      x
    } else {
      x <- y <- v <- w
      v
    }
    u + x + y + w
  })
  r2c:::pp_clean(call6b1)

  # Multi assign of branch result; recall we always copy branch
  # return value if it _could_ be used.  So some redundant uses below.
  call6c0 <- quote({
    v <- if(a)
      u <-
        if(b) x <- y <- mean(z)
        else x <- y <- w
    v + u + y
  })
  r2c:::pp_clean(call6c0)
  call6c1 <- quote({
    v <- if(a)
      u <-
        if(b) x <- y <- mean(z)
        else x <- y <- w
    u + y
  })
  r2c:::pp_clean(call6c1)
  call6c2 <- quote({
    v <- if(a)
      u <-
        if(b) x <- y <- mean(z)
        else x <- y <- w
    u
  })
  r2c:::pp_clean(call6c2)
})

unitizer_sect('errors', {
  call7a <- quote({
      x0 <- x + y
      if (b) r2c::rec(x0 + y)
      else r2c::rec(y + y)
  })
  r2c:::pp_clean(call7a)
})
