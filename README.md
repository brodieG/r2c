# fapply

**Experimental**.  May never see the light of day.

A DSL for fast numerical calculations on groups.

R code using a small subset of R functions and operating exclusively on numeric
vectors could in theory be converted to C and compiled to native instructions.
The standard arithmetic operators and basic statistics would be supported, and
the user could use any combination of these to create their own statistics.
Additionally, a "runner" will be provided to allow the native code to be called
with different groups of the data without incurring the R evaluation overhead
for each group.

Conceptually this is similar to what e.g. `data.table`'s gforce, which for
common statistics such as `mean(x)` skips the R function completely and instead
runs native instructions on each group.  The primary difference is that with
`fapply` it will be possible to compose complex statistics from the supported
functions, whereas gforce is limited to single function single symbol
expressions like `mean(x)`.

This also bears some resemblance to `{inline}` and `{Rcpp}` in as much as there
is a need to compile C code, but unlike those the user needs no experience
whatsoever with C or C++.

