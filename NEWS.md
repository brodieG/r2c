# r2c

## 0.3.0

### New Features

* Subset (`[`) and subassign (`x[s] <- y`) with strictly positive indices.
* Relational (`>`, `>=`, `<`, `<=`) and equality operators (`==`, `!=`).
* Logical functions:
  * Operators (`&`, `|`, `&&`, `||`).
  * `all` and `any`.
  * `ifelse`, with the caveat the return value type is based on the combination
    of `yes` and `no` irrespective of whether one or both of them are used.
* Unary operators (`-`, `+`, and `!`).
* Vector generation:
  * `rep`
  * `seq_along`
  * `c`,
  * `numeric` (and `r2c` only `numeric_along`).
* Control structures, with significant constraints (see
  ?`r2c-control-structures`):
  * `if`/`else`/`else if`.
  * `for`
* Constant arbitrary R expressions may be embedded, with constraints (see
  ?`r2c-expression-types`).
* Iteration level interrupts (e.g. between groups, windows).
* Improved top-level documentation.

### Internal Changes

* Direct calls to "r2c_fun" no longer use `group_exec_int` internally and thus
  save the cost of allocating a dummy group vector.
* Dynamic libraries are unloaded when the "r2c_fun" object they are linked to is
  garbage collected (see `?unload_r2c_dynlibs` for limitations).
* Detect whether to use `pow` or `powl` C exponentiation functions to match
  Windows behavior.

## 0.2.0

### New Features

* Implement assignment (`<-`, `=`) and multi-line statements with braces (`{`).
* Optimize expressions by reusing repeated sub-calls instead of re-evaluating
  them (see `reuse_calls`, which can potentially be used outside of `r2c`).
* Add limited `::` support so it is possible to use the compilation functions on
  `r2c` provided R functions without `r2c` attached.
* Add `r2cf` to directly compile R functions composed of `r2c` compatible calls.

### Breaking Changes

Recall the API is unstable and may still change further over the next few
releases.

* `r2c` has been renamed `r2cl`.
* `envir` parameter to the runner functions has been dropped.  Now "r2c_fun"
  functions always evaluate in the lexical environment they were created in.  In
  the future there might be facilities to change that environment.

## 0.1.0

### New Features

* `roll*_exec` to apply `r2c` functions on sliding windows of data.
* `group_exec` accepts factor group columns.

### Breaking Changes

Recall the API is unstable and may still change further over the next few
releases.

* `group_exec` parameters re-ordered to align with `roll*_exec`.

## 0.0.1

Initial release:

* `r2c` for basic aggregation and arithmetic operations.
* `group_exec` to repeatedly apply them on groups.
