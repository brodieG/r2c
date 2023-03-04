# r2c

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
