# r2c

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
