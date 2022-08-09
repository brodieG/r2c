# We're trying to test compiling, loading, and calling a function

.onLoad <- function(libname, pkgname) {
  .Call(R2C_assumptions)
  IX <<- .Call(R2C_constants)
}
.onUnload <- function(libpath) {
  library.dynam.unload("r2c", libpath)
}
