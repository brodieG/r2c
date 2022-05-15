# We're trying to test compiling, loading, and calling a function

.onUnload <- function(libpath) {
  library.dynam.unload("r2c", libpath)
}
