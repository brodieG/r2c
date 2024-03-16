
unitizer_sect('show_c_code', {
  c.code <- capture.output(show_c_code(r2c_sum))
  c.code
  c.code.all <- capture.output(show_c_code(r2c_sum, all=TRUE))
  # include directives contain file system references
  grep('#include', c.code.all, invert=TRUE, value=TRUE)
})
