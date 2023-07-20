
unitizer_sect("vecrec", {
  # group size always larger
  r2c:::size_vecrec(list(list(2L), list(1:2)), gmax=2, gmin=1)
  # group size larger and constant
  r2c:::size_vecrec(list(list(2L), list(1:2)), gmax=2, gmin=2)
  # ambiguous
  r2c:::size_vecrec(list(list(2L), list(1:2)), gmax=2, gmin=0)
  # constant size always larger
  r2c:::size_vecrec(list(list(5L), list(1:2)), gmax=2, gmin=1)
})
unitizer_sect("eqlen", {
  # Not guaranteed equal
  r2c:::size_eqlen(list(list(3L), list(1:2)), gmax=2, gmin=1)
  # Guaranteed equal
  r2c:::size_eqlen(list(list(3L), list(1:2)), gmax=1, gmin=1)
})
unitizer_sect("valid size", {
  # Bad (needs to be lists in list)
  r2c:::valid_size_input(list(1))
  # Bad (needs to be lists in list)
  r2c:::valid_size_input(list(list(1), 1:2))
  # Good
  r2c:::valid_size_input(list(list(1)))
  # Good
  r2c:::valid_size_input(list(list(1), list(1:2)))
  # Bad (NA)
  r2c:::valid_size_input(list(list(NA), list(1:2)))
  # Bad (negative)
  r2c:::valid_size_input(list(list(-1), list(1:2)))
})
