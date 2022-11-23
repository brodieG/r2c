library(r2c)


wiexe_len <- function(i, x, w, by, align='center')
  window_i_exec(r2c_len, width=w, by=by, align=align, data=x, index=i)
wiexe_sum <- function(i, x, w, by, align='center')
  window_i_exec(r2c_sum, width=w, by=by, align=align, data=x, index=i)

unitizer_sect("base index boundaries", {
  # Windows are closed on left open on right
  wiexe_len(i=0, x=0, w=1, by=1, align='left')
  wiexe_len(i=0, x=0, w=1, by=1, align='center')
  wiexe_len(i=0, x=0, w=1, by=1, align='right')

  xs <- c(42, 99)
  wiexe_len(i=0:1, x=xs, w=1, by=1, align='left')
  wiexe_len(i=0:1, x=xs, w=1, by=1, align='center')
  wiexe_len(i=0:1, x=xs, w=1, by=1, align='right')

  # Make sure the correct values are selected
  wiexe_sum(i=0:1, x=xs, w=1, by=1, align='left')
  wiexe_sum(i=0:1, x=xs, w=1, by=1, align='center')
  wiexe_sum(i=0:1, x=xs, w=1, by=1, align='right')

  wiexe_sum(i=0:1, x=xs, w=.99, by=1, align='left')
  wiexe_sum(i=0:1, x=xs, w=.99, by=1, align='center')
  wiexe_sum(i=0:1, x=xs, w=.99, by=1, align='right')

  wiexe_sum(i=0:1, x=xs, w=1.01, by=1, align='left')
  wiexe_sum(i=0:1, x=xs, w=1.01, by=1, align='center')
  wiexe_sum(i=0:1, x=xs, w=1.01, by=1, align='right')
  # Align right means 1.101, so 1 should get both values
  wiexe_sum(i=0:1, x=xs, w=1.01, by=1, align=1)

  wiexe_sum(i=0:1, x=xs, w=1, by=.99, align='left')
  wiexe_sum(i=0:1, x=xs, w=1, by=.99, align='center')
  wiexe_sum(i=0:1, x=xs, w=1, by=.99, align='right')

  wiexe_sum(i=0:1, x=xs, w=1, by=1.01, align='left')
  wiexe_sum(i=0:1, x=xs, w=1, by=1.01, align='center')
  wiexe_sum(i=0:1, x=xs, w=1, by=1.01, align='right')

})
unitizer_sect("Numeric Align", {
  # See previous section
  wiexe_len(i=0:1, x=0:1, w=1, by=1, align=0)
  wiexe_len(i=0:1, x=0:1, w=1, by=1, align=0.5)
  wiexe_len(i=0:1, x=0:1, w=1, by=1, align=1)

  show_bits(wiexe_sum(i=0:1, x=1:2, w=1, by=1, align=-1))
  show_bits(wiexe_sum(i=0:1, x=1:2, w=1, by=1, align=1.5))
  show_bits(wiexe_sum(i=0:1, x=1:2, w=1, by=1, align=2))
})


unitizer_sect("errors", {
  # mismatch index/data length
  wiexe_len(i=0, x=numeric(), w=1, by=1)
})

