# - Helper Functions -----------------------------------------------------------

# Given an array of n x 2 array of coordinates, and the row marking the start,
# reorder the array to start at that point and then order all remain rows in a
# next-closest order

reorder_nearest <- function(mx, i) {
  mx <- t(mx)
  point <- mx[, i]
  res <- matrix(point)
  mx <- mx[,-i]
  while(ncol(mx)) {
    i <- which.min(colSums((mx - point)^2))
    res <- cbind(res, mx[,i])
    point <- mx[, i]
    mx <- mx[,-i, drop=FALSE]
  }
  t(res)
}
# Find the longest stretch and add points interpolating

interpolate_longest <- function(x, n) {
  a <- t(as.matrix(x[1:2]))
  longest <- which.max(
    colSums((a[, -ncol(a), drop=FALSE] - a[, -1, drop=FALSE])^2)
  )
  interp <- rbind(
    seq(a[1, longest], a[1, longest + 1], length.out=n),
    seq(a[2, longest], a[2, longest + 1], length.out=n)
  )
  t(
    cbind(
      a[,seq_len(longest - 1), drop=FALSE],
      interp,
      if(longest + 1 < ncol(a)) a[,seq(longest + 2, ncol(a)), drop=FALSE]
  ) )
}
# Reduce observations to target obs count by iteratively removing the points
# with least error from the line connecting the prior and next obs.  We're being
# lazy and literally doing this one by one, taken the worst each time, with no
# guarantee this will lead to the overall best reduction.

reduce_points <- function(x, n) {
  min.y <- min(x[,2]) # hack to prevent deletion of the bottom part of the two
  while(nrow(x) > n) {
    cat("\r     \r", nrow(x))
    e <-  embed(seq_len(nrow(x)), 3)
    v1 <- x[e[, 3],]
    v2 <- x[e[, 2],]
    v3 <- x[e[, 1],]
    ang0 <- acos(
      rowSums((v2 - v1) * (v3 - v1)) /
      (sqrt(rowSums((v2 - v1) ^ 2)) * sqrt(rowSums((v3 - v1)^2)))
    )
    dist <- sin(ang0) * sqrt(rowSums((v2 - v1)^2))
    dist[is.na(dist)] <- 0
    dist[v2[,2] == min.y] <- Inf
    x <- x[-(which.min(dist) + 1), ]
  }
  x
}

# - Blob of Blurgh  ------------------------------------------------------------

library(jpeg)
jpg <- readJPEG('~/Downloads/C-hirez.jpg')
# plot(as.raster(jpg))
hist(jpg[,,1])
C <- (jpg[,,1] > .5) + 0
# writeJPEG(C, '~/Downloads/C-hirez-2.jpg')
# C <- readJPEG('~/Downloads/C-hirez-2.jpg')
# plot(as.raster(C))
rows <- lapply(split(C, row(C)), \(x) which(diff(x) != 0))
cols <- lapply(split(C, col(C)), \(x) which(diff(x) != 0))

# Keep only the transitions that are either first, last, or more than 100px from
# prior transitions.

heal_jagged <- function(x) {
  if(!length(x) %in% c(0,2,4))
    x[c(1, which(diff(x[-length(x)]) > 50) + 1L, length(x))]
  else x
}
rows2 <- lapply(rows, heal_jagged)
which(!lengths(rows2) %in% c(0, 2, 4))
rows2[[614]] <- rows2[[614]][-4]

cols2 <- lapply(cols, heal_jagged)
which(!lengths(cols2) %in% c(0, 2, 4))
plot(lengths(cols2))
cols2[which(lengths(cols2[1:100]) > 2)] <-
  lapply(cols2[which(lengths(cols2[1:100]) > 2)], "[", c(1,4))
bad.down <- which(lengths(cols2) == 2 & seq_along(cols2) > 900)

cols2[bad.down] <- replicate(length(bad.down), integer(), simplify=FALSE)
plot(lengths(cols2))

# Generate coordinates from the row/column data

row.coords <- do.call(
  rbind,
  lapply(
    seq_along(rows2),
    \(x) cbind(rep(x, length(rows2[[x]])), rows2[[x]])
  )
)
# Order the coordinates correctly (doing only rows since that seems good enough)

dat <- t(row.coords)
picked <- logical(ncol(dat))
picks <- rep(NA_integer_, length(picked))
picks[1L] <- 1L
pick <- 1L
i <- 1L

while(!all(picked)) {
  # get all candidates that are near our last pick on a row basis
  near <- which(!picked & abs(dat[1L,] - dat[1L, pick]) < 10)
  if(!length(near)) stop("Logic error, nothing near")
  dist.2 <- (dat[,near,drop=FALSE] - dat[,pick])^2
  pick <- near[which.min(dist.2[1L,] + dist.2[2L,])]
  picked[pick] <- TRUE
  picks[i] <- pick
  i <- i + 1
}
C.logo <- row.coords[picks,]
C.logo[,1] <- (C.logo[,1] - min(C.logo[,1])) / diff(range(C.logo[,1]))
C.logo[,2] <- (C.logo[,2] - min(C.logo[,2])) / diff(range(C.logo[,2]))
C.logo.tmp <- C.logo

dropped <- logical(nrow(C.logo.tmp))
kept <- seq_along(dropped)

# We're going to cheat and drop things that are in the connections between the
# inside and outside of the C.  To do this compute closest y distance to next
# point, and anything further than threshold dropped.

safe <- C.logo.tmp[,1] < .350 | C.logo.tmp[,1] > .630 | C.logo.tmp[,2] < .600
C.logo.tmp <- C.logo.tmp[
  safe |
  (
    c(TRUE, abs(C.logo.tmp[-1,2] - C.logo.tmp[-nrow(C.logo.tmp),2]) < .003) &
    c(abs(C.logo.tmp[-1,2] - C.logo.tmp[-nrow(C.logo.tmp),2]) < .003, TRUE)
  ),
]
end.cand.i <- unique(
  which(abs(C.logo.tmp[-1,2] - C.logo.tmp[-nrow(C.logo.tmp),2]) > .02) + 1,
  which(abs(C.logo.tmp[-1,2] - C.logo.tmp[-nrow(C.logo.tmp),2]) > .02)
)
end.cand <- end.cand.i[
  C.logo.tmp[end.cand.i, 1] > .350 &  C.logo.tmp[end.cand.i, 1] < .650 &
  C.logo.tmp[end.cand.i, 2] > .500
]
end.i <- rep(end.cand, each=2)-0:1

C.inside <- C.logo.tmp[end.i[1]:end.i[4],2:1]
C.outside <- reorder_nearest(C.logo.tmp[-(end.i[1]:end.i[3]),2:1], 1367)
# points(C.inside, col='red')
# plot(C.logo)
# points(C.logo[1,,drop=F], col='red')
# polypath(C.logo, col='yellow', border=NA)

# Get rid of any points that do not deflect very much from the preceding or next
# point.  In particular this is so we don't have anything in the connections
# between the inside and outside part of the C.
#
# Compute angle between 1st and 2nd and 1st and 3rd
# Use angle to compute perpendicular from 1st to 3rd to 2nd.
# Iteratively remove things.
#
# This is not entirely right, btw, but likely close enough it's okay

# repeat {
#   cat(nrow(C.logo.tmp), '')
#   C.logo.2 <- C.logo.tmp[c(nrow(C.logo.tmp), seq_len(nrow(C.logo.tmp)), 1L),]
#   v1 <- C.logo.2[-(nrow(C.logo.2) - 1:0),]
#   v2 <- C.logo.2[-c(1, nrow(C.logo.2)),]
#   v3 <- C.logo.2[-(1:2),]
#   v21 <- v2 - v1
#   v23 <- v2 - v3
#   ang <-
#     pi -
#     acos(rowSums(v21 * v23) /
#     (sqrt(rowSums(v21^2)) * sqrt(rowSums(v23^2))))
#   ang0 <- acos(
#     rowSums((v2 - v1) * (v3 - v1)) /
#     (sqrt(rowSums((v2 - v1) ^ 2)) * sqrt(rowSums((v3 - v1)^2)))
#   )
#   dist <- sin(ang0) * sqrt(rowSums(v21^2))
#   drop <- ((is.na(dist) | dist < 1) & ang < .1) |
#     (abs(sqrt(rowSums(v21^2))) <= sqrt(2) & sqrt(rowSums(v23^2)) <= sqrt(2))
# 
#   drop.w <- which(drop)
#   if(!length(drop.w)) break
#   drop.w <- drop.w[c(TRUE, diff(drop.w) > 1)]
#   # We do not want to drop sequential points
#   C.logo.tmp <- C.logo.tmp[-drop.w,,drop=FALSE]
#   dropped[kept[drop.w]] <- TRUE
#   kept <- kept[-drop.w]
# }
# cat('\n')
# rng <- C.logo.tmp[,1] < 42
# rng <- C.logo.tmp[,2]>910 & C.logo.tmp[,1]>600
# cbind(C.logo.tmp, dist, ang, ang0, drop, v21, v23)[rng,]

# Now we have the coordinates, probably want to reduce to a similar number for
# the transitions.

library(svgchop)
svg <- chop(R_logo(), steps=100)
ext <- attr(svg, "extents")
plot.new()
plot.window(ext$x, rev(ext$y), asp=1)
plot.window(0:1, 0:1, asp=1)

xy <- get_xy_coords(svg)
xy <- lapply(
  xy, \(z)
    with(z,
      list(
        x=(x - ext$x[1]) / diff(ext$x),
        y=(ext$y[2] - y) / diff(ext$y)
    )
) )
#lines(xy2[[1]])
#lines(xy2[[2]])

fills <- get_fills(svg)
# polypath(xy[[1]], col=fills[[1]], border=NA)
# polypath(xy[[2]], col=fills[[2]], border=NA)

hoop.logo.dat.hole <- data.frame(x=xy[[1]]$x, y=xy[[1]]$y, id=1)

# Let's try to connect the hoop outside to the inside, by taking the last point
# of the outside, finding the closest point to the inside, re-ordering the
# inside starting at that point.

hole <- which(is.na(hoop.logo.dat.hole$x))
hole.dat <- hoop.logo.dat.hole[seq_len(nrow(hoop.logo.dat.hole)) > hole, ]
hole.dat.closest <- which.min(
  colSums(
    (rbind(hole.dat$x, hole.dat$y) - unlist(hoop.logo.dat.hole[hole - 1, 1:2]))^2
) )
hole.dat.reord <- hole.dat[
  c(seq(hole.dat.closest, nrow(hole.dat)), seq_len(hole.dat.closest - 1)),
]

hoop.inside <- unique(hole.dat.reord)
hoop.outside <- unique(hoop.logo.dat.hole[seq_len(hole - 1L),])

library(string2path)
d <- string2path("2", "/System/Library/Fonts/Monaco.ttf", tolerance=1e-5)
stopifnot(nrow(d) == 306) # tol 1e-5
par(mai=numeric(4))

d[,1] <- (d[,1] - min(d[,1])) / diff(range(d[,1]))
d[,2] <- (d[,2] - min(d[,2])) / diff(range(d[,2]))

# plot.new()
# plot.window(range(d$x), range(d$y), asp=1)
# polypath(d, col='black', border=NA)
# points(d, col='red')

# Closed path, so we drop last point to avoid math problems later
two.outer <- interpolate_longest(d[c(306, 2:143), 1:2], 40)
two.inner <- interpolate_longest(d[144:305, 1:2], 40)

# Fill in the long stretches

# For each of the three shapes we want to animate between, we need to define the
# two "sides", and make sure each has the same number of points.  So we need a
# method of reducing to a given number of points rather than to a specific
# tolerance.
#
# For the hoop that's easy as we create the break.  For the C we can cheat.  For
# the 2 we probably need to cheat too.

# * For the two:
#   * TR: 144 (top) / 143 (below)
#   * BL: 305 (top) / 306 (below)
# * For the hoop:
#   * Transition between inside and outside
# * For the C:

objs <- list(
  h.i=hoop.inside, h.o=hoop.outside,
  t.o=two.outer, t.i=two.inner,
  c.i=C.inside, c.o=C.outside
)
obs <- min(vapply(objs, nrow, 0))
objs.r <- lapply(objs, reduce_points, n=obs)

dev.off()
dev.new()
par(mfrow=c(1, 3))
plot_in_out <- function(x) {
  inside <- x[[1]]
  outside <- x[[2]]
  plot(rbind(inside, outside)[,1:2])
  points(inside, col=hsv(.2, 1, seq(.5,1,length.out=nrow(inside))))
  points(outside, col=hsv(.6, 1, seq(.5,1,length.out=nrow(outside))))
  inside <- inside[rev(seq_len(nrow(inside))), ]
  points(
    (inside[,1] + outside[,1])/2,
    (inside[,2] + outside[,2])/2
  )
}
for(i in seq_len(length(objs.r)/2)) plot_in_out(objs.r[2 * i - 1:0])

# - Animate --------------------------------------------------------------------

library(transformr)
library(tweenr)
# Transformer expects coordinates as data.table with ids for the pieces, and
# holes encoded with NA rows.

objs.r.xy <- lapply(objs.r, '[', ,1:2)
polys <- lapply(
  split(objs.r.xy, as.integer((seq_along(objs.r.xy) + 1) / 2)),
  \(x) {
    tmp <- do.call(rbind, x)
    colnames(tmp) <- c('x', 'y')
    tmp <- as.data.frame(tmp)
    cbind(tmp, id=1L)
  }
)
lines <- lapply(
  split(objs.r.xy, as.integer((seq_along(objs.r.xy) + 1) / 2)),
  \(x) {
    data.frame(
      x=(x[[1]][,1] + rev(x[[2]][,1]))/2,
      y=(x[[1]][,2] + rev(x[[2]][,2]))/2,
      id=1L
    )
  }
)

# Take 3: record the angle from one vector to the next, and the length of the
# vector, and converge such that the angle never crosses zero.
# Start angle is straight up?
# We are not handling the case where a vector goes from one side of turning
# around completely (180) to the other, crossing the preceding segment, because
# nothing is close to doing that.

steps <- 40
sigend <- 8
scale <- 500
inc <- 1/(1 + exp(seq(sigend, -sigend, length.out=steps)))
file.base <- "~/Downloads/anim-r2c/img-%04d.png"
x.ext <- y.ext <- c(0,2) * scale

anim <- polys
k <- 0
for(i in seq_len(length(anim) - 1)) {
  start <- anim[[i]]
  end <- anim[[i + 1]]
  a <- t(as.matrix(start[,1:2]))
  b <- t(as.matrix(end[,1:2]))
  a <- a[, rev(seq_len(ncol(a)))]
  b <- b[, rev(seq_len(ncol(b)))]
  va <- a[,-1] - a[,-ncol(a)]
  vb <- b[,-1] - b[,-ncol(b)]

  base.coords <- lapply(inc, \(i) a[,1] + (b[,1] - a[,1]) * i)

  va0 <- cbind(c(0, 1), va[,-ncol(va)])
  ang.a <-
    acos(colSums(va0 * va) / (sqrt(colSums(va0 ^ 2)) * sqrt(colSums(va^2)))) *
    sign(va0[1,] * va[2,] - va0[2,] * va[1,])
  ang.a[is.na(ang.a)] <- 0

  vb0 <- cbind(c(0, 1), vb[,-ncol(vb)])
  ang.b <-
    acos(colSums(vb0 * vb) / (sqrt(colSums(vb0 ^ 2)) * sqrt(colSums(vb^2)))) *
    sign(vb0[1,] * vb[2,] - vb0[2,] * vb[1,])
  ang.b[is.na(ang.b)] <- 0

  dist.a <- sqrt(colSums(va^2))
  dist.b <- sqrt(colSums(vb^2))

  # vector angles relative to previous vector
  angles <- lapply(inc, \(i, a, b) a + (b - a) * i, ang.a, ang.b)
  dists <- lapply(inc, \(i, a, b) a + (b - a) * i, dist.a, dist.b)

  coords <- Map(
    \(a, d, base) {
      v <- matrix(0:1)
      vs <- matrix(NA, nrow=2, ncol=length(a))
      # Rotate each vector relative to previous
      for(i in seq_along(a)) {
        R <- matrix(c(cos(a[i]), sin(a[i]), -sin(a[i]), cos(a[i])), 2)
        v <- R %*% v
        vs[,i] <- v
      }
      # Scale each vector by distance
      vs <- vs * rep(d, each=2)
      # Compute all the points
      rbind(cumsum(c(base[1], vs[1,])), cumsum(c(base[2], vs[2,])))
    },
    angles,
    dists,
    base.coords
  )
  coords.all <- lapply(coords, \(x) t(x * scale))
  for(j in seq_along(coords.all)) {
    png(sprintf(file.base, j + k), width=x.ext[2], height=y.ext[2])
    plot.new()
    plot.window(x.ext, y.ext, asp=1)
    # polypath(anim.s[[j]], col='black', border=NA)
    lines(coords.all[[j]], col='black')
    dev.off()
  }
  k <- k + j
}
