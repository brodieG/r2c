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
  a <- t(as.matrix(x[,1:2]))
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
# Keep applying interpolate_longest until no segment is longer than `threshold`

interpolate_threshold <- function(x, threshold) {
  stopifnot(is.matrix(x), nrow(x) > 1)
  repeat {
    dists <- sqrt(rowSums((x[-1,] - x[-nrow(x),]) ^ 2))
    if(any(dists > threshold * 1.1))
      x <- interpolate_longest(x, ceiling(max(dists) / threshold) + 1)
    else break
  }
  x
}

# Reduce observations to target obs count by computing total distance of
# original path (if path is detailed enough, first every second obs is removed
# to smooth right angle pixel patterns) and interpolating along the path
# splitting up by the total distance.  We run the algorithm twice to try to
# smooth out the final spacing.

reduce_points <- function(x, n) {
  x <- x0 <- as.matrix(x[,1:2])
  stopifnot(nrow(x) > 3, nrow(x) >= n)
  # drop every second point if we can afford it
  if(nrow(x) > 2 * n) x <- x[seq(2L, nrow(x) - 1L, by=2L), ]

  for(i in 1:2) {
    xd <- x[-1,] - x[-nrow(x),]
    dist <- c(0, sqrt(rowSums(xd^2)))
    distc <- cumsum(dist)
    targets <- seq(0, distc[length(distc)], length.out=n)
    int <- findInterval(targets, distc)
    y <- x[int,]
    # Interpolate to get exact distance.  Can't do it for last point
    int2 <- int[-length(int)]
    targeti <- dist[int2 + 1]
    targetd <- targets[-length(targets)] - distc[int2]
    y[-nrow(y),] <- y[-nrow(y),] + targetd/targeti * xd[int2,,drop=FALSE]
    x <- y
  }
  x
}
# - R Logo Data ----------------------------------------------------------------

library(svgchop)
Rsvg <- chop(R_logo(), steps=40)
ext <- attr(Rsvg, "extents")
plot.new()
plot.window(ext$x, rev(ext$y), asp=1)
plot.window(0:1, 0:1, asp=1)

xy <- get_xy_coords(Rsvg)
xy <- lapply(
  xy, \(z)
    with(z,
      list(
        x=(x - ext$x[1]) / diff(ext$x),
        y=(ext$y[2] - y) / diff(ext$x)   # changed for 0.2.0 to preserve asp
    )
) )
R.starts <- which(is.na(xy[[2]]$x))
R.xy <- lapply(xy[[2]], "[", -R.starts)

#lines(xy2[[1]])
#lines(xy2[[2]])

fills <- get_fills(Rsvg)
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
# Extend the hoop so that it closes explicitly visually
hoop.outside <- hoop.outside[c(nrow(hoop.outside), seq_len(nrow(hoop.outside))),]
hoop.inside <- hoop.inside[c(seq_len(nrow(hoop.inside)), 1L),]

obs <- nrow(hoop.inside) # we know this is the smallest number of points

# - C Logo Data ----------------------------------------------------------------

library(jpeg)
jpg <- readJPEG('extra/C-hirez.jpg')
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
C.scale <- max(c(diff(range(C.logo[,1])), diff(range(C.logo[,2]))))
C.logo[,1] <- (C.logo[,1] - min(C.logo[,1])) / C.scale
C.logo[,2] <- (C.logo[,2] - min(C.logo[,2])) / C.scale
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

# Scale to R size and center on X (note x/y flipped still)
C.logo.tmp <- C.logo.tmp * max(R.xy[['y']])
# C.logo.tmp[,2] <- C.logo.tmp[,2] + (1 - max(R.xy[['x']])) / 2
C.logo.tmp[,2] <-
  C.logo.tmp[,2] - min(C.logo.tmp[,2]) + (1 - diff(range(C.logo.tmp[,2]))) / 2

C.inside <- C.logo.tmp[end.i[1]:end.i[4],2:1]
C.outside <- reorder_nearest(C.logo.tmp[-(end.i[1]:end.i[3]),2:1], 1367)

# - Number 2 -------------------------------------------------------------------

# Now we have the coordinates, probably want to reduce to a similar number for
# the transitions.

library(string2path)
# For Monaco, end points are 350:351, and 165:166
d <- string2path("2", "/System/Library/Fonts/Monaco.ttf", tolerance=1e-5)
# d <- string2path("2", "Menlo", "bold", tolerance=1e-5)
# stopifnot(nrow(d) == 290) # tol 1e-5
stopifnot(nrow(d) == 351) # tol 1e-5
par(mai=numeric(4))

d[,1] <- (d[,1] - min(d[,1])) / diff(range(d[,1]))
d[,2] <- (d[,2] - min(d[,2])) / diff(range(d[,2]))
d[,1] <- d[,1] * max(R.xy[['y']]) + (1 - max(R.xy[['y']])) / 2
d[,2] <- d[,2] * max(R.xy[['y']])

# plot.new()
# plot.window(range(d$x), range(d$y), asp=1)
# polypath(d, col='black', border=NA)
# points(d, col='red')

# Closed path, so we drop last point to avoid math problems later.
# * TR: 144 (top) / 143 (below)
# * BL: 305 (top) / 306 (below)
# Also interpolate the legs of the two

# First reduce both smooth parts of the twos
two.interp.points <- 40
two.outer.r <- reduce_points(d[2:165, 1:2], obs - (two.interp.points - 1))
two.inner.r <- reduce_points(d[166:349, 1:2], obs - (two.interp.points - 1))
# two.outer.r <- reduce_points(d[3:133, 1:2], obs - (two.interp.points - 1))
# two.inner.r <- reduce_points(d[c(134:290, 1), 1:2], obs - (two.interp.points - 1))

# add back the legs and interpolate points on them
# two.outer.ri <- interpolate_longest(
#   rbind(as.matrix(d[3, 1:2]), two.outer.r), two.interp.points
# )
# two.inner.ri <- interpolate_longest(
#   rbind(two.inner.r, as.matrix(d[2, 1:2])), two.interp.points
# )
two.outer.ri <- interpolate_longest(
  rbind(as.matrix(d[351, 1:2]), two.outer.r), two.interp.points
)
two.inner.ri <- interpolate_longest(
  rbind(two.inner.r, as.matrix(d[350, 1:2])), two.interp.points
)

# - Assemble Objects -----------------------------------------------------------

objs.r <- list(
  h.i=reduce_points(hoop.inside, obs), h.o=reduce_points(hoop.outside, obs),
  t.o=two.outer.ri, t.i=two.inner.ri,
  c.i=reduce_points(C.inside, obs), c.o=reduce_points(C.outside, obs)
)

dev.off()
dev.new()
par(mfrow=c(1, 3))
plot_in_out <- function(x) {
  inside <- x[[1]]
  outside <- x[[2]]
  plot(rbind(inside, outside)[,1:2], type='l')
  points(inside, col=hsv(.2, 1, seq(.5,1,length.out=nrow(inside))))
  points(outside, col=hsv(.6, 1, seq(.5,1,length.out=nrow(outside))))
  inside <- inside[rev(seq_len(nrow(inside))), ]
  points(
    (inside[,1] + outside[,1])/2,
    (inside[,2] + outside[,2])/2
  )
}
for(i in seq_len(length(objs.r)/2)) plot_in_out(objs.r[2 * i - 1:0])

