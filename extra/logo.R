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
svg <- chop(R_logo(), steps=40)
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
R.starts <- which(is.na(xy[[2]]$x))
R.xy <- lapply(xy[[2]], "[", -R.starts)

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
# Extend the hoop so that it closes explicitly visually
hoop.outside <- hoop.outside[c(nrow(hoop.outside), seq_len(nrow(hoop.outside))),]
hoop.inside <- hoop.inside[c(seq_len(nrow(hoop.inside)), 1L),]

obs <- nrow(hoop.inside) # we know this is the smallest number of points

# - C Logo Data ----------------------------------------------------------------

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

# Scale to R size and center on X

C.logo.tmp <- C.logo.tmp * max(R.xy[['y']])
C.logo.tmp[,2] <- C.logo.tmp[,2] + (1 - max(R.xy[['y']])) / 2

C.inside <- C.logo.tmp[end.i[1]:end.i[4],2:1]
C.outside <- reorder_nearest(C.logo.tmp[-(end.i[1]:end.i[3]),2:1], 1367)

# - Number 2 -------------------------------------------------------------------

# Now we have the coordinates, probably want to reduce to a similar number for
# the transitions.

library(string2path)
d <- string2path("2", "/System/Library/Fonts/Monaco.ttf", tolerance=1e-5)
stopifnot(nrow(d) == 306) # tol 1e-5
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

two.outer <- interpolate_longest(d[c(306, 2:143), 1:2], 40)
two.inner <- interpolate_longest(d[144:305, 1:2], 40)

# First reduce both smooth parts of the twos
two.interp.points <- 40
two.outer.r <- reduce_points(d[2:143, 1:2], obs - (two.interp.points - 1))
two.inner.r <- reduce_points(d[144:304, 1:2], obs - (two.interp.points - 1))

# add back the legs and interpolate points on them
two.outer.ri <- interpolate_longest(
  rbind(as.matrix(d[306, 1:2]), two.outer.r), two.interp.points
)
two.inner.ri <- interpolate_longest(
  rbind(two.inner.r, as.matrix(d[305, 1:2])), two.interp.points
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

# - Animate --------------------------------------------------------------------

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
inc <- c(0, 1/(1 + exp(seq(sigend, -sigend, length.out=steps-2))), 1)
file.base <- "~/Downloads/anim-r2c/img-%04d.png"
tol <- .1
x.ext <- y.ext <- (.5 + c(-1, 1) * 1/sqrt(2)) * scale
hoop.colors <- c("#A8A8A8", "#A8A8A8", "#7DB3DC")
R.color <- "#1E64B6"

anim <- polys
# Need to add tolerance
# Browse[2]> lim <- sqrt(2) * c(-1,1) / 2 + .5
# Browse[2]> plot(t(x), xlim=lim, ylim=lim)
#
# Browse[2]> lines(circ, col='red')

circ.degs <- seq(0, 2 * pi, length.out=100)
circ <- (cbind(sin(circ.degs), cos(circ.degs)) / sqrt(2) + .5) * scale
# plot(cbind(sin(circ.degs), cos(circ.degs)), type='l')
# lines(cbind(sin(circ.degs), cos(circ.degs))/sqrt(2), type='l')
scale_coords <- function(x) {
  r <- sqrt(1/2)
  dat <- x - .5
  # Keep shifting points from furthest towards center until either no more are
  # outside radius, or we start generating new ones.  In the latter case, step
  # back one and use scaling to make it fit
  over <- (size <- sqrt(colSums(dat ^ 2))) > r
  mult <- 1
  max.size <- Inf
  while(any(over)) {
    over.prev <- over
    max.size.prev <- max.size
    dat.prev <- dat
    max.point <- which.max(size)
    max.size <- size[max.point]
    max.shift <- dat[,max.point] * r / max.size - dat[,max.point]
    dat <- dat + (max.shift * mult)
    over <- (size <- sqrt(colSums(dat ^ 2))) > r
    if(any(over & !over.prev)) mult <- mult / 2
    if(max.size.prev <= max.size + .01) {
      dat <- dat.prev
      break
    }
  }
  # Scale if still needed b/c shifting not enough
  size <- max(c(sqrt(colSums(dat^2) * 2), 1))
  # t(dat / size + .5)
  t(dat + .5)
}
for(i in seq_len(length(anim) - 1)) {
  if(i == 1) {
    coords.all <- list()
    colors.all <- character()
  }
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
  colors.all <- c(
    colors.all, rgb(colorRamp(hoop.colors[i + 0:1])(inc), maxColorValue=255)
  )
  # scale coords so they don't exit the (0:1)*scale bounding box
  coords.all <- c(coords.all, lapply(coords, scale_coords))
}

coords.R <- coords.R.end <- do.call(cbind, R.xy)
coords.R.end[,1] <- coords.R.end[,1] - 2
coords.R.diff <- coords.R.end - coords.R
coords.R.all <- replicate(n=length(coords.all), coords.R.end, simplify=FALSE)
for(i in seq_along(inc)) coords.R.all[[i]] <- coords.R + coords.R.diff * inc[i]


for(j in seq_along(coords.all)) {
  png(
    sprintf(file.base, j),
    width=round(diff(x.ext)/2)*2, height=round(diff(y.ext)/2)*2
  )
  plot.new()
  plot.window(x.ext, y.ext, asp=1)
  polypath(coords.all[[j]] * scale, col=colors.all[j], border=NA)
  polypath(coords.R.all[[j]] * scale, col=R.color, border=NA)
  lines(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0) * scale, ncol=2, byrow=TRUE), col='red')
  lines(circ, col='blue')
  # lines(coords.all[[j]], col='black')
  dev.off()
}
# ffmpeg -pattern_type glob -i '*.png' -r 30 -pix_fmt yuv420p out.mp4
