
library(jpeg)
jpg <- readJPEG('~/Downloads/C-hirez.jpg')
plot(as.raster(jpg))
hist(jpg[,,1])
C <- (jpg[,,1] > .5) + 0
# writeJPEG(C, '~/Downloads/C-hirez-2.jpg')
# C <- readJPEG('~/Downloads/C-hirez-2.jpg')
plot(as.raster(C))
rows <- lapply(split(C, row(C)), \(x) which(diff(x) != 0))
wols <- lapply(split(C, col(C)), \(x) which(diff(x) != 0))

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
C.logo.tmp <- C.logo <- row.coords[picks,]
dropped <- logical(nrow(C.logo.tmp))
kept <- seq_along(dropped)
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

repeat {
  cat(nrow(C.logo.tmp), '')
  C.logo.2 <- C.logo.tmp[c(nrow(C.logo.tmp), seq_len(nrow(C.logo.tmp)), 1L),]
  v1 <- C.logo.2[-(nrow(C.logo.2) - 1:0),]
  v2 <- C.logo.2[-c(1, nrow(C.logo.2)),]
  v3 <- C.logo.2[-(1:2),]
  v21 <- v2 - v1
  v23 <- v2 - v3
  ang <-
    pi -
    acos(rowSums(v21 * v23) /
    (sqrt(rowSums(v21^2)) * sqrt(rowSums(v23^2))))
  ang0 <- acos(
    rowSums((v2 - v1) * (v3 - v1)) /
    (sqrt(rowSums((v2 - v1) ^ 2)) * sqrt(rowSums((v3 - v1)^2)))
  )
  dist <- sin(ang0) * sqrt(rowSums(v21^2))
  drop <- ((is.na(dist) | dist < 1) & ang < .1) |
    (abs(sqrt(rowSums(v21^2))) <= sqrt(2) & sqrt(rowSums(v23^2)) <= sqrt(2))

  drop.w <- which(drop)
  if(!length(drop.w)) break
  drop.w <- drop.w[c(TRUE, diff(drop.w) > 1)]
  # We do not want to drop sequential points
  C.logo.tmp <- C.logo.tmp[-drop.w,,drop=FALSE]
  dropped[kept[drop.w]] <- TRUE
  kept <- kept[-drop.w]
}
cat('\n')
rng <- C.logo.tmp[,1] < 42
rng <- C.logo.tmp[,2]>910 & C.logo.tmp[,1]>600
cbind(C.logo.tmp, dist, ang, ang0, drop, v21, v23)[rng,]

plot(C.logo.tmp[rng,])
lines(C.logo.tmp[rng,])
points(C.logo[dropped, ], col='red')


# Now we have the coordinates, probably want to reduce to a similar number for
# the transitions.

library(svgchop)
svg <- chop(R_logo(), steps=100)
ext <- attr(svg, "extents")
plot.new()
plot.window(ext$x, rev(ext$y), asp=1)

xy <- get_xy_coords(svg)
fills <- get_fills(svg)
polypath(xy[[1]], col=fills[[1]], border=NA)
polypath(xy[[2]], col=fills[[2]], border=NA)

library(transformr)
library(tweenr)
# Transformer expects coordinates as data.table with ids for the pieces, and
# holes encoded with NA rows.

c.logo.dat <- data.frame(x=C.logo[,1], y=C.logo[,2], id=1)
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
  c(seq(hole.dat.closest, nrow(hole.dat)), seq_len(hole.dat.closest)),
]
hoop.logo.dat <- rbind(
  hoop.logo.dat.hole[seq_len(hole - 1L),], hole.dat.reord
)

animation <- tween_polygon(
  c.logo.dat, hoop.logo.dat, 'cubic-in-out', 40, id
) |> keep_state(10)

anim.s <- split(animation, animation$.frame)
x.ext <- range(animation[['x']], na.rm=TRUE)
y.ext <- range(animation[['y']], na.rm=TRUE)
plot.new()
plot.window(x.ext, rev(y.ext), asp=1)
polypath(anim.s[[1]], col='black', border=NA)
polypath(anim.s[[50]], col='grey', border=NA)

file.base <- "~/Downloads/anim-r2c/img-%04d.png"
for(i in seq_along(anim.s)) {
  png(sprintf(file.base, i), width=x.ext[2], height=y.ext[2])
  plot.new()
  plot.window(x.ext, rev(y.ext), asp=1)
  polypath(anim.s[[i]], col='black', border=NA)
  dev.off()
}

library(string2path)
d <- string2path("2", "/System/Library/Fonts/Monaco.ttf")
par(mai=numeric(4))
plot.new()
plot.window(range(d$x), range(d$y), asp=1)
fills <- get_fills(svg)
polypath(d, col='black', border=NA)
points(d, col='red')

