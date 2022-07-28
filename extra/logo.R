
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
C.logo <- row.coords[picks,]
plot(C.logo)
polypath(C.logo, col='yellow', border=NA)

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
# Transformer expects coordiantes as data.table with ids for the pieces

c.logo.dat <- data.frame(x=C.logo[,1], y=C.logo[,2], id=1)
hoop.logo.dat <- data.frame(x=xy[[1]]$x, y=xy[[1]]$y, id=1)
animation <- tween_polygon(
  c.logo.dat, hoop.logo.dat, 'cubic-in-out', 40, id
) |> keep_state(10)

anim.s <- split(animation, animation$.frame)
x.ext <- range(animation[['x']], na.rm=TRUE)
y.ext <- range(animation[['y']], na.rm=TRUE)
plot.new()
plot.window(x.ext, rev(y.ext), asp=1)
polypath(anim.s[[1]], col='black', border=NA)

# file.base <- "~/Downloads/anim-r2c/img-%04d.png"
# for(i in seq_along(anim.s)) {
#   png(sprintf(file.base, i), width=x.ext[2], height=y.ext[2])
#   plot.new()
#   plot.window(x.ext, rev(y.ext), asp=1)
#   polypath(anim.s[[i]], col='black', border=NA)
#   dev.off()
# }

library(string2path)
d <- string2path("2", "/System/Library/Fonts/Monaco.ttf")
par(mai=numeric(4))
plot.new()
plot.window(range(d$x), range(d$y), asp=1)
fills <- get_fills(svg)
polypath(d, col='black', border=NA)
points(d, col='red')

