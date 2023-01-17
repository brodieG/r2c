# Note we've made changes since this was originally written, such as making sure
# we preserve the aspect ratio of the R logo
source('extra/logo-base.R')

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
col.adjust <- .8 * c(.5, .75, 1) # darker
C.color <-  "#7DB3DC"
C.color <- rgb(t(col2rgb(C.color) * col.adjust), maxColorValue=255)
hoop.color <- "#A8A8A8"
hoop.color <- rgb(t(col2rgb(hoop.color) * .7), maxColorValue=255)
hoop.colors <- c(hoop.color, hoop.color, C.color)
R.color <- "#1E64B6"
R.color <- rgb(t(col2rgb("#1E64B6") * col.adjust), maxColorValue=255)

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
# R isn't centered to begin with
coords.R <- coords.R.end <- do.call(cbind, R.xy)
R.x.r <- range(coords.R[,1])
R.x.off <- (1 - diff(R.x.r)) / 2
coords.R.end[,1] <- coords.R[,1] - R.x.r[1] + R.x.off - 1 -.55
coords.R.diff <- coords.R.end - coords.R
coords.R.all <- replicate(n=length(coords.all), coords.R.end, simplify=FALSE)
for(i in seq_along(inc)) coords.R.all[[i]] <- coords.R + coords.R.diff * inc[i]
two.off <- .8

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

# plot(t(t(coords.R.end) - c(.55,0)), type='l', asp=1, xlim=c(-1.5,1))
# lines(t(t(coords.all[[40]]) - c(.80, 0)), col='purple')
# lines(t(t(coords.all[[80]]) - c(0, 0)), col='yellow')

