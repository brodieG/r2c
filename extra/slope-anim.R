slope <- function(x, y)
  sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2)

library(r2c)
set.seed(2)
n <- 300
w <- 20
pw <- w * 2
offset <- -w/4
x <- cumsum(c(0, runif(n - 1)))
y.raw <- runif(n - 1) - .5
y <- cumsum(c(0, y.raw + c(0, y.raw[-1])))
# y <- cumsum(c(0, rep(.5, n-1)))

# The irony I haven't implemented yet what would allow me to do this easily.

extra <- w
x.max <- ceiling(max(x)) + pw + offset
x.min <- floor(min(x)) + offset - w/2
by <- mean(diff(x)) * 2
show.lines <- 50

roll_list <- function(fun, x, y, pos, start, end, by, w) {
  i <- seq(start, end, by)
  wi <- lapply(i, \(i) which(pos >= i - w/2 & pos <= i + w/2))
  Map(fun, lapply(wi, \(i) x[i]), lapply(wi, \(i) y[i]))
}
y.rng <- max(
  unlist(
    roll_list(
      function(x, y) if(length(y) < 2) 0 else diff(range(y)),
      x, y, pos=x, start=x.min, end=x.max, by=by, w=w * 2
) ) )
# Points to compute the slope off
w.vals <- roll_list(
  cbind,
  x, y, pos=x, start=x.min, end=x.max, by=by, w=w
)
# Background points
w.vals2 <- roll_list(
  cbind,
  x, y, pos=x - offset, start=x.min, end=x.max, by=by, w=pw
)

d.centers <- vapply(w.vals, colMeans, numeric(2))
d.centers[is.na(d.centers)] <- 0
w.anchor <- seq(x.min, x.max, by)

# Want a random walk.
# * Fixed window lm centered in the middle
# * Old slopes left in increasingly faded colors (spanning screen?)
# * Plot centered in middle of window, but scale is the same
# * Points outside the window faded
# * Show 2x more points than are fit on the window so we can see them on either
#   side

# dev.off()
# dev.new(width=5, height=5)
# par(bg='white', mai=numeric(4))

colors.raw <- rainbow(show.lines)
# Desaturate colors as we get further by merging them on grey

colrgb <- col2rgb(colors.raw)
colors <- rgb(
  t(
    (colrgb - c(col2rgb("grey"))) *
    (rep(rev(seq_len(show.lines) - 1), each=3) / (show.lines - 1)) +
    c(col2rgb("grey"))
  ),
  maxColorValue=255
)
file.base <- '~/Downloads/r2c/slope-anim/img-%04d.png'
{
  slope.x <- matrix(numeric(), nrow=2)
  slope.y <- matrix(numeric(), nrow=2)

  y.u <- 0
  x.u <- w.anchor[1]
  for(i in seq_along(w.anchor)) {
    cat(sprintf("Frame %04d\r", i))
    png(sprintf(file.base, i), width=480, height=480)
    par(mai=numeric(4))
    plot.new()
    y.u.prev <- y.u
    x.u.prev <- x.u
    anc <- w.anchor[i]
    x.i <- w.vals[[i]][,1] - anc
    y.i <- w.vals[[i]][,2]
    x.j <- w.vals2[[i]][,1] - anc
    y.j <- w.vals2[[i]][,2]
    y.u <- mean(y.i)
    x.u <- mean(x.i)
    if(is.na(y.u)) y.u <- y.u.prev
    if(is.na(x.u)) x.u <- x.u.prev
    slope.i <- slope(x.i, y.i)

    x.plot <- c(offset - w, offset + w)
    y.plot <- y.u + y.rng * c(-1, 1)/2
    plot.window(x.plot, y.plot, xaxs='i', yaxs='i')
    rect(-w/2, y.plot[1], w/2, y.plot[2], col='#EEEEEE', border=NA)

    slope.x <- cbind(slope.x - by, c(-w, w) / 2)
    slope.y <- cbind(
      slope.y,
      c(y.u - (x.u + w/2) * slope.i,
      y.u + (-x.u + w/2) * slope.i)
    )
    in.frame <- seq_len(ncol(slope.x)) > length(in.frame) - show.lines
    slope.x <- slope.x[, in.frame, drop=FALSE]
    slope.y <- slope.y[, in.frame, drop=FALSE]
    line.n <- ncol(slope.x)

    if(!all(is.na(slope.y[1,])))
      matlines(
        slope.x, slope.y, type='l', col=rev(colors[seq_len(line.n)]), lty=1
      )
    # if(!is.na(slope.i)) abline(w.anchor[i], slope.i)
    points(x.j, y.j, col='grey')
    points(x.i, y.i)
    dev.off()
  }
  cat('\n')
}
# ffmpeg -pattern_type glob -i '*.png' -r 30 -pix_fmt yuv420p out.mp4

