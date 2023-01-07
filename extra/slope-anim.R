slope <- function(x, y)
  sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2)

library(r2c)
set.seed(1)
n <- 500
w <- 20
pw <- w * 2
offset <- -w/4
x <- cumsum(c(0, runif(n - 1)))
y <- cumsum(c(0, runif(n - 1) - .5))

# The irony I haven't implemented yet what would allow me to do this easily.

x.max <- ceiling(max(x)) + w/2
x.min <- -w/2
by <- mean(diff(x))

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
w.vals <- roll_list(
  cbind,
  x, y, pos=x, start=x.min, end=x.max, by=by, w=w
)
w.vals2 <- roll_list(
  cbind,
  x, y, pos=x, start=x.min + offset, end=x.max - offset, by=by, w=pw
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
par(bg='white', mai=numeric(4))
show.lines <- 50
colors <- rainbow(show.lines)
slope.x <- matrix(numeric(), nrow=2)
slope.y <- matrix(numeric(), nrow=2)
for(i in seq_along(w.anchor)) {
  plot.new()
  x.i <- w.vals[[i]][,1]
  y.i <- w.vals[[i]][,2]
  x.j <- w.vals2[[i]][,1]
  y.j <- w.vals2[[i]][,2]
  y.u <- mean(y.i)
  x.u <- mean(x.i)
  if(is.na(y.u)) y.u <- 0
  if(is.na(x.u)) x.u <- 0
  slope.i <- slope(x.i, y.i)
  x.plot <- w.anchor[i] + c(-w + offset, w + offset)
  y.plot <- y.u + y.rng * c(-1, 1)/2
  plot.window(x.plot, y.plot)
  slope.x <- cbind(
    slope.x,
    c(w.anchor[i] + c(-w,w)/2)
  )
  slope.y <- cbind(
    slope.y,
    c(
      y.u - (w/2 + (x.u - w.anchor[i])) * slope.i,
      y.u + (w/2 - (x.u - w.anchor[i])) * slope.i
    )
  )
  in.frame <- slope.x[2,] > w.anchor[i] - w
  in.frame <- in.frame & seq_along(in.frame) > length(in.frame) - show.lines
  slope.x <- slope.x[, in.frame, drop=FALSE]
  slope.y <- slope.y[, in.frame, drop=FALSE]
  line.n <- ncol(slope.x)
  color.i <- seq(show.lines, show.lines - line.n + 1, by=-1)
  if(!all(is.na(slope.y[1,])))
    matlines(slope.x, slope.y, type='l', col=colors[color.i], lty=1)
  # if(!is.na(slope.i)) abline(w.anchor[i], slope.i)
  points(x.j, y.j, col='grey')
  points(x.i, y.i)
  Sys.sleep(.0125)
}

