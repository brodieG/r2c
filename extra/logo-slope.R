source('extra/logo-base.R')

# Basic Shapes

Rxy.raw <- get_xy_coords(Rsvg)[[2]]
Rfill <- get_fills(Rsvg)[[2]]
Rxy <- lapply(Rxy.raw, function(x) x - min(x, na.rm=TRUE))
Rxy <- lapply(
  Rxy, function(x, scale) x / scale, scale=max(unlist(Rxy), na.rm=TRUE)
)
Rxy$y <- 1 - Rxy$y

C.ins <- reduce_points(C.inside, obs)
C.out <- reduce_points(C.outside, obs)

two.outer.ri
two.inner.ri

# Scale to same height

scale_mx <- function(mx1, mx2, x.shift) {
  min.x <- min(c(mx1[,1], mx2[,1]))
  mx1[,1] <- mx1[,1] - min.x
  min.y <- min(c(mx1[,2], mx2[,2]))
  mx1[,2] <- mx1[,2] - min.y
  mx2[,2] <- mx2[,2] - min.y
  max.y <- max(c(mx1[,2], mx2[,2]))
  mx <- mx1 / max.y
  mx[,1] <- mx[,1] + x.shift
  mx
}
C.ins.scale <- scale_mx(C.ins, C.out, )
C.out.scale <- scale_mx(C.out, C.ins)

Two.out.scale <- scale_mx(two.outer.ri, two.inner.ri)
Two.ins.scale <- scale_mx(two.inner.ri, two.outer.ri)

plot.new()
plot.window(0:3, 0:1, asp=1)
lines(Rxy, col='blue')
lines(C.ins.scale, col='green')
lines(C.out.scale, col='green')
lines(Two.ins.scale, col='yellow')
lines(Two.out.scale, col='yellow')



