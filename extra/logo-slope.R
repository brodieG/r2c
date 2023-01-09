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
r2c.char.width <- 1.1

Two.out.scale <- scale_mx(two.outer.ri, two.inner.ri, r2c.char.width)
Two.ins.scale <- scale_mx(two.inner.ri, two.outer.ri, r2c.char.width)

C.ins.scale <- scale_mx(C.ins, C.out, 2 * r2c.char.width)
C.out.scale <- scale_mx(C.out, C.ins, 2 * r2c.char.width)

Rmx <- do.call(cbind, Rxy)
Rsplit <- which(is.na(Rmx[,1]))
Rmx.out.raw <- Rmx[seq_len(Rsplit - 1),]
Rmx.in.raw <- Rmx[(Rsplit + 1):nrow(Rmx),]
Rmx.out <- interpolate_threshold(Rmx.out.raw, .025)
Rmx.in <- interpolate_threshold(Rmx.in.raw, .025)

dev.off()
height <- 3
dev.new(r2c.char.width=height * r2c.char.width * 3, height=height)
par(mai=numeric(4))
plot.new()
plot.window(0:1*3, 0:1, asp=1)

lines(Rmx.out, col='blue')
lines(Rmx.in, col='blue')
lines(C.ins.scale, col='green')
lines(C.out.scale, col='green')
lines(Two.ins.scale, col='yellow')
lines(Two.out.scale, col='yellow')

points(Rmx.out, col='blue')
points(Rmx.in, col='blue')
points(C.ins.scale, col='green')
points(C.out.scale, col='green')
points(Two.ins.scale, col='yellow')
points(Two.out.scale, col='yellow')

r2c.points.raw <- rbind(
  Rmx.out, Rmx.in,
  C.ins.scale, C.out.scale,
  Two.ins.scale, Two.out.scale
)

