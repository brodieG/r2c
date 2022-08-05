# source('extra/logo.R')
source('../website/static/script/_lib/rayrender.R')

library(rayrender)

frames <- 21
stopifnot(frames%%2 != 0)
frames.start <- floor(frames/2)
frames.end <- floor(frames/2)
point.pwr <- 5
point.x <- 1
points.start <- c(0, diff(seq(0, point.x, length.out=frames.start)^point.pwr))
points.end <- rev(diff(seq(0, point.x, length.out=frames.end + 1)^point.pwr))
points.end <- points.end / points.end[1] * points.start[length(points.start)]
frame.points <- cumsum(
  c(points.start, points.start[length(points.start)], points.end)
)
x <- frame.points / max(frame.points)

# x <- (2-(cos(seq(0, pi, length.out=frames)) + 1)) * .5
depths <- x * .5
angles <- x * 90
radii <- approxfun(0:1, c(1e3, 10))(x)
# radii <- rep(1e3, 5)
fovs <- approxfun(0:1, c(11, 11))(x)
svgs <- round(
  approxfun(c(0,1, 1), c(1, steps, steps))(seq(0, 1, length.out=frames))
)
depth <- .2

lf.start <- c(.5, 6, .5)
la.start <- c(.5, 0, .6)
lf.off <- c(0, 0, -2)
fov <- 15
file.base <- '~/Downloads/anim-r2c-3d-2/img-000.png'
skip <- 1
start <- Sys.time()
render <- function(scene, lf, la, out, width=400, height=400, samples=100) {
  render_scene(
    scene,
    fov=fov,
    width=width, height=height, samples=samples,
    lookfrom=lf + lf.off,
    lookat=la,
    camera_up=c(0, 0, -1),
    filename=out,
    clamp_value=5,
    preview=FALSE
  )
}
studio <- group_objects(
  generate_studio(
    distance=-1, 
    material=diffuse(),
    # material=diffuse(checkercolor='green', checkerperiod=.5),
    depth=-1,
    width=4, height=2, curvature=1
  ),
  angle=c(-90, 0, 0)
)
for(i in seq(1, length(coords.all), by=skip)) {
  cat(sprintf("\rFrame %03d ellapsed %f", i, Sys.time() - start))
  # depth <- depths[i]
  # angle <- angles[i]
  # radius <- radii[i]
  # fov <- fovs[i]
  # nudge <- 1e-3
  # svgi <- svgs[i]
  # hoop <- t(norm[[svgi]][[1]])
  # rrr <- t(norm[[svgi]][[2]])

  obj.xy  <- coords.all[[i]]
  obj.xy[,2] <- 1 - obj.xy[,2]
  obj <- extruded_polygon(
    obj.xy,
    top=depth, bottom=0,
    material=diffuse(color=colors.all[i])
  )
  r.xy <- coords.R.all[[i]]
  r.xy[,2] <- 1 - r.xy[,2]
  r3d <- extruded_polygon(
    r.xy, top=depth * 1.1, bottom=0,
    material=diffuse(color=R.color),
    holes=R.starts
  )
  # wall.xz <- xz_rect(
  #   xwidth=10, zwidth=10, material=diffuse(),
  #   y=bottom,
  # )
  # wall.xy <- xy_rect(
  #   xwidth=3, ywidth=3, material=diffuse(), z=-depth / 2
  # )
  # floor <- sphere(
  #   z=-radius, radius=radius
  # )
  # walls <- group_objects(
  #   dplyr::bind_rows(
  #     floor
  #     # wall.xz,
  #     # wall.xy
  #   ),
  #   group_angle=c(angle, 0, 0),
  #   # pivot_point=c(0, bottom, -depth / 2)
  #   pivot_point=c(0, bottom, 0)
  # )
  light <- sphere(x=5, y=5, z=-5, material=light(intensity=100))
  # light <- sphere(x=0, y=0, z=sqrt(75), material=light(intensity=80))

  scene <- dplyr::bind_rows(
    obj,
    r3d,
    # ray.logo,
    # walls,
    light,
    studio,
    # cube(x=.4, y=.4, xwidth=.1, ywidth=.1)
  )
  out <- next_file(file.base)
  render(scene, lf=lf.start, la=la.start, out=out)
}
cat(paste0(c("\r", rep(" ", getOption('width')), "\r"), collapse=""))

# Adjust camera angle
steps2 <- 20
sigend2 <- 4
inc2 <- c(0, 1/(1 + exp(seq(sigend2, -sigend2, length.out=steps2-2))), 1)

lf.shift <- two.off + .0
lf.end <- (lf.start - c(lf.shift, 0, 0)) * c(1, 1.5, 1.5)
lf.d <- lf.end - lf.start
la.end <- la.start - c(lf.shift, 0, 0)
la.d <- la.end - la.start
start <- Sys.time()

for(i in seq(1, steps2, by=skip)) {
  cat(sprintf("\rFrame %03d ellapsed %f", i, Sys.time() - start))
  out <- next_file(file.base)
  render(
    scene, lf=lf.start + inc2[i] * lf.d, la=la.start + inc2[i] * la.d, out=out
  )
}
cat(paste0(c("\r", rep(" ", getOption('width')), "\r"), collapse=""))

two.xy  <- anim[[2]]
two.xy[,2] <- 1 - two.xy[,2]
two.xy[,1] <- two.xy[,1] - two.off
two3d <- extruded_polygon(
  two.xy,
  top=depth, bottom=0,
  material=diffuse(color=colors.all[40])
)
scene <- dplyr::bind_rows(
  obj,
  r3d,
  two3d,
  studio,
  light,
  # cube(x=.4, y=.4, xwidth=.1, ywidth=.1)
)
for(i in 1) {
  out <- next_file(file.base)
  render(scene, lf=lf.end, la=la.end, out=out)
}
