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
la.start <- c(.5, 0, .5)
lf.off <- c(0, 0, -2)
fov <- 15
file.base <- '~/Downloads/anim-r2c-3d/img-000.png'
skip <- 1
start <- Sys.time()
render <- function(scene, lf, la, out, width=400, height=400, samples=20, fov=15) {
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
    depth=-1.001,
    width=4, height=2, curvature=1
  ),
  angle=c(-90, 0, 0)
)
for(i in seq(1, length(coords.all), by=skip)) {
# for(i in length(coords.all)) {
  writeLines(sprintf("\rFrame %03d ellapsed %f", i, Sys.time() - start))

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
  light <- sphere(x=2, y=5, z=-7, material=light(intensity=90))

  scene <- rbind(
    obj,
    r3d,
    light,
    studio,
    NULL
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
lf.end <- (lf.start - c(lf.shift, 0, 0)) * c(1, 1.65, 1.65)
lf.d <- lf.end - lf.start
la.end <- la.start - c(lf.shift, 0, 0)
la.d <- la.end - la.start
start <- Sys.time()

two.xy  <- anim[[2]]
two.xy[,2] <- 1 - two.xy[,2]
two.xy[,1] <- two.xy[,1] - two.off
two.y.start <- 1

for(i in seq(1, steps2, by=skip)) {
# for(i in steps2) {
  writeLines(sprintf("\rFrame %03d ellapsed %f (zoom)", i, Sys.time() - start))
  out <- next_file(file.base)
  two3d <- extruded_polygon(
    two.xy,
    top=depth, bottom=0, z=two.y.start - inc2[i],
    material=diffuse(color=colors.all[40])
  )
  scene <- rbind(
    obj,
    r3d,
    two3d,
    studio,
    light,
    # cube(x=.4, y=.4, xwidth=.1, ywidth=.1)
    NULL
  )
  render(
    scene, lf=lf.start + inc2[i] * lf.d, la=la.start + inc2[i] * la.d, out=out
  )
}
cat(paste0(c("\r", rep(" ", getOption('width')), "\r"), collapse=""))

scene <- rbind(
  obj,
  r3d,
  two3d,
  studio,
  light,
  # cube(x=.4, y=.4, xwidth=.1, ywidth=.1)
  NULL
)
for(i in 1) {
  out <- next_file(file.base)
  render(scene, lf=lf.end+c(0, -1, 4), la=la.end+c(-.5,0,.5), out=out, fov=5)
}
