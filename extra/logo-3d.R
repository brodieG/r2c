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

lf.start <- c(.5, 6, 1.5)
la.start <- c(.5, 0, .5)

start <- Sys.time()
stop()
for(i in seq(1, length(coords.all), by=1)) {
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
  # light <- sphere(x=5, y=5, z=5, material=light(intensity=100))
  light <- sphere(x=0, y=0, z=sqrt(75), material=light(intensity=80))

  scene <- dplyr::bind_rows(
    obj,
    r3d,
    # ray.logo,
    # walls,
    # light,
    # cube(x=.4, y=.4, xwidth=.1, ywidth=.1)
  )
  out <- next_file('~/Downloads/anim-r2c-3d/img-000.png')
  if(TRUE) {
    render_preview(
      scene, 
      # fov=fov,
      fov=15,
      width=600, height=600,
      # width=1200, height=1200,
      lookfrom=lf.start,
      lookat=la.start,
      # lookfrom=c(-3, 0, .2),
      # lookat=c(0, -.38, 0),
      light_direction=c(0, -1, -3),
      filename=out
    )
  }
  else {
    render_scene(
      scene,
      fov=fov,
      # fov=11,
      width=720, height=400, samples=500,
      lookfrom=c(0, .5, 6),
      lookat=c(0, 0, 0),
      filename=out,
      clamp_value=5,
      # debug_channel="normals"
    )
  }
}
cat(paste0(c("\r", rep(" ", getOption('width')), "\r"), collapse=""))
out <- next_file('~/Downloads/anim-r2c-3d/img-000.png')
render_preview(
  scene, 
  # fov=fov,
  fov=30,
  width=600, height=600,
  # width=1200, height=1200,
  lookfrom=lf.start - c(1, 0, 0),
  lookat=la.start - c(1, 0, 0),
  # lookfrom=c(-3, 0, .2),
  # lookat=c(0, -.38, 0),
  light_direction=c(0, -1, -3),
  filename=out
)
