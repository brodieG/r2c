source('extra/logo-base.R')
source('../website/static/script/_lib/rayrender.R')
library(rayrender)

# dev.new(width=18, height=6)
# par(mfrow=c(1,3))
# # R (this one has NA to mark the holes)
# plot(xy[[2]], type='l', xlim=0:1, ylim=0:1)
# # 2
# with(objs.r, plot(rbind(t.i, t.o), type='l', xlim=0:1, ylim=0:1))
# # C
# with(objs.r, plot(rbind(c.i, c.o), type='l', xlim=0:1, ylim=0:1))

col.adjust <- .8 * c(.5, .75, 1) # darker
C.color <-  "#7DB3DC"
C.color <- rgb(t(col2rgb(C.color) * col.adjust), maxColorValue=255)
hoop.color <- "#A8A8A8"
hoop.color <- rgb(t(col2rgb(hoop.color) * .7), maxColorValue=255)
hoop.colors <- c(hoop.color, hoop.color, C.color)
R.color <- "#1E64B6"
R.color <- rgb(t(col2rgb("#1E64B6") * col.adjust), maxColorValue=255)

depth <- .2
lf.start <- c(.5, 6, .5)
la.start <- c(.5, 0, .5)
lf.off <- c(0, 0, -2)
fov <- 15
file.base <- '~/Downloads/anim-r2c-3d2/img-000.png'
skip <- 1
start <- Sys.time()
render <- function(
  scene, lf, la, out, width=400, height=400, samples=20, fov=15
) {
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

# obj.xy <- do.call(cbind, R.xy)
# obj.xy[,2] <- 1 - obj.xy[,2]
# obj <- extruded_polygon(
#   obj.xy,
#   top=depth, bottom=0,
#   material=diffuse(color=R.color)
# )
r3d <- extruded_polygon(
  R.xy, top=depth * 1.1, bottom=0,
  material=diffuse(color=R.color),
  holes=R.starts
)
light <- sphere(
  x=.5, y=2, z=.5, radius=.05,
  material=light(
    intensity=1000, spotlight_focus=c(.5, depth * 1.1, .5), invisible=FALSE,
    spotlight_width=30, spotlight_start_falloff=30
  )
)

scene <- rbind(
  # obj,
  r3d,
  light,
  studio,
  NULL
)
out <- next_file(file.base)
# render(scene, lf=lf.start, la=la.start, out=out, fov=40, samples=20)
render(scene, lf=lf.start + c(3, 0, -3), la=la.start, out=out, fov=40, samples=20)

# Take two
# R.xy is really plotted in x-z (i.e. the y values are z values)
# left-right is +x -> -x
# bottom-top is -z -> +z
# viewer-obj is +y -> -y

depth <- .1
spacer <- .1
two.xy <- with(objs.r, rbind(t.o, t.i))
two.xy[, 1] <- -two.xy[, 1]
two.xy[, 1] <- two.xy[, 1] - mean(range(two.xy[, 1]))

R.xy2 <- cbind(-R.xy$x, R.xy$y)
R.xy2[,1] <- R.xy2[,1] - min(R.xy2[,1]) + max(two.xy[,1]) + spacer

c.xy <- with(objs.r, rbind(c.o, c.i))
c.xy[,1] <- -c.xy[,1]
c.xy[,1] <- c.xy[,1] - max(c.xy[,1])
c.xy[,1] <- c.xy[,1] + min(two.xy[,1]) - spacer

r3d <- extruded_polygon(
  R.xy2, top=depth, bottom=0,
  material=diffuse(color=R.color),
  holes=R.starts
)
two3d <- extruded_polygon(
  two.xy, top=depth, bottom=0, material=diffuse(color=hoop.color)
)
c3d <- extruded_polygon(
  c.xy, top=depth, bottom=0, material=diffuse(color=C.color)
)

light <- sphere(
  x=.5, y=2, z=.5, radius=.05,
  material=light(
    intensity=1000, spotlight_focus=c(.5, depth, .5), invisible=FALSE,
    spotlight_width=30, spotlight_start_falloff=30
  )
)
# render(scene, lf=lf.start, la=la.start, out=out, fov=40, samples=20)

# Mess with cone / spotlight

width <- height <- 400
samples <- 20
light.x.pos <- c(mean(range(R.xy2[,1])), 0, mean(range(c.xy[,1]))) * 1.05
for(focus.x in light.x.pos) {
  spot.angle <- 16
  light <- sphere(
    x=0, y=1.25, z=.85, radius=.025,
    material=light(
      # spotlight_focus=c(.657, depth, .25),
      spotlight_focus=c(focus.x, depth, .25),
      spotlight_width=spot.angle,
      spotlight_start_falloff=spot.angle,
      intensity=4000, invisible=TRUE
    )
  )
  scene <- rbind(
    xy_rect(xwidth=10, ywidth=20),
    # xz_rect(xwidth=10, zwidth=10, y=-.5),
    r3d,
    two3d,
    c3d,
    light,
    sphere(x=2, y=2, z=2, material=light(intensity=.5)),
    NULL
  )
  out <- next_file(file.base)
  render_scene(
    filename=out,
    width=width, height=height, samples=samples, scene, fov=35,
    lookfrom=c(0, 2, 1) * 2, lookat=c(0,0,.35),
    clamp_value=5,
    camera_up=c(0, 0, 1)
  )
}



