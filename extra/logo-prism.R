
library(rayrender)
lf.start <- c(.5, 6, .5)
la.start <- c(.5, 0, .5)
lf.off <- c(0, 0, -2)
fov <- 15

prism.xy <- cbind(
  c(-.5, 0, .5),
  c(0, sin(pi/3), 0) - .5 * tan(pi/6)
)
x.base <- seq(-1, 1, length.out=20)
y.base <- sqrt(1 - x.base^2)
circle.xy <- cbind(c(x.base, x.base), c(y.base, -y.base))
plot(circle.xy)
polypath(prism.xy, col='red')



prism <- extruded_polygon(
  prism.xy,
  top=1, bottom=0,
  # material=dielectric(color='#FFFF88'),
  material=dielectric(color='#FFFFFF'),
)
base <- cylinder(y=-.5, radius=1)

rw <- 3
gap <- .1
zshift <- .35
# light <- sphere(
#   x=10, y=5, z=0, radius=.gap
#   material=light(intensity=10000k)
# )
light <- yz_rect(
  ywidth=rw/2, zwidth=gap, x=3, y=rw/2, #z=zshift,
  material=light(intensity=400, importance_sample=FALSE),
  flipped=TRUE
)
light2 <- sphere(
  y=10, x=-4, material=light(intensity=10, importance_sample=FALSE)
)
rect1 <- yz_rect(z=rw/2 + gap/2, ywidth=rw, zwidth=rw, flipped=TRUE)
rect2 <- yz_rect(z=-rw/2 - gap/2, ywidth=rw, zwidth=rw, flipped=TRUE)
projector <- group_objects(
  group_objects(
    rbind(rect1, rect2, light), translate=c(2,0,.3)
  ),
  angle=c(0, 20, 0), order_rotation=3:1
)

scene <- rbind(
  base,
  prism,
  light2,
  # obj,
  projector,
  NULL
)

rez <- 200
render_scene(
  scene,
  # lookfrom=c(-.25, 3, .25)*40,
  lookfrom=c(0, 3, .25)*40,
  # lookfrom=c(0, 3, 6)*40,
  lookat=c(0, 0, 0),
  fov=1,
  # fov=3,
  width=rez, height=rez,
  samples=1000,
  preview=FALSE,
  # clamp_value=3,
  # debug_channel='normals'
)
