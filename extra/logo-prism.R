
library(rayrender)

prism.xy <- cbind(
  c(-.5, 0, .5),
  c(0, sin(pi/3), 0) - .5 * tan(pi/6)
)
x.base <- seq(-1, 1, length.out=20)
y.base <- sqrt(1 - x.base^2)
# circle.xy <- cbind(c(x.base, x.base), c(y.base, -y.base))
# plot(circle.xy)
# polypath(prism.xy, col='red')

# source('extra/logo-base.R')
source('../website/static/script/_lib/rayrender.R')

render_prism <- function(
  lcolor='#FFFFFF', refraction=1.5, rez=200, samples=1000, file.base
) {
  prism <- extruded_polygon(
    prism.xy,
    top=1, bottom=0,
    # material=dielectric(color='#FFFF88'),
    material=dielectric(color='#FFFFFF', refraction=refraction),
  )
  # base <- cylinder(y=-.01/2, radius=1.5, length=0.01)
  base <- cylinder(y=-.01/2, radius=1.25, length=0.01)
  # base <- cube(y=-.01/2, ywidth=0.01, xwidth=1.5, zwidth=1.5)
  # rim <- cylinder(
  #   y=0.25, length=.5, flipped=TRUE, capped=FALSE, phi_max=190, phi_min=150
  # )

  rw <- 3
  # gap <- .05
  gap <- .025
  zshift <- .35
  # light <- sphere(
  #   x=10, y=5, z=0, radius=.gap
  #   material=light(intensity=10000k)
  # )
  light <- yz_rect(
    ywidth=rw/2, zwidth=gap, x=3, y=rw/2, #z=zshift,
    # material=light(intensity=4000, importance_sample=FALSE,color=lcolor),
    material=light(intensity=1e7, importance_sample=FALSE,color=lcolor),
    flipped=TRUE
  )
  light2 <- sphere(
    y=8, z=4, x=0, material=light(intensity=3, importance_sample=FALSE)
  )
  rect1 <- yz_rect(z=rw/2 + gap/2, ywidth=rw, zwidth=rw, flipped=TRUE)
  rect2 <- yz_rect(z=-rw/2 - gap/2, ywidth=rw, zwidth=rw, flipped=TRUE)
  projector <- group_objects(
    group_objects(
      rbind(rect1, rect2, light), translate=c(3,0,.3)
    ),
    angle=c(0, 20, 0), order_rotation=3:1
  )

  scene <- rbind(
    base,
    # rim,
    prism,
    light2,
    projector,
    # yz_rect(x=-1, ywidth=rw, zwidth=rw, material=diffuse()),
    NULL
  )
  out <- next_file(file.base)
  render_scene(
    scene,
    # lookfrom=c(-.25, 3, .25)*40,
    # lookfrom=c(0, 3, .25)*40,
    # # This one might be a good one to demonstrate transparency might be a
    # # good angle.
    # lookfrom=c(.5, 1, 1)*80,
    lookfrom=c(.5, .7, 1)*80,
    # lookfrom=c(-1, 1, 1)*80,

    # lookfrom=c(0, 3, 6)*40,
    lookat=c(0, .3, 0),
    fov=1.2,
    # fov=3,
    width=rez, height=rez,
    samples=samples,
    preview=FALSE,
    sample_method='sobol',
    transparent_background=TRUE,
    filename=out,
    # clamp_value=3,
    # debug_channel='normals'
  )
}

# n <- 100
n <- 1
s <- 1000; rez <- 720
# s <- 1000; rez <- 200
ref.start <- 1.35
ref.end <- 1.65
rainbow <- rainbow(n)
# rainbow <- "#FFFFFF"
ref <- seq(ref.start, ref.end, length.out=n)
file.base <- '~/Downloads/renders/prism-11/img-000.png'
if(!dir.exists(dirname(file.base))) dir.create(dirname(file.base))

for(i in seq_len(n))
  render_prism(
    rainbow[i], refraction=ref[i], samples=s, file.base=file.base, rez=rez
  )

stop()

library(png)
files <- list.files(dirname(file.base), full.names=TRUE)
pngs <- lapply(files, readPNG)

# Try to minimize precision issues

while(length(pngs) > 1) {
  if(length(pngs) %% 2) {
    pngs[[length(pngs) - 1]] <-
      (pngs[[length(pngs) - 1]] + pngs[[length(pngs)]]) / 2
    pngs[[length(pngs)]] <- NULL
  }
  a <- pngs[seq(1, length(pngs), by=2)]
  b <- pngs[seq(2, length(pngs), by=2)]
  pngs <- Map(\(x, y) (x + y)/2, a, b)
}
file.base <- '~/Downloads/renders/prism-1/img-000.png'
writePNG(pngs[[1]], next_file(file.base))


stop()

render_scene(
  rbind(
    xz_rect(xwidth=10, zwidth=10),
    sphere()
  ),
  samples=100,
  width=100, height=100,
  debug_channel='normals'
)

render_scene(
  rbind(
    xz_rect(xwidth=2, zwidth=2, ),
    xz_rect(y=.1, xwidth=1, zwidth=1, material=metal()),
    sphere(y=20, material=light(intensity=20))
  ),
  lookfrom=c(.5, 3, .5),
  fov=45,
  samples=100,
  width=100, height=100,
  filename=next_file(file.base),
  transparent_background=TRUE,
)






