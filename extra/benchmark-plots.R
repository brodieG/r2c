# Code used to generate the README plots

library(ggplot2)
types <- c('Single Pass', 'Group Wise')
width <- 6
height <- 4
y.coord.mult <- 1.1

# - Sum ------------------------------------------------------------------------

graal.sum <- read.table(
  text=grep(
    "\\d+\\.\\d+", readLines('extra/benchmarks-graal-sum.txt'),
    value=TRUE
) )
Method <- c('sum(x)', 'vapply(..., sum)', 'data.table', 'collapse', 'r2c',
'FastR')
time <- c(0.043, 0.747, 0.234, 0.031, 0.056)
graal.time <- unique(cummin(graal.sum[['V3']]))
arrow.range <- c(min(graal.time), max(time))
graal.disp.max <- min(c(max(time) * y.coord.mult, max(graal.time)))

dat <- data.frame(
  Method=factor(Method, levels=Method),
  type=factor(c('Single Pass', rep('Group Wise', 5)), levels=types),
  time=c(time, min(graal.time))
)
dat.graal <- data.frame(
  Method=factor('FastR', levels=Method),
  type=factor('Group Wise', levels=types),
  time=graal.time
)
dat.both <- rbind(
  cbind(dat.graal, alpha=.2),
  cbind(dat, alpha=1)
)
dat.arrow <- data.frame(
  Method=factor('FastR', levels=Method),
  time=rev(range(arrow.range)) + diff(range(arrow.range)) * c(.5,.01),
  type=factor('Group Wise', levels=types)
)
(p <- ggplot(dat.both, aes(y=time)) +
  geom_col(aes(x=Method, alpha=I(alpha), fill=Method), position='identity') +
  geom_line(
    aes(x=I(rep(4.65, 2))),
    data=dat.arrow, arrow=arrow(type='closed', length=unit(.1, "inches")),
  ) +
  geom_text(
    x=4.75, y=mean(c(min(graal.time), graal.disp.max)),
    label=paste0(
      strwrap("GraalVM requires several iterations to \"warm\" up.", 16),
      collapse="\n"
    ),
    size=2.5,
    hjust=0, vjust=.4
  ) +
  geom_text(data=dat, aes(x=Method, label=time), vjust=-.2) +
  ggtitle("Group Sum (10MM Obs, ~1MM Groups)") +
  facet_grid(.~type, drop=TRUE, scales="free_x", space="free") +
  scale_fill_manual(values=c(r2c='#1F6ABF'), guide="none", na.value='grey35') +
  ylab("Time in Seconds") + xlab(NULL) +
  coord_cartesian(ylim=c(0, graal.disp.max))
)
ggsave("extra/time_gsum_all-vs.png", p, width=width, height=height)

# - Slope 1e6 G ----------------------------------------------------------------

graal.slope <- read.table(
  text=grep(
    "\\d+\\.\\d+", readLines('extra/benchmarks-graal-mapply.txt'),
    value=TRUE
) )
Method <- c(
'slope(x)', 'mapply(slope, ...)', 'data.table', 'collapse', 'r2c', 'FastR'
)
time <- c(0.250, 12.570 , 11.519, .247, .150)
graal.time <- unique(cummin(graal.slope[['V3']]))
graal.disp.max <- min(c(max(time) * y.coord.mult, max(graal.time)))
dat <- data.frame(
  Method=factor(Method, levels=Method),
  type=factor(c('Single Pass', rep('Group Wise', 5)), levels=types),
  time=c(time, min(graal.time))
)
dat.graal <- data.frame(
  Method=factor('FastR', levels=Method),
  type=factor('Group Wise', levels=types),
  time=graal.time
)
dat.both <- rbind(
  cbind(dat.graal, alpha=.2),
  cbind(dat, alpha=1)
)
dat.arrow <- data.frame(
  Method=factor('FastR', levels=Method),
  time=rev(range(graal.time)) + diff(range(graal.time)) * c(-.01,.01),
  type=factor('Group Wise', levels=types)
)
(p <- ggplot(dat.both, aes(y=time)) +
  geom_col(aes(x=Method, alpha=I(alpha), fill=Method), position='identity') +
  geom_line(
    aes(x=I(rep(4.7, 2))),
    data=dat.arrow, arrow=arrow(type='closed', length=unit(.1, "inches")),
  ) +
  geom_text(
    x=4.75, y=mean(c(min(graal.time), graal.disp.max)),
    label=paste0(
      strwrap("GraalVM requires several iterations to \"warm\" up.", 16),
      collapse="\n"
    ),
    size=2.5,
    hjust=0, vjust=.4
  ) +
  geom_text(data=dat, aes(x=Method, label=round(time, 2)), vjust=-.2) +
  ggtitle("Group Slope (10MM Obs, ~1MM Groups)") +
  facet_grid(.~type, drop=TRUE, scales="free_x", space="free") +
  scale_fill_manual(values=c(r2c='#1F6ABF'), guide="none", na.value='grey35') +
  ylab("Time in Seconds") + xlab(NULL))
ggsave("extra/time_glope_all-vs.png", p, width=width, height=height)

# - Slope 1e3 G ----------------------------------------------------------------

graal.slope.3 <- read.table(
  text=grep(
    "\\d+\\.\\d+", readLines('extra/benchmarks-graal-mapply-1e3.txt'),
    value=TRUE
) )
Method <- c(
'slope(x)', 'mapply(slope, ...)', 'data.table', 'collapse', 'r2c', 'FastR'
)
graal.time <- unique(cummin(graal.slope.3[['V3']]))
time <- c(0.250, 0.310, 0.359, .319, .093)
arrow.range <- c(min(graal.time), max(time))
graal.disp.max <- min(c(max(time) * y.coord.mult, max(graal.time)))

dat <- data.frame(
  Method=factor(Method, levels=Method),
  type=factor(c('Single Pass', rep('Group Wise', 5)), levels=types),
  time=c(time, min(graal.time))
)
dat.graal <- data.frame(
  Method=factor('FastR', levels=Method),
  type=factor('Group Wise', levels=types),
  time=graal.time
)
dat.both <- rbind(
  cbind(dat.graal, alpha=.2),
  cbind(dat, alpha=1)
)
dat.arrow <- data.frame(
  Method=factor('FastR', levels=Method),
  time=rev(range(arrow.range)) + diff(range(arrow.range)) * c(.5,.01),
  type=factor('Group Wise', levels=types)
)
(p <- ggplot(dat.both, aes(y=time)) +
  geom_col(aes(x=Method, alpha=I(alpha), fill=Method), position='identity') +
  geom_line(
    aes(x=I(rep(4.7, 2))),
    data=dat.arrow, arrow=arrow(type='closed', length=unit(.1, "inches")),
  ) +
  geom_text(
    x=4.75, y=mean(c(min(graal.time), graal.disp.max)),
    label=paste0(
      strwrap("GraalVM requires several iterations to \"warm\" up.", 16),
      collapse="\n"
    ),
    size=2.5,
    hjust=0, vjust=.4
  ) +
  geom_text(data=dat, aes(x=Method, label=round(time, 2)), vjust=-.2) +
  ggtitle("Group Slope (10MM Obs, ~1K Groups)") +
  facet_grid(.~type, drop=TRUE, scales="free_x", space="free") +
  scale_fill_manual(values=c(r2c='#1F6ABF'), guide="none", na.value='grey35') +
  ylab("Time in Seconds") + xlab(NULL) +
  coord_cartesian(ylim=c(0, graal.disp.max))
)
ggsave("extra/time_gslope_all_1e3-vs.png", p, width=width, height=height)


