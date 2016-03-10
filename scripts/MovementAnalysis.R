library(data.table)
library(ggplot2)
library(sp)
library(lubridate)
library(dplyr)
library(viridis)
library(ggthemes)

#########################################################
# MOVEMENT PARAMETERS CALCULATIONS                      #
#########################################################
dat <- fread("Collars.csv")
df <- dat[dat$ndowid == 292, ]
df <- dat[dat$ndowid %in% c(292, 831), ]

coord_conv <- function(df, conversion = 'utm') {
  df <- df[complete.cases(df[, .(long_x, lat_y)])]
  conv <- SpatialPoints(cbind(as.numeric(df$long_x), as.numeric(df$lat_y)),
                        proj4string = CRS('+proj=longlat'))
  conv <- as.data.frame(spTransform(conv, CRS('+proj=utm +zone=11')))
  colnames(conv) <- c('x', 'y')
  df <- cbind(df, conv)
  return(df)
}

move.dist <- function(x, y) {
  dist <- c(0, sqrt((x[-1] - x[-length(x)])**2 + 
                      (y[-1] - y[-length(y)])**2))
  return(dist)
}

move.r2n <- function(x, y) {
  r2n <- (x - x[1])**2 + (y - y[1])**2
  return(r2n)
}

move.dt <- function(time) {
  time <- as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S")
  dt <- c(0, unclass(time[-1]) - unclass(time[-length(time)]))
  return(dt)
}
# move.dt(df$timestamp)[25]
# unclass(move.dt(df$timestamp)[1]) - unclass(move.dt(df$timestamp)[2])
# class(df$timestamp[2])

move.speed <- function(dist, time) {
  speed <- (dist / 1000) / (time / 3600)
  return(speed)
}

df <- coord_conv(df)
df[, dist := move.dist(x, y), by = ndowid]
df[, sig.dist := cumsum(dist), by = ndowid]
df[, R2n := move.r2n(x, y), by = ndowid]
df[, dt := move.dt(timestamp), by = ndowid]
df[, speed := move.speed(dist, dt), by = ndowid]
df[, hr := hour(timestamp), by = ndowid]
df[, mth := month(timestamp), by = ndowid]
df[, d.mth := days(timestamp), by = ndowid]
d <- df

# grouping the functions in one data.table call

df[, ':=' (dist = move.dist(x, y),
           R2n = move.r2n(x, y),
           sig.dist = cumsum(move.dist(x, y))), by = ndowid]
df[, date := as.Date(timestamp)]
d <- df

## ESTIMATING CHANGE IN NSD TO PINPOINT DATES OF LARGE MOVEMENTS
dR2n <- c(df$R2n[-length(df$R2n)] - df$R2n[-1], 0)
sR2n <- dR2n / sd(dR2n)
d[, d.R2n  := dR2n]
d[, s.R2n := sR2n]
d$timestamp <- as.POSIXct(d$timestamp)
ggplot(d, aes(timestamp, d.R2n)) + geom_point()
ggplot(d, aes(s.R2n)) + geom_histogram() 
  geom_vline(xintercept = c(s, -s, s*2, -s*2, s*3, -s*3, s*4, -s*4, s*5, -s*5, s*6, -s*6))
s <- sd(d$d.R2n)
ggplot(d, aes(timestamp, R2n)) + geom_line()

mgrt <- d[s.R2n < -6 | s.R2n > 6, ] 

ggplot() + geom_line(dat = d, aes(x = timestamp, y = R2n)) +
  geom_vline(xintercept = c(unclass(mgrt$timestamp)))

mgrt$timestamp <- unclass(mgrt$timestamp)

ggplot() + #geom_line(dat = d, aes(x = timestamp, y = R2n)) +  
  geom_point(mgrt, aes(x = timestamp, y = R2n))  
  
class(mgrt$timestamp)

###############################################################################
#                   Movement Figures for Muliple Collars                      #
###############################################################################
movement_eda <- function(dat, plot_var, type = 'line') {
  color_pal <- c('royalblue4', 'firebrick4', 'wheat4', 'mediumorchid4', 'springgreen4')
  
  p <- ggplot(dat, aes(group = ndowid, color = factor(ndowid), fill = factor(ndowid)))
  if(type == 'histogram'){
    p <- p + geom_histogram(aes_string(x = plot_var))
  } else if (type == 'line'){
    p <- p + geom_line(aes_string(x = 'date', y = plot_var), size = .75)
  } else if (type == 'point'){
    p <- p + geom_point(aes_string(x = 'date', y = plot_var), size = 1.5)
  }
  p <- p + facet_wrap(~ndowid, scales = 'free', ncol = 1) +
           scale_color_manual(values = color_pal) + 
           scale_fill_manual(values = color_pal) +
           theme(panel.background = element_rect(fill = 'white'),
                 plot.background = element_rect(fill = 'white'),
                 panel.grid.major.x = element_line(color = 'grey90', size = .5),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_line(color = 'grey90', size = .5),
                 panel.grid.minor.y = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(color = 'grey50', size = 14),
                 axis.text.x = element_text(color = 'grey50', size = 10),
                 axis.text.y = element_text(color = 'grey50', size = 10),
                 axis.ticks = element_blank(),
                 strip.background = element_blank(),
                 strip.text = element_text(color = 'grey50', size = 12))
  return(p)
}

move_eda <- function(dat, plot_var, type = 'point', color = 'royalblue4') {
  p <- ggplot(dat, aes(group = ndowid, color = factor(ndowid)))
  p <- p + geom_point(aes_string(x = 'date', y = plot_var), size = .75)
  p <- p + facet_wrap(~ndowid, scales = 'free_x', ncol = 1)
  return(p)
}

move_eda(df, 'dist')
movement_eda(dat = df[df$ndowid == 831], plot_var = 'dist', color = 'springgreen4', type = 'point')

movement_eda(df, 'dist', 'point')

###############################################################################
#                           Testing with 5 animals                            #
###############################################################################
df <- dat[dat$ndowid %in% c(292, 831, 898, 900, 906), ]
df <- dat[dat$ndowid == 292, ]
df <- coord_conv(df)
df[, ':=' (dist = move.dist(x, y),
           R2n = move.r2n(x, y),
           sig.dist = cumsum(move.dist(x, y))), by = ndowid]

dist <- move.dist(df$x, df$y)
df[, ':=' (dist = move.dist(x, y),
           R2n = move.r2n(x, y),
           sig.dist = cumsum(move.dist(x, y))), by = ndowid]

df[, date := as.Date(timestamp)]
str(df)

p <- movement_eda(df, 'dist', 'histogram')
p + scale_x_sqrt()
grid.arrange(movement_eda(df, 'R2n', 'line'), 
  movement_eda(df, 'dist', 'point'), ncol = 2)

###############################################################################
#                                 Heatmap                                     #
###############################################################################
dat <- fread("Collars.csv", nrows = 100000)
unique(dat$ndowid)
df <- dat
df <- dat[dat$ndowid == 900, ]
nrow(df)
df <- coord_conv(df)
df[, ':=' (dist = move.dist(x, y),
           mth = month(timestamp, label = T, abbr = T),
           hr = hour(timestamp)),
           by = ndowid]
str(df)
df <- as.data.frame(d)
df <- df %>% 
  group_by(mth, hr) %>% 
  summarize(m.dist = mean(dist),
            t.dist = sum(dist),
            sd.dist = sd(dist))

ggplot(df, aes(x = hr, y = mth, fill = m.dist)) +
  geom_tile(color = 'white', size = .1) +
  scale_fill_viridis() +
  scale_x_continuous(breaks = seq(0, 23, 4)) +
  coord_equal() +
  theme_tufte() +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )
  
ggplot(df, aes(x = hr, y = mth, fill = sd.dist)) +
  geom_tile(color = 'white', size = .1) +
  scale_fill_viridis() +
  scale_x_continuous(breaks = seq(0, 23, 4)) +
  coord_equal() +
  theme_tufte() +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )  

####################
# BOXPLOT BY HOURS #
####################
dat <- fread("Collars.csv")
dat <- fread("Collars.csv", nrows = 100000)
unique(dat$ndowid)
df <- dat[dat$ndowid == 2594, ]
df <- dat[dat$ndowid == 900, ]
df <- coord_conv(df)
df[, ':=' (dist = move.dist(x, y),
           mth = month(timestamp, label = T, abbr = T),
           hr = hour(timestamp)),
   by = ndowid]
str(df)
df <- as.data.frame(df)
df <- df %>% 
  group_by(hr) %>% 
  summarize(m.dist = mean(dist),
            t.dist = sum(dist),
            sd.dist = sd(dist))

d <- d[speed <= 20]

ggplot(df, aes(x = hr, y = log(dist), group = hr)) +
  geom_boxplot(outlier.shape = 95) +
  theme_tufte()

ggplot(d, aes(x = hr, y = sqrt(speed), group = hr)) +
  geom_boxplot(outlier.shape = 95) +
  facet_wrap(~mth) +
  theme_tufte()

ggplot(d, aes(x = sqrt(speed))) + geom_histogram()
summary(d$speed)

ggplot(d, aes(x = dt, y = speed)) + geom_point()


#########################################
# PLOTTING ALL FIGURES FOR PRESENTATION #
#########################################
dat <- fread('Collars.csv')
df <- dat[ndowid %in% c(4085, 4086, 3494, 1576), ]
df <- coord_conv(df)
df$timestamp <- fasttime::fastPOSIXct(df$timestamp)
df[, dist := move.dist(x, y), by = ndowid]
df[, sig.dist := cumsum(dist), by = ndowid]
df[, R2n := move.r2n(x, y), by = ndowid]
df[, dt := move.dt(timestamp), by = ndowid]
df[, speed := move.speed(dist, dt), by = ndowid]
df[, hr := hour(timestamp), by = ndowid]
df[, mth := month(timestamp), by = ndowid]
df[, d.mth := days(timestamp), by = ndowid]
str(df)
d <- df

g1 <- movement_eda(df[ndowid == 1140, ], 'sig.dist')
g2 <- movement_eda(df[ndowid == 1140, ], 'R2n')
g3 <- movement_eda(df[ndowid == 1140, ], 'dist', 'point')
g5 <- movement_eda(df[ndowid == 1145, ], 'sig.dist')
g6 <- movement_eda(df[ndowid == 1145, ], 'R2n')
g7 <- movement_eda(df[ndowid == 1145, ], 'dist', 'point')
glist <- list(g1, g2, g3)
g <- do.call('grid.arrange', c(glist, ncol = 1))
plist <- list(g5, g6, g7)
p <- do.call('grid.arrange', c(plist, ncol = 1))
grid.arrange(glist, plist, ncol = 2)

grid.arrange(g1, g2, g3, g5, g6, g7, layout_matrix = rbind(c(1, 5),
                                                           c(2, 6),
                                                           c(3, 7)))

movement_eda <- function(dat, plot_var, type = 'line', color) {
  
  if (missing(color)) {
    color_pal <- ggthemes::gdocs_pal()(20)
  } else {
    color_pal <- color
  }

  p <- ggplot(dat, aes(group = ndowid, color = factor(ndowid), fill = factor(ndowid)))
  if(type == 'histogram'){
    p <- p + geom_histogram(aes_string(x = plot_var))
  } else if (type == 'line'){
    p <- p + geom_line(aes_string(x = 'timestamp', y = plot_var), size = .75)
  } else if (type == 'point'){
    p <- p + geom_point(aes_string(x = 'timestamp', y = plot_var), size = 1.5)
  }
  p <- p + facet_wrap(~ndowid, scales = 'free', ncol = 1) +
    scale_color_manual(values = color_pal) + 
    scale_fill_manual(values = color_pal) +
    theme(panel.background = element_rect(fill = 'white'),
          plot.background = element_rect(fill = 'white'),
          panel.grid.major.x = element_line(color = 'grey90', size = .5),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = 'grey90', size = .5),
          panel.grid.minor.y = element_blank(),
          legend.position = 'none',
          axis.title.x = element_blank(),
          axis.title.y = element_text(color = 'grey50', size = 14),
          axis.text.x = element_text(color = 'grey50', size = 10),
          axis.text.y = element_text(color = 'grey50', size = 10),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(color = 'grey50', size = 12))
  return(p)
}

pal <- ggthemes::gdocs_pal()(5)

grid.arrange( 
  movement_eda(df[ndowid == 1576, ], 'sig.dist', color = pal[2]),
  movement_eda(df[ndowid == 1576, ], 'R2n', color = pal[2]),
  movement_eda(df[ndowid == 1576, ], 'dist', 'point', color = pal[2]),
  ncol = 1)

############################
# HEATMAP FOR PRESENTATION #
############################
dat <- fread("Collars.csv", nrows = 100000)
d <- df[ndowid == 1576, ]
d$day.m <- day(d$timestamp)
d.hm <- d %>% 
  group_by(mth, hr) %>% 
  summarize(m.dist = mean(dist),
            m.speed = mean(sqrt(speed)))

d.hm.d <- d %>% 
  group_by(hr, day.m, mth) %>% 
  summarize(m.dist = mean(dist),
            m.speed = mean(sqrt(speed)))

## HEATMAP OF DISTANCE TRAVELLED
ggplot(d.hm, aes(x = hr, y = mth, fill = m.dist)) +
  geom_tile(color = 'white', size = .1) +
  scale_fill_viridis(name = 'Mean Distance') +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  scale_y_continuous(breaks = seq(1, 12, 1)) +
  labs(list(x = 'Hour of day', y = 'Month', title = 'Heatmap of mean distance travelled (1576)')) +
  coord_equal() +
  theme_tufte() +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

## HEATMAP OF SPEED
ggplot(d.hm, aes(x = hr, y = mth, fill = m.speed)) +
  geom_tile(color = 'white', size = .1) +
  scale_fill_viridis(name = 'sqrt(Speed)') +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  scale_y_continuous(breaks = seq(1, 12, 1)) +
  labs(list(x = 'Hour of day', y = 'Month', title = 'Heatmap of speed (1576)')) +
  coord_equal() +
  theme_tufte() +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

ggplot(d.hm.d[mth == 1], aes(x = hr, y = day.m, fill = m.speed, group = mth)) +
  geom_tile(color = 'white') +
  facet_wrap(~mth) +
  scale_fill_viridis(name = 'sqrt(Speed)') +
  scale_x_continuous(breaks = seq(0, 23, 4)) +
  scale_y_continuous(breaks = seq(1, 31, 2)) +
  labs(list(x = 'Hour of day', y = 'Day of month', title = 'Heatmap of speed (1576)')) +
  coord_equal() +
  theme_tufte() +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

dat <- fread('AllCollars.csv')

glist <- lapply(1:12, function(i) 
ggplot(d.hm.d[mth == i], aes(x = hr, y = day.m, fill = m.speed, group = mth)) +
  geom_tile(color = 'white') +
  facet_wrap(~mth) +
  scale_fill_viridis(name = 'sqrt(Speed)') +
  scale_x_continuous(breaks = seq(0, 23, 4)) +
  scale_y_continuous(breaks = seq(1, 31, 2)) +
  labs(list(x = 'Hour of day', y = 'Day of month', title = 'Heatmap of speed (1576)')) +
  coord_equal() +
  theme_tufte() +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )
)

do.call('grid.arrange', c(glist))

?lapply
glist
