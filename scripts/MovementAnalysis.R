library(data.table)
library(ggplot2)
library(sp)
library(lubridate)
library(dplyr)
library(viridis)
library(ggthemes)
###############################################################################
#                     descriptive parameters for figures                      #
###############################################################################
dat <- fread("Collars.csv", nrows = 100000)
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
df[, d.mth := days_in_month(timestamp), by = ndowid]
d <- df

# grouping the functions in one data.table call

df[, ':=' (dist = move.dist(x, y),
           R2n = move.r2n(x, y),
           sig.dist = cumsum(move.dist(x, y))), by = ndowid]
df[, date := as.Date(timestamp)]
d <- df

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

###############################################################################
# BOXPLOT
###############################################################################
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
