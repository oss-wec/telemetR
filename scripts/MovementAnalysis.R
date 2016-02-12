###############################################################################
#                     descriptive parameters for figures                      #
###############################################################################
library(data.table)
library(ggplot2)
library(sp)
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

df <- coord_conv(df)
df[, dist := move.dist(x, y), by = ndowid]
df[, sig.dist := cumsum(dist), by = ndowid]
df[, R2n := move.r2n(x, y), by = ndowid]
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
#                             Testing with 5 animals                          #
###############################################################################
df <- dat[dat$ndowid %in% c(292, 831, 898, 900, 906), ]
df <- coord_conv(df)
df[, ':=' (dist = move.dist(x, y),
           R2n = move.r2n(x, y),
           sig.dist = cumsum(move.dist(x, y))), by = ndowid]
df[, date := as.Date(timestamp)]
str(df)

grid.arrange(movement_eda(df, 'R2n', 'line'), 
  movement_eda(df, 'dist', 'point'), ncol = 2)
