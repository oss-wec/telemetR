library(data.table)
library(maptools)
library(RgoogleMaps)
library(ggplot2)
library(ggmap)

dat <- fread('Collars.csv')
df <- dat[species == 'MULD', ]
df <- df[complete.cases(df[, .(lat_y, long_x)]), ]

rmel <- df
dbhs <- df
rbhs <- df
cbhs <- df
muld <- df

center <- c(lon = -117.12, lat = 38.42)
base <- get_map(location = center, zoom = 6)
pal <- ggthemes::gdocs_pal()(5)
g <- ggmap(base) +
  geom_point(data = muld, aes(x = long_x, y = lat_y), color = pal[1], size = .5) +
  geom_point(data = dbhs, aes(x = long_x, y = lat_y), color = pal[2], size = .5) +
  geom_point(data = cbhs, aes(x = long_x, y = lat_y), color = pal[3], size = .5) +
  geom_point(data = rbhs, aes(x = long_x, y = lat_y), color = pal[4], size = .5) +
  geom_point(data = rmel, aes(x = long_x, y = lat_y), color = pal[5], size = .5)
g
ggsave(g, file = "map_all.jpg")

g <- ggmap(base) +
  geom_point(data = rmel, aes(x = long_x, y = lat_y), color = pal[5], size = .5)
g
ggsave(g, file = "map_rmel.jpg")

