# minimum convex polygon methods. Multiple contours and multiple individuals.

library(adehabitatHR)
library(leaflet)
library(sp)
library(data.table)
library(geojsonio)

# loading data and subsetting
dat <- fread('Collars.csv')
df <- dat[ndowid %in% c(1132, 1135), ]
df <- coord_conv(df)

# what is the difference between SpatialPoints and coordinates.
cp <- SpatialPoints(df[, .(x, y)], CRS('+proj=utm +zone=11'))
coordinates(df) <- df[, .(x, y)]

cp <- mcp(df[, 2], percent = 95)
plot(cp)
df.cp <- as.data.frame(cp)
mcp.area(df[, 2], unin = 'm', unout = 'ha')
?mcp

for (i in seq_along(cp@polygons)) {
  print(i)
}


## estimate multiple MCPs
df <- dat[dat$ndowid %in% c(1139, 1140)]
df <- coord_conv(df)

spdf <- SpatialPointsDataFrame(coordinates(cbind(df$x, df$y)), data = df,
                               proj4string = CRS('+proj=utm +zone=11'))
spdf[, 2]   # id variable
cp <- mcp(spdf[, 2], percent = 99)
cp <- spTransform(cp, CRS('+proj=longlat'))
gj <- geojsonio::geojson_json(cp)
gj <- geojson_list(cp)

leaflet() %>% addTiles() %>% 
  addCircleMarkers(lng = df$long_x, lat = df$lat_y) %>% 
  addGeoJSON(gj)

# extract polygons
p1 <- cp[cp$id == '1139', ]
p2 <- cp[cp$id == '1140', ]

p1gj <- geojson_json(p1)
p2gj <- geojson_json(p2)
ud_list <- list(p1gj, p2gj)
names(ud_list) <- c('1139', '1140')

leaflet() %>% addTiles() %>% 
  addCircleMarkers(lng = df$long_x, lat = df$lat_y) %>% 
  addGeoJSON(ud_list[[2]])

## encapsulating
ids <- cp$id
gj_list <- vector("list", length(cp$id))
for (i in seq_along(ids)) {
  poly <- cp[cp$id == ids[1]]
  poly <- geojson_json(poly)
  gj_list[[i]] <- poly
}
names(gj_list) <- ids

# all together (as in application)
df <- dat[dat$ndowid %in% c(1139, 1140)]
df <- coord_conv(df)
spdf <- SpatialPointsDataFrame(coordinates(cbind(df$x, df$y)), data = df,
                               proj4string = CRS('+proj=utm +zone=11'))
cp <- mcp(spdf[, 2], percent = 99)
cp <- spTransform(cp, CRS('+proj=longlat'))
ids <- as.character(cp$id)
hr <- vector("list", length(cp$id))
for (i in seq_along(ids)) {
  print(i)
  print(ids[i])
  poly <- cp[cp$id == ids[i]]
  poly <- geojson_json(poly)
  hr[[i]] <- poly
}
names(hr) <- ids
print(hr)
leaflet() %>% addTiles() %>% 
  addCircleMarkers(lng = df$long_x, lat = df$lat_y) %>% 
  addGeoJSON(gj_list[[1]]) %>% 
  addGeoJSON(gj_list[[2]])
