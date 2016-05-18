dat <- read.csv('data/CollarData.csv')
library(rNDOW)
dat <- xyConv(dat)
spdf <- SpatialPointsDataFrame(coordinates(cbind(dat$x, dat$y)), 
                               data = dat, proj4string = CRS('+init=epsg:26911'))
spdf[, 3]
kd <- kernelUD(spdf[, 3])
kd95 <- getverticeshr(kd, 95)

rgdal::writeOGR(kd95, '.', 'testshp', driver = 'ESRI Shapefile')

spdf <- spTransform(kd95, CRS('+init=epsg:4326 +datum=WGS84 +proj=longlat'))
rgdal::writeOGR(spdf, 'testkml.kml', 'testkml', driver = 'KML')


library(leaflet)
bm <- leaflet() %>% addTiles()
ly <- addCircleMarkers(bm, lng = dat$long_x, lat = dat$lat_y)
ly

## new leaflet map function
mapPoints <- function(map, df) {
  df <- as.data.table(df)
  df <- df[complete.cases(df[, .(long_x, lat_y)])]
  unq_id <- unique(df$ndowid)
  pal <- rep_len(color_pal, length(unq_id))
  layers <- list()
  
  for(i in 1:length(unq_id)) {
    dat <- df[ndowid == unq_id[i]]
    map <- addPolylines(map, lng = dat$long_x, lat = dat$lat_y,
                        group = as.character(unq_id[i]),
                        color = pal[i], weight = 1) 
    map <- addCircleMarkers(map, lng = dat$long_x, lat = dat$lat_y,
                            group = as.character(unq_id[i]), color = pal[i],
                            radius = 3, stroke = FALSE,fillOpacity = .3,
                            popup = paste(sep = "<br>",
                                          paste("<b>NDOW ID:</b> ", unq_id[i]),
                                          paste("<b>timestamp:</b> ", dat$timestamp),
                                          paste("<b>LocID</b>: ", dat$locid)))
                            layers <- c(layers, as.character(unq_id[i]))
  }
  map <- addLayersControl(map, overlayGroups = layers)
  return(map)
}

mapPolygons <- function(map, geojson) {
  pal <- rep_len(color_pal, length(geojson))
  for (i in seq_along(geojson)) {
    map <- addGeoJSON(map, geojson[[i]], color = pal[i],
                      weight = 1, group = names(geojson)[i])
  }
  return(map)
}


ud <- get_mud(kd, 95)
lflt <- leaflet() %>% addTiles()
pts <- mapPoints(lflt, dat)
ply <- mapPolygons(lflt, ud)

lflt %>% mapPolygons(ud) %>% mapPoints(dat)

## geojson back to sp for export
spconv <- geojson_sp(ud[[1]])
?geojson_sp

geojson_write(ud, file = 'testjson.geojson')


for (i in seq_along(ud)) {
  
  print(names(ud)[i])
}

sdf_ply <- geojson_sp(ud[[1]])
plot(sdf_ply)
sdf_ply@polygons[[1]]@ID <- "3494"
