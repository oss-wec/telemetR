library(data.table)
library(adehabitatHR)
library(maptools)
library(geojsonio)
library(leaflet)
library(readr)
library(dplyr)
source('global.R')

dat <- read_csv('../CollarData (12).csv')
df <- xyConv(dat)
kd <- SpatialPointsDataFrame(coordinates(cbind(df$long_x, df$lat_y)), data = df,
                             proj4string = CRS('+proj=longlat'))
plot(kd, pch = 21, cex = .5)


kd <- kernelUD(kd[, 2])
hr <- lapply(kd, function(x) getContours(x, c(50, 90)))
hr <- lapply(hr, function(x) geojson_json(x))

lflt <- leaflet() %>% addProviderTiles('Esri.WorldTopoMap',
                                       options = providerTileOptions(attribution = NA))
lflt %>% mapPolygons(hr) %>% mapPoints(df)

## shapefile export
hr <- correctIDs(hr)
shpOut <- Reduce(rbind, hr)
shpOut@proj4string <- CRS('+init=epsg:4326')
writePolyShape(shpOut, 'testShp_out')


## brownian bridge
traj <- to_ltraj(df); traj1 <- dplyr::filter(df, ndowid == 2630) %>% to_ltraj
bb <- estimate_bbmm(traj)
bb1 <- estimate_bbmm(traj1)

length(bb); length(bb1)
class(bb); class(bb1)

if (class(bb) == 'estUDm') {
  print('list of UDs')
} else {
  print('single UD')
}

bbBugFix <- function(bb) {
  if (class(bb) == 'estUDm') {
    v <- bb
  } else {
    v <- list(bb)
  }
  return(v)
}

tst <- bbBugFix(bb1)
hr <- lapply(bb, function(x) getContours(x, c(80, 95)))
for (i in seq_along(hr)) {
  hr[[i]]@proj4string <- CRS('+proj=utm +zone=11')
  hr[[i]] <- spTransform(hr[[i]], CRS('+proj=longlat'))
}
hr <- lapply(hr, function(x) geojson_json(x))
lflt <- leaflet() %>% addProviderTiles('Esri.WorldTopoMap',
                                       options = providerTileOptions(attribution = NA))
lflt %>% mapPolygons(hr) %>% mapPoints(df)


