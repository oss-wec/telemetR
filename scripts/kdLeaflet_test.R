library(data.table)
library(adehabitatHR)
library(maptools)
library(geojsonio)
library(leaflet)

dat <- fread('Collars.csv')
df <- dat[ndowid %in% c(2630, 2648)]
df <- coord_conv(df)

kd <- SpatialPointsDataFrame(coordinates(cbind(df$long_x, df$lat_y)), data = df,
                             proj4string = CRS('+proj=longlat'))
kd <- kernelUD(kd[, 2])
hr <- lapply(kd, function(x) getContours(x, c(50, 90)))
hr <- lapply(hr, function(x) geojson_json(x))

lflt <- leaflet() %>% addProviderTiles('Esri.WorldTopoMap',
                                       options = providerTileOptions(attribution = NA))
lflt %>% mapPolygons(hr) %>% mapPoints(df)
