library(adehabitatHR)
library(leaflet)
library(sp)
library(data.table)
library(geojsonio)

data(puechabonsp)
names(puechabonsp)
image(puechabonsp$map, col = grey(c(1:10/10)))
plot(puechabonsp$relocs, add = T, col = as.data.frame(puechabonsp$relocs)[,1])

# MINIMUM CONVEX POLYGON
cp <- mcp(puechabonsp$relocs[,1], percent=95)
plot(cp)
plot(puechabonsp$relocs, add = T)

# with ndow data
ndow <- fread("data/CollarData3494.csv")
ndow <- coord_conv(ndow)
coords.ndow <- ndow[, .(x, y)]
sp.ndow <- SpatialPoints(coords.ndow, CRS('+proj=utm +zone=11'))

ncp <- mcp(sp.ndow, percent = 95)
hr.area <- ncp@data
plot(ncp)
plot(sp.ndow, add = T, col = 'firebrick4', pch = 1)

## ploting in leaflet
ncp <- spTransform(ncp, CRSobj = CRS('+proj=longlat'))
mincp <- geojson_json(ncp)
leaflet() %>% addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircleMarkers(lat = ndow$lat_y, lng = ndow$long_x, 
                   stroke = F, radius = 3, fillOpacity = .75) %>% 
  addGeoJSON(mincp)

## using increasing percent estimation
hrsize <- mcp.area(sp.ndow, percent = seq(50, 100, by = 5), unin = "m", unout = "km2")

# KERNEL UTILIZATION DISTRIBUTION

## setup, example data
data("puechabonsp")
kud <- kernelUD(puechabonsp$relocs[, 1], h='href')
kud
image(kud)    #show the estimated kernelUD
kud[[1]]@h    #show h estimate, and method

kudl <- kernelUD(puechabonsp$relocs[,1], h = "LSCV")
image(kudl)
plotLSCV(kudl)
homerange <- getverticeshr(kudl, 95)
plot(homerange, col=1:4)
vud <- getvolumeUD(kudl)
vud
image(kudl[[1]])
xyz <- as.image.SpatialGridDataFrame(kudl[[1]])
contour(xyz, add = T)

image(vud[[1]])
xyzv <- as.image.SpatialGridDataFrame(vud[[1]])
contour(xyzv, add = T)

# KERNEL DENSITY ESTIMATION
## NDOW data
ndow <- fread("data/CollarData3494.csv")
ndow <- coord_conv(ndow)
coords.ndow <- ndow[, .(x, y)]
sp.ndow <- SpatialPoints(coords.ndow, CRS('+proj=utm +zone=11'))

## kernel density estimation
ndow.kd <- kernelUD(sp.ndow, h = 'href')
image(ndow.kd)
nvud <- getvolumeUD(ndow.kd)
image(nvud)
xyz <- as.image.SpatialGridDataFrame(nvud)
contour(xyz, add = T)

ud90 <- get_ud(ndow.kd, 90)
ud90 <- geojson_json(ud90)

leaflet() %>% addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircleMarkers(lat = ndow$lat_y, lng = ndow$long_x, 
                   stroke = F, radius = 3, fillOpacity = .75) %>% 
  addGeoJSON(ud90)

