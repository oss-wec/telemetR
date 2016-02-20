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

# I want to display multiple contours at once, how do I do this?
# using the data from above...merging spatial dataframes

ud90 <- get_ud(ndow.kd, 90)
ud70 <- get_ud(ndow.kd, 70)
ud50 <- get_ud(ndow.kd, 50)

conts <- geojson_list(get_ud(ndow.kd, 90)) + geojson_list(get_ud(ndow.kd, 70)) + 
  geojson_list(get_ud(ndow.kd, 50))

hr <- geojson_json(geojson_list(get_ud(ndow.kd, 90)) + 
                     geojson_list(get_ud(ndow.kd, 70)) + 
                     geojson_list(get_ud(ndow.kd, 50)))

leaflet() %>% addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircleMarkers(lat = ndow$lat_y, lng = ndow$long_x, 
                   stroke = F, radius = 3, fillOpacity = .75) %>% 
  addGeoJSON(hr, stroke = T, weight = 1, fillOpacity = .3, smoothFactor = 2)

# BROWNIAN BRIDGE
## ndow data
ndow <- fread("data/CollarData3494.csv")
ndow <- coord_conv(ndow)
traj <- to_ltraj(ndow)
bb <- estimate_bbmm(traj)

ud90 <- get_ud(bb, 90)
ud70 <- get_ud(bb, 70)
ud50 <- get_ud(bb, 50)

gj90 <- geojson_list(ud90)
gj70 <- geojson_list(ud70)
gj50 <- geojson_list(ud50)
hr <- geojson_json(gj90 + gj70 + gj50)

pal <- ggthemes::gdocs_pal()(10)

leaflet() %>% addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircleMarkers(lat = ndow$lat_y, lng = ndow$long_x, color = pal[1],
                   stroke = F, radius = 3, fillOpacity = .75, group = 'points') %>% 
  addGeoJSON(hr, color = pal[1], stroke = T, weight = 1, fillOpacity = .3, 
             smoothFactor = 1, group = 'hr') %>% 
  addLayersControl(
    overlayGroups = c('points', 'homerange'),
    options = layersControlOptions(collapsed = F))

#####################################################################
# EXPANDING FUNCTIONALITY TO INCLUDE MORE THAN ONE ANIMAL AT A TIME #
#####################################################################

dat <- fread('Collars.csv')
dat$timestamp <- fastPOSIXct(dat$timestamp)
df <- dat[ndowid %in% c(1135, 1136), ]

## estimating kernel density for two animals
coordinates(df) <- df[, .(long_x, lat_y)]  #converting to SPDF
kd <- kernelUD(df[, 2])
image(kd)
vd <- getvolumeUD(kd)
image(vd[[1]])
contour(vd[[1]], add = T)
image(vd[[2]])
contour(vd[[2]], add = T)

## encapsulating
df <- dat[ndowid == 1135, ]
coordinates(df) <- df[, .(long_x, lat_y)]
df@proj4string <- CRS('+proj=longlat')
#df <- spTransform(df, CRS('+proj=utm +zone=11'))
kd <- kernelUD(df[, 2])
image(kd)
x <- getverticeshr(kd, 99)
plot(x[[1]], add = T)

get_ud <- function(ud, percent) {
  ud1 <- getverticeshr(ud, percent[2])
}
x <- get_ud(kd, c(90, 99))

## function to return multiple UD contours for one UD
get_ud <- function(ud, percent) {
  ud_list <- list(length(percent))
  for (i in seq_along(percent)) {
    ctr <- getverticeshr(x = ud, percent = percent[i])
    ctr@proj4string <- CRS('+proj=longlat')
    ctr <- geojson_list(ctr)
    ud_list[[i]] <- ctr
  }
  geojson <- geojson_json(Reduce(`+`, ud_list))
  return(geojson)
}

x <- get_ud(kd, seq(55, 95, 10))
gj <- geojson_json(x[[1]] + x[[2]])

leaflet(df) %>% addTiles() %>% addGeoJSON(x, weight = 1) %>% addCircleMarkers(radius = 3, stroke = F)
