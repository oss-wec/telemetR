library(leaflet)
# Practice brownian bridge from adehabitatHR vignette

data("puechcirc")
x <- puechcirc[1]
x

# visualize tracks using plot modified by adehabitat
plot(x)

# estimating sig1
lik <- liker(x, sig2 = 58, rangesig1 = c(10, 100))
lik2 <- liker(x, sig2 = 58, rangesig1 = c(1, 10))
lik2

# estimate kernel brownian bridge home range 
tata <- kernelbb(x, sig1 = 6.23, sig2 = 58, grid = 50)
tata

# visualize results
image(tata)
plot(getverticeshr(tata, 95), add = TRUE, lwd=2)

## testing on 3464
# entering and munging data
dat <- read.csv("CollarData3494.csv")
dat$timestamp <- as.POSIXct(dat$timestamp)
dat <- dat[complete.cases(dat[, c("long_x", "lat_y")]), ]
geocoord <- sp::SpatialPoints(cbind(as.numeric(dat$long_x),
                                    as.numeric(dat$lat_y)),
                              proj4string = sp::CRS("+proj=longlat"))
utmcoord <- as.data.frame(sp::spTransform(geocoord, sp::CRS("+proj=utm +zone=11")))
colnames(utmcoord) <- c("Easting", "Northing")
dat <- cbind(dat, utmcoord)
rm(utmcoord); rm(geocoord)

# convert to ltraj
traj <- as.ltraj(dat[, 12:13], date = dat$timestamp, id = dat$ndowid)
plot(traj)

# estimate sig1
lik <- liker(traj, sig2 = 40, rangesig1 = c(1, 10))

# estimate kernel brownian bridge home range
bb <- kernelbb(traj, sig1 = 5, sig2 = 40, grid = 150)
bb
image(bb)
plot(getverticeshr(bb, 95), add = T, lwd = 2)
plot(getverticeshr(bb, 75), add = T, lwd = 2)

ud95 <- getverticeshr(bb, 95)
ud95@proj4string <- CRS("+proj=utm +zone=11 +datum=WGS84")
ud95.latlong <- spTransform(ud95, CRS("+proj=longlat"))

leaflet(ud95.latlong) %>% addTiles() %>% addPolygons()

leaflet(ud95.latlong) %>% addProviderTiles(("Esri.WorldTopoMap")) %>% 
  addPolygons(stroke = F, color = "midnightblue", fillOpacity = .4) %>% 
  addCircleMarkers(lng = dat$long_x, lat = dat$lat_y, color = "red", stroke = F, radius = 3)


traj
plotltr(traj)
hist(traj[[1]]$dist)
