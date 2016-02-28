# tLoCoh
library(tlocoh)
library(sp)
library(rgdal)

data('toni')
toni.sp.latlong <- SpatialPoints(toni[ , c("long","lat")],
                                 proj4string=CRS("+proj=longlat +ellps=WGS84"))
toni.sp.utm <- spTransform(toni.sp.latlong, 
                           CRS('+proj=utm +south +zone=36 +ellps=WGS84'))
toni.mat.utm <- coordinates(toni.sp.utm)
colnames(toni.mat.utm) <- c('x', 'y')
toni$timestamp.utc <- as.POSIXct(toni$timestamp.utc, tz = 'UTC')
local.tz <- 'Africa/Johannesburg'
toni$timestamp.gmt <- as.POSIXct(format(toni$timestamp.utc, tz = local.tz),
                                 tz = local.tz)
head(toni$timestamp.gmt)

toni.lxy <- xyt.lxy(xy = toni.mat.utm, dt = toni$timestamp.gmt, id = 'toni',
                    proj4string = toni.sp.utm@proj4string)
## summary of the tlocoh object
summary(toni.lxy)
## plot the xy coordinates of the tlocoh object...
## I don't know what the different colors represent
plot(toni.lxy)
hist(toni.lxy)
lxy.plot.freq(toni.lxy, deltat.by.date = T)
lxy.plot.sfinder(toni.lxy)

# estimating convex hulls
toni.lxy <- lxy.thin.bursts(toni.lxy, thresh = .2)
toni.lxy <- lxy.nn.add(toni.lxy, s = .003, k = 25)
summary(toni.lxy)

toni.lhs <- lxy.lhs(toni.lxy, k = 3*2:8, s = .003)
toni.lhs <- lhs.iso.add(toni.lhs)
plot(toni.lhs, iso = T, record = T, ufipt = F)
# choose k=15, replot better
plot(toni.lhs, iso=T, k=15, allpts=T, cex.allpts=0.3, col.allpts="gray30", ufipt=F)
