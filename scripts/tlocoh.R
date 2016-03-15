# tLoCoh
library(tlocoh)
library(sp)
library(rgdal)
library(fasttime)
library(data.table)

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


# using NDOW data and the LoCoH 
dat <- fread('Collars.csv')
df <- dat[ndowid == 3494, ]
df$timestamp <- fasttime::fastPOSIXct(df$timestamp)
df_na <- df[is.na(df$long_x | df$lat_y), ]
nrow(df_na)
df <- coord_conv(df)
df <- SpatialPointsDataFrame(df[, .(x, y)], data = df, 
                             proj4string = CRS('+proj=utm +zone=11'))

# creating and using the tlocoh data objects
df.lxy <- xyt.lxy(xy = df@coords, dt = df@data$timestamp, proj4string = df@proj4string,
                  id = 'dbhs3494')
plot(df.lxy)
df.lxy <- lxy.ptsh.add(df.lxy)
df.lxy <- lxy.nn.add(df.lxy, s = .03, k = 25)
summary(df.lxy)
df.lhs <- lxy.lhs(df.lxy, k = 3*2:8, s = .03)
summary(df.lhs, compact = T)
df.lhs <- lhs.iso.add(df.lhs)
plot(df.lhs, iso = T, record = T, ufipt = F, allpts = T)
plot(df.lhs, iso = T, k = 21, allpts = T, ufipt = F)

# visitation, behavioural stuff
df.k15 <- lhs.select(df.lhs, k = 21)
df.k15 <- lhs.ellipses.add(df.k15)
plot(df.k15, hulls = T, ellipses = T, allpts = T, nn = T, ptid = 'auto')
df.k15 <- lhs.visit.add(df.k15, ivg = 2 * 43200)
df.k15 <- lhs.iso.add(df.k15, sort.metric = 'ecc')
plot(df.k15, iso = T, iso.sort.metric = 'ecc')
hist(df.k15, metric = 'nsv')
plot(df.k15, hpp = T, hpp.classify = 'nsv', ivg = 2*43200, col.ramp = 'rainbow')
lhs.plot.scatter(df.k15)

# adaptive (a method)
df.lxy <- lxy.nn.add(df.lxy, s = .03, a = auto.a(nnn = 15, ptp = .98))
df.lhs.amixed <- lxy.lhs(df.lxy, s = .03, a = 4:15*1000, iso.add = T)
