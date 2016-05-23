library(sp)
library(adehabitatHR)

data("puechabonsp")
dat <- puechabonsp$relocs
kud <- kernelUD(dat[,1])

x <- getvolumeUD(kud)
x <- x[[1]]
image(x)
xyma <- coordinates(x)
xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
ud <- as.image.SpatialGridDataFrame(x[,1])$z

## CREATING ONE HOLELESS POLYGON 
re <- contourLines(x = xyl$x, y = xyl$y, ud,
                   nlevels = 1, levels = 75)

tmp <- cbind(re[[1]]$x, re[[1]]$y)
tmp <- rbind(tmp, tmp[1, ])

# create polygon for contour
ply <- Polygon(tmp)
# create polygons for contour
plys <- Polygons(list(ply), ID = '75%')
# create spatial polygons
splys <- SpatialPolygons(list(plys))
plot(splys)
# spatialpolygonsdataframe
spdf <- SpatialPolygonsDataFrame(splys, data.frame(id = '75%'), match.ID = F)

## CREATING 2 HOLELESS POLYGONS
re <- contourLines(x = xyl$x, y = xyl$y, ud,
                   nlevels = 2, levels = c(50, 75))
ply1 <- cbind(re[[1]]$x, re[[1]]$y)
ply1 <- rbind(ply1, ply1[1, ]) %>% Polygon
ply2 <- cbind(re[[2]]$x, re[[2]]$y)
ply2 <- rbind(ply2, ply2[1, ]) %>% Polygon

plys <- Polygons(list(ply1, ply2), 'Brock')
spdf <- SpatialPolygonsDataFrame(SpatialPolygons(list(plys)), data = data.frame(id = 'brock'), match.ID = F)
plot(spdf)

###############################################################################
# ENCAPSULATING

getConts <- function(ud, pct) {
  x <- getvolumeUD(ud)
  xyma <- coordinates(x)
  xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
  z <- as.image.SpatialGridDataFrame(x[, 1])$z
  
  re <- contourLines(x = xyl$x, y = xyl$y, z,
                     nlevels = length(pct), levels = pct)
  ply <- lapply(1:length(re), function(i) {
    y <- re[[i]]
    zz <- cbind(y$x, y$y)
    zz <- rbind(zz, zz[1, ])
    Polygon(zz)
  })
  
  plys <- SpatialPolygons(list(Polygons(ply, ID = 'ud')))
  dfid <- data.frame(id = 'ud'); rownames(dfid) <- 'ud'
  spdf <- SpatialPolygonsDataFrame(plys, dfid)
  return(spdf)
}

x <- getConts(kud[[1]], pct = c(25, 50, 75))
xx <- lapply(kud, function(x) getConts(x, c(25, 50, 75)))

x@polygons[[1]]@ID
x@polygons[[1]]@ID <- 'Brock'

names(xx)

for (i in seq_along(xx)) {
  xx[[i]]@polygons[[1]]@ID <- names(xx)[i]
  row.names(xx[[i]]@data) <- names(xx)[i]
}
print(xx[[1]]@polygons[[1]]@ID); row.names(xx[[1]]@data)
print(xx[[2]]@polygons[[1]]@ID)
print(xx[[3]]@polygons[[1]]@ID)
print(xx[[4]]@polygons[[1]]@ID)
xreduce <- Reduce(rbind, xx)
plot(xreduce)


## circular data

set.seed(123)
theta = runif(n <- 300, 0, 2 * pi)
r = sqrt(runif(n, 0.25^2, 0.5^2))
xy = data.frame(x = 0.5 + r * cos(theta), y = 0.5 + r * sin(theta), id = rep("a", 300))
plot(xy[, 1:2])
spdf <- SpatialPointsDataFrame(coordinates(xy[, 1:2]), xy)

kd <- kernelUD(spdf[, 3])
image(kd)
plot(getverticeshr(kd, 75), add = T)
xx <- getConts(kd, 75)

x <- getvolumeUD(kd)
xyma <- coordinates(x[[1]])
xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
z <- as.image.SpatialGridDataFrame(x[[1]][, 1])$z
re <- contourLines(x = xyl$x, y = xyl$y, z,
                   nlevels = 1, levels = 75)
