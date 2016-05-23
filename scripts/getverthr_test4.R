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
  return(ply)
}

x <- getConts(kud[[1]], pct = c(25, 50, 75))
xx <- Polygons(x, ID = 'Brock')
