## testing animals with multiple kdes

kud <- kernelUD(dat[, 1])
kd <- kud[[1]]
vd <- getvolumeUD(kd)

## polygons
xyma <- coordinates(vd)
xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
ud <- as.image.SpatialGridDataFrame(x[, 1])$z
re <- contourLines(x = xyl$x,
                   y = xyl$y,
                   ud, nlevels = 2, levels = c('50', '90'))

## spdf
spdf <- unlist(lapply(re, function(y) {
  tmp <- cbind(y$x, y$y)
  tmp <- rbind(tmp, tmp[1, ])
  SpatialPolygons(list(Polygons(list(Polygon(tmp)), 1)))
}))

plot(spdf[[2]])
plot(spdf[[1]], add = T)

getConts <- function(vd, pct) {
  xyma <- coordinates(vd)
  xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
  ud <- as.image.SpatialGridDataFrame(vd[, 1])$z
  re <- contourLines(x = xyl$x,
                     y = xyl$y,
                     ud, nlevels = length(pct), levels = pct)
  ## spdf
  spdf <- unlist(lapply(re, function(y) {
    tmp <- cbind(y$x, y$y)
    tmp <- rbind(tmp, tmp[1, ])
    SpatialPolygons(list(Polygons(list(Polygon(tmp)), 1)))
  }))
  return(spdf)
}

# testing get conts with one animal
x <- getConts(vd, c(25, 50, 95))
plot(x[[3]]); plot(x[[2]], add = T); plot(x[[1]], add = TRUE)

# testing get conts with multiple animals
vud <- getvolumeUD(kud)
x <- lapply(vud, function(x) getConts(x, c(25, 50, 95)))

y <- lapply(c(25, 50, 95), function(i) getverticeshr(kud, i))

###############################################################################
# Full Get Conts
getConts <- function(x, percent)
{
  ida <- 'homerange'
  x<-getvolumeUD(x)
  xyma <- coordinates(x)
  xyl <- list(x=unique(xyma[,1]), y=unique(xyma[,2]))
  ud <- as.image.SpatialGridDataFrame(x[,1])$z
  
  re <- contourLines(x = xyl$x, y = xyl$y, ud, 
                     nlevels = length(percent),
                     levels = percent)
  
  ## identify whether there is a hole in the list:
  areaa <- unlist(lapply(re, function(y) {
    ttmp <- cbind(y$x,y$y)
    ttmp <- rbind(ttmp, ttmp[1,])
    .arcpspdf(SpatialPolygons(list(Polygons(list(Polygon(ttmp)), 1))))
  }))
  
  spatpol <- do.call("cbind",lapply(1:length(re), function(i) {
    y <- re[[i]]
    zz <- cbind(y$x,y$y)
    zz <- rbind(zz,zz[1,])
    tmp <- SpatialPolygons(list(Polygons(list(Polygon(zz)),
                                         as.character(i))))
    return(!is.na(over(x, tmp)))
  }))
  spatpol <- as.data.frame(spatpol)
  hol <- unlist(lapply(1:ncol(spatpol), function(i) {
    all(apply(data.frame(spatpol[spatpol[,i],-i]), 1, any))
  }))
  areaa <- sum(areaa*sign(as.numeric(!hol)-0.5))
  
  ii <- SpatialPolygons(list(Polygons(lapply(1:length(re), function(i) {
    y <- re[[i]]
    zz <- cbind(y$x,y$y)
    zz <- rbind(zz,zz[1,])
    return((Polygon(zz, hole=hol[i])))
  }), ida)))
  
  dff <- data.frame(id=ida, area=areaa)
  row.names(dff) <- ida
  ii <- SpatialPolygonsDataFrame(ii, dff)
  # if (!is.na(pfs))
  #   proj4string(ii) <- CRS(pfs)
  # return(ii)
}

# get contours for 1 animal: return SpatialPolygonsDataFrame
x <- getConts(kud[[1]], c(25, 50, 75))
plot(x)
x@proj4string <- CRS('+proj=utm +zone=11')
x <- spTransform(x, CRS('+proj=longlat'))
gj <- geojsonio::geojson_json(x)
leaflet() %>% addTiles() %>% addGeoJSON(gj)

# get contours for n animals: return list SpatialPolygonsDataFrame
xx <- lapply(kud, function(x) getConts(x, c(25, 50, 75)))
plot(xx[[1]])
plot(xx[[2]])
plot(xx[[3]])
plot(xx[[4]])
gj <- geojsonio::geojson_json(xx[[1]])
