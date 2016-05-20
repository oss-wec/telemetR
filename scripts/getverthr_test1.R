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
