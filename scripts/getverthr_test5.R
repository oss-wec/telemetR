ply <- lapply(1:length(re), function(i) {
  y <- re[[i]]
  zz <- cbind(y$x, y$y)
  zz <- rbind(zz, zz[1, ])
  Polygon(zz)
})

ids <- c('50', '75')
plys <- lapply(seq_along(rel), function(i){
  Polygons(lapply(seq_along(rel[[i]]), function(j){
    y <- rel[[i]][[j]]
    zz <- cbind(y$x, y$y)
    zz <- rbind(zz, zz[1, ])
    Polygon(zz)
  }), ids[i])
})

plys <- lapply(plys, function(x) checkPolygonsHoles(x))
splys <- SpatialPolygons(plys)
dff <- data.frame(id = ids)
row.names(dff) <- ids
spdf <- SpatialPolygonsDataFrame(splys, dff)
spdf$COLOR <- c('#006837',  '#31A354')
plot(spdf, col = spdf$COLOR, border = NA)

# new test data set
set.seed(123)
theta = runif(n <- 300, 0, 2 * pi)
r = sqrt(runif(n, 0.25^2, 0.5^2))
xy = data.frame(x = 0.5 + r * cos(theta), y = 0.5 + r * sin(theta), id = rep("a", 300))
plot(xy[, 1:2])
x2 <- c(xy$x, xy$x + .75, xy$x + .75, xy$x)
y2 <- c(xy$y, xy$y, xy$y + .75, xy$y + .75)
plot(x2, y2)
xy <- data.frame(x = x2, y = y2)
xy$i <- rep('a', nrow(xy))
dat <- SpatialPointsDataFrame(coordinates(xy[, 1:2]), data = xy)
plot(dat, pch = 21, cex = .5)

# dataset test
kud <- kernelUD(dat[, 3])
x <- getvolumeUD(kud)
x <- x[[1]]
image(x, col = rev(viridis::viridis(100)))
plot(getverticeshr(x, 95), add = T)
plot(getverticeshr(x, 90), add = T)

# setup for contours
xyma <- coordinates(x)
xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
ud <- as.image.SpatialGridDataFrame(x[,1])$z
rel <- lapply(c(25, 50, 75, 95), function(x) {
  contourLines(x = xyl$x, y = xyl$y, ud,
               nlevels = 1, levels = x)
})

# Polygons
ids <- c('25', '50', '75', '95')
plys <- lapply(seq_along(rel), function(i){
  Polygons(lapply(seq_along(rel[[i]]), function(j){
    y <- rel[[i]][[j]]
    zz <- cbind(y$x, y$y)
    zz <- rbind(zz, zz[1, ])
    Polygon(zz)
  }), ids[i])
})

plys <- lapply(plys, function(x) checkPolygonsHoles(x))
splys <- SpatialPolygons(plys)
dff <- data.frame(id = ids)
row.names(dff) <- ids
spdf <- SpatialPolygonsDataFrame(splys, dff)
spdf$COLOR <- viridis::viridis(4)
plot(spdf, col = spdf$COLOR, border = NA)
image(x, col = rev(viridis::viridis(64)))
viridis::viridis(4)


