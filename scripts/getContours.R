## TEST DATA
set.seed(123)
theta = runif(n <- 300, 0, 2 * pi)
r = sqrt(runif(n, 0.25^2, 0.5^2))
xy = data.frame(x = 0.5 + r * cos(theta), y = 0.5 + r * sin(theta), id = rep("a", 300))
x2 <- c(xy$x, xy$x + .75, xy$x + .75, xy$x)
y2 <- c(xy$y, xy$y, xy$y + .75, xy$y + .75)
xy <- data.frame(x = x2, y = y2)
xy$i <- rep('a', nrow(xy))
dat <- SpatialPointsDataFrame(coordinates(xy[, 1:2]), data = xy)
plot(dat, pch = 21, cex = .5)

## EXPECTED RESULTS (MOSTLY)
kud <- kernelUD(dat[, 3])
x <- getvolumeUD(kud)
x <- x[[1]]
image(x, col = rev(viridis::viridis(100)))
plot(getverticeshr(x, 95), add = T)
plot(getverticeshr(x, 90), add = T)

## FUNCTION
getContours <- function(ud, pct) {
  ids <- as.character(pct)
  x <- getvolumeUD(ud)
  xyma <- coordinates(x)
  xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
  z <- as.image.SpatialGridDataFrame(x[, 1])$z
  
  cl <- lapply(pct, function(x) { 
    contourLines(x = xyl$x, y = xyl$y, z, nlevels = 1, levels = x)
  })
  
  plys <- lapply(seq_along(cl), function(i) {
    Polygons(lapply(seq_along(cl[[i]]), function(j) {
      m <- cl[[i]][[j]]
      ply <- cbind(m$x, m$y)
      ply <- rbind(ply, ply[1, ])
      Polygon(ply)
    }), ID = ids[i])
  })
  
  plys <- lapply(plys, function(x) checkPolygonsHoles(x))
  splys <- SpatialPolygons(plys)
  dff <- data.frame(id = ids)
  row.names(dff) <- ids
  spdf <- SpatialPolygonsDataFrame(splys, dff)
  return(spdf)
}

## TEST FUNCTION WITH ONE ANIMAL
tst <- getContours(kud[[1]], c(25, 50, 75, 95))
tst$COLOR <- viridis::viridis(nrow(tst@data))
plot(tst, col = tst$COLOR, border = NA)

## TEST OVER MULTIPLE IDS
x2 <- c(xy$x, xy$x + .75, xy$x + .75, xy$x)
y2 <- c(xy$y, xy$y, xy$y + .75, xy$y + .75)
xy <- data.frame(x = x2, y = y2)
x2 <- c(xy$x, xy$x + .75, xy$x + .75, xy$x)
y2 <- c(xy$y, xy$y, xy$y + .75, xy$y + .75)
xy <- data.frame(x = x2, y = y2)
x2 <- c(xy$x, xy$x + .75, xy$x + .75, xy$x)
y2 <- c(xy$y, xy$y, xy$y + .75, xy$y + .75)
xy <- data.frame(x = x2, y = y2)
dat <- SpatialPointsDataFrame(coordinates(xy[, 1:2]), data = xy)
dat@data$i <- rep(c('a', 'b', 'c', 'd', 'e'), 15360)
kud <- kernelUD(dat[, 3])
image(kud)
tst_mult <- lapply(kud, function(x) getContours(x, c(25, 50, 75, 95)))
sapply(1:5, function(i) {
  plot(tst_mult[[i]], col = viridis::viridis(4))
})

## TEST CONVERT TO GEOJSON
gjl <- lapply(tst_mult, function(x) geojsonio::geojson_json(x))

## REDUCE TO ONE SPATIALPOLYGONSDATAFRAME
nm <- names(tst_mult)
for(i in seq_along(tst_mult)) {
  row.names(tst_mult[[i]]) <- paste(nm[i], row.names(tst_mult[[i]]))
  print(row.names(tst_mult[[i]]))
}
sdf_poly <- Reduce(rbind, tst_mult)
plot(sdf_poly, col = viridis::viridis(4))
writePolyShape(sdf_poly, 'testShapeExport')

## TEST WITH REAL DATA
df <- read_csv('data/CollarData 2.csv')
dat <- SpatialPointsDataFrame(coordinates(cbind(df[, 5:6])), as.data.frame(df),
                              proj4string = CRS('+init=epsg:4326'))
plot(dat, pch = 21, cex = .5)
head(dat[, 3])

kd <- kernelUD(dat[, 3])
udply <- lapply(kd, function(x) getContours(x, c(25, 50, 75)))
sapply(1:length(udply), function(i) {
  plot(udply[[i]], col = viridis::viridis(3))
})

correctIDs <- function(contour) {
  nm <- names(contour)
  for(i in seq_along(contour)) {
    row.names(contour[[i]]) <- paste(nm[i], row.names(contour[[i]]))
    print(row.names(contour[[i]]))
  }
  return(contour)
}
udply <- correctIDs(udply)
ex <- Reduce(rbind, udply)
plot(ex, col = viridis(3))
